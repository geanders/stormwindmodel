#' Impute hurricane tracks to finer time scale
#'
#' This function takes data on a hurricane's track and imputes to fill in
#' values at a finer time resolution. For example, if the hurricane tracks
#' are recorded at 6-hour intervals, this could be used to impute locations
#' and windspeeds at 15-minute intervals.
#'
#' @note The function does a simple linear interpolation, both for location
#' and for wind speed, between the measured observations in the inputted
#' hurricane track data.
#'
#' @param hurr_track Dataframe with columns the hurricane track for a single
#'    storm. This must include columns for date-time (year, month, day, hour,
#'    minute), latitude, longitude, and wind speed (in knots)
#' @param tint Interval (in hours) for the estimates
#'
#' @return The function returns an extended version of the track data, with
#'    latitude, longitude, and wind speed linearly interpolated between
#'    observed values. Also, wind speed is converted in this function to m / s
#'    and the absolute value of the latitude is taken (necessary for further
#'    wind speed calculations).
#'
#' @examples
#' data("hurr_tracks", package = "hurricaneexposure")
#' example_track <- subset(hurr_tracks, storm_id == "Floyd-1999")
#' full_track <- create_full_track(hurr_track = example_track,
#'                                 tint = 0.25)
#'
#' @importFrom dplyr %>%
#'
#' @export
create_full_track <- function(hurr_track = subset(hurr_tracks,
                                                  storm_id == "Floyd-1999"),
                              tint = 0.25){
  hurr_track <- dplyr::select(hurr_track, date, latitude, longitude, wind) %>%
    dplyr::rename(lonr = longitude,
                  phir = latitude,
                  sustained_Vmax = wind) %>%
    dplyr::mutate(date = lubridate::ymd_hm(date),
                  phir = abs(as.numeric(phir)),
                  lonr = -1 * as.numeric(lonr),
                  sustained_Vmax = 0.51444 * as.numeric(sustained_Vmax), # Convert from ks to m / s
                  dhr = as.numeric(difftime(dplyr::lead(date),
                                            date, units = "hours")),
                  interval = floor(dhr / tint),
                  delphi = (dplyr::lead(phir) - phir) / interval,
                  dellon = (dplyr::lead(lonr) - lonr) / interval,
                  delvmax = (dplyr::lead(sustained_Vmax) - sustained_Vmax) / interval)

  for(i in 1:(nrow(hurr_track) - 1)){
    start_obs <- hurr_track[i, ]
    for(k in 1:hurr_track[i, "interval"]){
      new_date <- start_obs$date + (k - 1) * lubridate::dhours(tint)
      new_phi <- start_obs$phir + (k - 1) * start_obs$delphi
      new_lon <- start_obs$lonr + (k - 1) * start_obs$dellon
      new_vmax <- start_obs$sustained_Vmax + (k - 1) * start_obs$delvmax
      if(i == 1 && k == 1){
        track_date <- new_date
        phi <- new_phi
        lon <- new_lon
        sustained_Vmax <- new_vmax
      } else {
        track_date <- c(track_date, new_date)
        phi <- c(phi, new_phi)
        lon <- c(lon, new_lon)
        sustained_Vmax <- c(sustained_Vmax, new_vmax)
      }
    }
  }
  full_track <- data.frame(date = track_date,
                           phi = phi,
                           lon = lon,
                           sustained_Vmax = sustained_Vmax)
  return(full_track)
}
