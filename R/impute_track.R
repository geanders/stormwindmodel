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
#'    minute; e.g., "198808051800" for August 5, 1988, 18:00 UTC),
#'    latitude, longitude, and wind speed (in knots). The column
#'    names for each of these must be "date", "latitude", "longitude",
#'    and "wind".
#' @param tint Interval (in hours) for the estimates. Default is 0.25 (i.e., 15
#'    minutes)
#'
#' @return The function returns an extended version of the track data, with
#'    latitude, longitude, and wind speed linearly interpolated between
#'    observed values. Also, wind speed is converted in this function to m / s
#'    and the absolute value of the latitude is taken (necessary for further
#'    wind speed calculations). Finally, the names of some columns are
#'    changed ("phi" for latitude, "lon" for longitude, and "sustained_Vmax"
#'    for wind speed.)
#'
#' @note This function imputes between each original data point, and it starts
#'    by determing the difference in time between each pair of data points.
#'    Because of this, the function can handle data that includes a point
#'    that is not at one of the four daily synoptic times (00:00, 06:00, 12:00,
#'    and 18:00). Typically, the only time hurricane observations are given
#'    outside of synoptic times for best tracks data is at landfall.
#'
#' @examples
#' data("hurr_tracks", package = "hurricaneexposuredata")
#' example_track <- subset(hurr_tracks, storm_id == "Floyd-1999")
#' full_track <- create_full_track(hurr_track = example_track,
#'                                 tint = 0.25)
#'
#' @importFrom dplyr %>%
#'
#' @export
create_full_track <- function(hurr_track = subset(hurr_tracks, storm_id == "Floyd-1999"),
                              tint = 0.25){
  hurr_track <- dplyr::select(hurr_track, date, latitude, longitude, wind) %>%
    dplyr::mutate(date = lubridate::ymd_hm(date),
                  latitude = abs(as.numeric(latitude)),
                  longitude = as.numeric(longitude),
                  longitude = ifelse(longitude > -180, longitude, longitude + 360),
                  longitude = -1 * longitude,
                  wind = 0.51444 * as.numeric(wind)) # Convert from ks to m / s

  interp_df <- floor(nrow(hurr_track) / 2)
  interp_date <- seq(from = min(hurr_track$date),
                     to = max(hurr_track$date),
                     by = tint * 3600)
  interp_date <- data.frame(date = interp_date)

  lat_spline <- stats::glm(latitude ~ splines::ns(date, df = interp_df),
                           data = hurr_track)
  interp_lat <- stats::predict.glm(lat_spline,
                                   newdata = as.data.frame(interp_date))
  lon_spline <- stats::glm(longitude ~ splines::ns(date, df = interp_df),
                           data = hurr_track)
  interp_lon <- stats::predict.glm(lon_spline, newdata = interp_date)
  wind_spline <- stats::glm(wind ~ splines::ns(date, df = interp_df),
                            data = hurr_track)
  interp_wind <- stats::predict.glm(wind_spline, newdata = interp_date)

  full_track <- data.frame(date = interp_date,
                           phi = interp_lat,
                           lon = interp_lon,
                           sustained_Vmax = interp_wind)
  return(full_track)
}
