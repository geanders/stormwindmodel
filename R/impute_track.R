#' Impute hurricane tracks to finer time scale
#'
#' This function takes data on a hurricane's track and imputes to fill in
#' values at a finer time resolution. For example, if the hurricane tracks
#' are recorded at 6-hour intervals, this could be used to impute locations
#' and windspeeds at 15-minute intervals.
#'
#' @details The function does a simple linear interpolation, both for location
#' and for wind speed, between the measured observations in the input
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
#'    changed ("tclat" for latitude, "tclon" for longitude, and "vmax"
#'    for wind speed.)
#'
#' @note This function imputes between each original data point, and it starts
#'    by determing the difference in time between each pair of data points.
#'    Because of this, the function can handle data that includes a point
#'    that is not at one of the four daily synoptic times (00:00, 06:00, 12:00,
#'    and 18:00). Typically, the only time hurricane observations are given
#'    outside of synoptic times for best tracks data is at landfall.
#'
#' @note After imputing the tracks, longitude is expressed as a positive number.
#'    This is so the output will work correctly in later functions to fit the
#'    wind model. However, be aware that you should use the negative value of
#'    longitude for mapping tracks from the output from this function.
#'
#' @examples
#' data("floyd_tracks")
#' full_track <- create_full_track(hurr_track = floyd_tracks)
#'
#' # Interpolate to every half hour (instead of default 15 minutes)
#' full_track <- create_full_track(hurr_track = floyd_tracks, tint = 0.5)
#'
#' @importFrom dplyr %>%
#'
#' @export
create_full_track <- function(hurr_track = floyd_tracks, tint = 0.25){
  hurr_track <- dplyr::select(hurr_track, date, latitude, longitude, wind) %>%
    dplyr::rename(vmax = wind,
                  tclat = latitude,
                  tclon = longitude) %>%
    dplyr::mutate(date = lubridate::ymd_hm(date),
                  tclat = abs(as.numeric(tclat)),
                  tclon = as.numeric(tclon),
                  tclon = ifelse(tclon > -180, tclon, tclon + 360),
                  tclon = -1 * tclon,
                  vmax = 0.51444 * as.numeric(vmax)) # Convert from ks to m / s

  interp_df <- floor(nrow(hurr_track) / 2)
  interp_date <- seq(from = min(hurr_track$date),
                     to = max(hurr_track$date),
                     by = tint * 3600)
  interp_date <- data.frame(date = interp_date)

  tclat_spline <- stats::glm(tclat ~ splines::ns(date, df = interp_df),
                             data = hurr_track)
  interp_tclat <- stats::predict.glm(tclat_spline,
                                     newdata = as.data.frame(interp_date))
  tclon_spline <- stats::glm(tclon ~ splines::ns(date, df = interp_df),
                             data = hurr_track)
  interp_tclon <- stats::predict.glm(tclon_spline, newdata = interp_date)
  vmax_spline <- stats::glm(vmax ~ splines::ns(date, df = interp_df),
                            data = hurr_track)
  interp_vmax <- stats::predict.glm(vmax_spline, newdata = interp_date)

  full_track <- data.frame(date = interp_date,
                           tclat = interp_tclat,
                           tclon = interp_tclon,
                           vmax = interp_vmax)
  return(full_track)
}
