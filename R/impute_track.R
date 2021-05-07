#' Impute hurricane tracks to finer time scale
#'
#' Inputs data on a hurricane's track and imputes to a finer time resolution.
#' For example, if the hurricane tracks are recorded at 6-hour intervals, this
#' could be used to impute locations and windspeeds at 15-minute intervals.
#' This function also does some reformatting necessary for later functions in
#' the \code{stormwindmodel} package.
#'
#' @details The function uses natural cubic splines for interpolation for location
#' and linear splines for interpolation for wind speed. The base R functions
#' \code{spline} and \code{approx} are used for these interpolations.
#'
#' @param hurr_track Dataframe with hurricane track data for a single
#'    storm. The dataframe must include columns for date-time (year, month, day,
#'    hour, minute; e.g., "198808051800" for August 5, 1988, 18:00 UTC),
#'    latitude, longitude, and wind speed (in knots). The column
#'    names for each of these must be \code{date}, \code{latitude},
#'    \code{longitude}, and \code{wind}. See the example \code{\link{floyd_tracks}}
#'    dataset for an example of the required format.
#' @param tint Interval (in hours) to which to interpolate the tracks. The
#'    default is 0.25 (i.e., 15 minutes).
#'
#' @return A version of the storm's track data with
#'    latitude, longitude, and wind speed interpolated between
#'    observed values. Also, wind speed is converted in this function to m / s
#'    and the absolute value of the latitude is taken (necessary for further
#'    wind speed calculations). Finally, the names of some columns are
#'    changed (\code{tclat} for latitude, \code{tclon} for longitude, and
#'    \code{vmax} for wind speed.)
#'
#' @note This function imputes between each original data point, and it starts
#'    by determining the difference in time between each pair of data points.
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
#' @importFrom rlang .data
#'
#' @export
create_full_track <- function(hurr_track = stormwindmodel::floyd_tracks,
                              tint = 0.25){
  hurr_track <- dplyr::select(hurr_track, .data$date, .data$latitude,
                               .data$longitude, .data$wind) %>%
    dplyr::rename(vmax = .data$wind,
                  tclat = .data$latitude,
                  tclon = .data$longitude) %>%
    dplyr::mutate(date = lubridate::ymd_hm(.data$date),
                  tclat = as.numeric(.data$tclat),
                  tclon = as.numeric(.data$tclon),
                  vmax = weathermetrics::convert_wind_speed(.data$vmax, "knots",
                                                            "mps", round = 3),
                  track_time_simple = difftime(.data$date, dplyr::first(.data$date),
                                               units = "hour"),
                  track_time_simple = as.numeric(.data$track_time_simple))

  # Identify cases where a storm goes over the international date line, and
  # longitudes change from about 180 to about -180, or vice versa. Correct this
  # before interpolating (then later we get everything back within the 180 to -180
  # range).
  if(diff(range(hurr_track$tclon)) > 300){
    hurr_track <- hurr_track %>%
      dplyr::mutate(tclon = ifelse(tclon > 0, tclon, tclon + 360))
  }

  full_track <- hurr_track %>%
    tidyr::nest(data = tidyr::everything()) %>%
    # Create times to interpolate to
    dplyr::mutate(interp_time = purrr::map(.data$data,
                                           .f = ~ seq(from = dplyr::first(.x$track_time_simple),
                                                      to = dplyr::last(.x$track_time_simple),
                                                      by = tint))) %>%
    # Interpolate latitude and longitude using natural cubic splines
    dplyr::mutate(tclat = purrr::map2(.data$data, .data$interp_time,
                                      .f = ~ spline(x = .x$track_time_simple,
                                                    y = .x$tclat,
                                                    xout = .y,
                                                    method = "natural")$y)) %>%
    dplyr::mutate(tclon = purrr::map2(.data$data, .data$interp_time,
                                      .f = ~ spline(x = .x$track_time_simple,
                                                    y = .x$tclon,
                                                    xout = .y,
                                                    method = "natural")$y)) %>%
    # Interpolate max wind using linear interpolation
    dplyr::mutate(vmax = purrr::map2(.data$data, .data$interp_time,
                                     .f = ~ approx(x = .x$track_time_simple,
                                                   y = .x$vmax,
                                                   xout = .y)$y)) %>%
    dplyr::mutate(date = purrr::map2(.data$data, .data$interp_time,
                                    .f = ~ dplyr::first(.x$date) +
                                      lubridate::seconds(3600 * .y))) %>%
    dplyr::select(.data$date, .data$tclat, .data$tclon, .data$vmax) %>%
    tidyr::unnest(.data$date:.data$vmax) %>%
    # Make sure that longitude is between -180 and 180
    dplyr::mutate(tclon = ((tclon + 180) %% 360) - 180)

  return(full_track)
}
