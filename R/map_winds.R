#' Map wind exposure at the county level
#'
#' Inputs a dataframe with modeled winds for each eastern U.S. county and
#' maps these modeled winds.
#'
#' @param grid_winds A dataframe that is the output of running
#'    \code{\link{get_grid_winds}} using eastern U.S. county centers as the
#'    grid point locations for modeling the winds.
#' @param value A character string giving the value to plot. Possible options
#'    are \code{"vmax_gust"} (maximum gust wind speeds) and
#'    \code{"vmax_sust"} (maximum sustained wind speeds).
#' @param break_point An numeric value giving the value of the \code{value} parameter
#'    (e.g.,maximum gust wind speeds or maximum sustained wind speeds)
#'    at which to break for a binary map showing exposure versus no exposure.
#'    The default for this parameter is \code{NULL}, which returns a map with
#'    continuous wind speed values. If the \code{break_point} argument is set
#'    to a numeric value, the function will return a map where counties are given
#'    binary classifications of "exposed" or "not exposed" based on whether
#'    modeled wind speed for the county is above or below this break point.
#' @param wind_metric A character vector with the wind metric to use for the map.
#'    Possible values are \code{"knots"} and \code{"mps"} (m / s, the default).
#'
#' @return This function returns a map of the \code{ggplot} class, plotting
#'    exposure to hurricane winds by county for the eastern half of the United
#'    States.
#'
#' @examples
#'
#' data("katrina_tracks")
#' data("county_points")
#' grid_winds_katrina <- get_grid_winds(hurr_track = katrina_tracks,
#'                                      grid_df = county_points)
#' map_wind(grid_winds_katrina)
#' map_wind(grid_winds_katrina, wind_metric = "knots")
#' map_wind(grid_winds_katrina, value = "vmax_gust")
#' map_wind(grid_winds_katrina, break_point = 20)
#'
#'
#' @importFrom dplyr %>%
#'
#' @export
map_wind <- function(grid_winds, value = "vmax_sust", break_point = NULL,
                     wind_metric = "mps"){

  grid_winds[ , "value"] <- grid_winds[ , value]
  if(wind_metric != "mps"){
    grid_winds[ , "value"] <- weathermetrics::convert_wind_speed(grid_winds$value,
                                                           old_metric = "mps",
                                                           new_metric = wind_metric)
  }

  if(!is.null(break_point)){
    cut_values <- cut(grid_winds$value,
                      breaks = c(0, break_point, max(grid_winds$value)),
                      include.lowest = TRUE)
    grid_winds$value <- cut_values
    num_colors <- 2
  } else {
    if(wind_metric == "mps"){
      breaks <- c(0, seq(15, 45, 5))
      exposure_palette <- c("#FEE5D9", "#FCBBA1", "#FC9272", "#FB6A4A",
                            "#DE2D26", "#A50F15")
    } else if(wind_metric == "knots"){
      breaks <- c(0, 34, 50, 64, 100)
      exposure_palette <- c("#FEE0D2", "#FC9272", "#DE2D26")
    } else if (wind_metric == "mph") {
      breaks <- c(0, 40, 60, 75, 115)
      exposure_palette <- c("#FEE0D2", "#FC9272", "#DE2D26")
    }
    palette_name <- "Reds"

    # Adjust for right outliers
    if(max(grid_winds$value) > max(breaks)){
      breaks <- c(breaks, max(grid_winds$value))
    }

    exposure_palette <- c("#ffffff", exposure_palette, "#1a1a1a")

    grid_winds <- grid_winds %>%
      dplyr::mutate(value = cut(.data$value, breaks = breaks,
                                   include.lowest = TRUE))
  }

  map_data <- dplyr::mutate(grid_winds,
                             fips = as.numeric(.data$gridid)) %>%
    dplyr::select(.data$fips, .data$value)

  county.fips <- maps::county.fips %>%
    dplyr::mutate(polyname = as.character(.data$polyname)) %>%
    dplyr::mutate(polyname = stringr::str_replace(.data$polyname, ":.+", ""))
  us_counties <- ggplot2::map_data("county") %>%
    dplyr::filter(!(.data$region %in% c("arizona", "california", "colorado", "idaho",
                           "montana", "nebraska", "nevada", "new mexico",
                           "north dakota", "oregon", "south dakota",
                           "utah", "washington", "wyoming", "minnesota"))) %>%
    tidyr::unite_(col = "polyname", from = c("region", "subregion"),
                  sep = ",") %>%
    dplyr::left_join(county.fips, by = "polyname") %>%
    dplyr::left_join(map_data, by = "fips")

  out <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = us_counties,
                 ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                               fill = ~ value),
                 color = "lightgray", size = 0.2) +
    ggplot2::borders("state", regions = c("virginia", "north carolina", "south carolina",
                                 "georgia", "florida", "alabama", "kentucky",
                                 "tennessee", "maryland", "west virginia",
                                 "district of columbia", "pennsylvania",
                                 "new jersey", "delaware", "mississippi",
                                 "louisiana", "texas", "oklahoma", "arkansas",
                                 "new york", "connecticut", "rhode island",
                                 "massachusetts", "new hampshire", "vermont",
                                 "maine", "kansas", "missouri", "iowa", "michigan",
                                 "illinois", "ohio", "wisconsin", "indiana"),
            colour = "black", fill = NA, size = 0.2, alpha = 0.5) +
    ggplot2::theme_void() +
    ggplot2::coord_map()

  exposure_legend <- paste0("Wind speed (",
                            ifelse(wind_metric == "mps", "m / s",
                                   wind_metric),
                            ")")

  if(!is.null(break_point)){
    out <- out + ggplot2::scale_fill_manual(name = exposure_legend,
                                            values = c("white", "#DE2D26"),
                                            labels = levels(cut_values))
  } else{
    out <- out + ggplot2::scale_fill_manual(name = exposure_legend,
                                            values = exposure_palette)
  }

  return(out)
}

#' Plot Atlantic basin hurricane tracks
#'
#' Plot the tracks of a selected tropical storm to a map of modeled wind
#' speed.
#'
#' @param storm_tracks A data frame with best tracks data for the storm
#'    track you would like to add. See the example \code{\link{floyd_tracks}}
#'    data for an example of the required format. This dataset must
#'    include columns for \code{date} (date-time of the track observation),
#'    \code{latitude}, and \code{longitude}.
#' @param plot_object NULL or the name of a ggplot object to use as the
#'    underlying plot object (e.g., the output from a call to
#'    \code{\link{map_wind}})
#' @param plot_points TRUE / FALSE indicator of whether to include points,
#'    as well as lines, when plotting the hurricane tracks.
#' @param alpha Numerical value designating the amount of transparency to
#'    use for plotting tracks.
#' @param color Character string giving the color to use to plot the tracks.
#'
#' @return A ggplot object that includes a line with the track of a given
#'    tropical storm. This object can be printed directly or added
#'    on to with other ggplot commands.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' data("county_points")
#' data("floyd_tracks")
#' grid_winds_floyd <- get_grid_winds(hurr_track = floyd_tracks,
#'                                    grid_df = county_points)
#' floyd_map <- map_wind(grid_winds_floyd, value = "vmax_sust",
#'                       wind_metric = "knots") +
#'              ggtitle("Maximum sustained wind speeds")
#' add_storm_track(floyd_tracks, plot_object = floyd_map)
#'
#'
#' @importFrom dplyr %>%
#' @export
add_storm_track <- function(storm_tracks, plot_object,
                       plot_points = FALSE, alpha = 1,
                       color = "firebrick"){

  map_data <- plot_object$data
  tracks <- storm_tracks %>%
    dplyr::select(.data$latitude, .data$longitude, .data$date) %>%
    dplyr::filter(.data$longitude > -106.65 &
                     .data$longitude < -67.01 &
                     .data$latitude > 25.13 &
                     .data$latitude < 47.48) %>%
    dplyr::mutate(date = lubridate::ymd_hm(.data$date))

  if(nrow(tracks) >= 3){
    full_tracks <- interp_track(tracks)
  } else{
    full_tracks <- tracks
  }

  out <- plot_object +
    ggplot2::geom_path(data = full_tracks,
                       ggplot2::aes_(x = ~ longitude,
                                     y = ~ latitude,
                                     group = NULL),
                       alpha = alpha,
                       color = color)

  if(plot_points){
    out <- out + ggplot2::geom_point(data = tracks,
                                     ggplot2::aes_(x = ~ longitude,
                                                   y = ~ latitude,
                                                   group = NULL),
                                     alpha = alpha)
  }
  return(out)
}

#' Interpolate a storm track
#'
#' This function takes a wider-spaced storm track (e.g., every 6 hours) and
#' interpolates to a finer interval (e.g., every 15 minutes). To do this, it
#' fits GLMs of latitude and longitude regressed on natural cubic splines of
#' date-time, and then predicts these splines to new intervals. These
#' splines use degrees of freedom equal to the number of original observations
#' divided by two.
#'
#' @param track A dataframe with hurricane track data for a single storm. See
#'    the \code{\link{floyd_tracks}} dataset that comes with the package for
#'    an example of the required format for this dataframe.
#' @param tint A numeric vector giving the time interval to impute to, in units
#'    of hours (e.g., 0.25, the default, interpolates to 15 minute-intervals).
#'
#' @return A dataframe with hurricane track data for a single storm,
#'    interpolated to the interval specified by \code{tint}.
interp_track <- function(track, tint = 0.25){
  interp_df <- floor(nrow(track) / 2)
  interp_date <- seq(from = min(track$date), to = max(track$date),
                     by = 3600 * tint)
  interp_date <- data.frame(date = interp_date)

  lat_spline <- stats::glm(latitude ~ splines::ns(date, df = interp_df),
                           data = track)
  interp_lat <- stats::predict.glm(lat_spline,
                                   newdata = as.data.frame(interp_date))
  lon_spline <- stats::glm(longitude ~ splines::ns(date, df = interp_df),
                           data = track)
  interp_lon <- stats::predict.glm(lon_spline, newdata = interp_date)

  full_track <- data.frame(date = interp_date,
                           latitude = interp_lat,
                           longitude = interp_lon)
  return(full_track)
}
