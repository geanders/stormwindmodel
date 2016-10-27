#' Map wind exposure at the county level
#'
#' @param grid_winds A dataframe that is the output of
#'    \code{\link{get_grid_winds}}.
#' @param value A character string giving the value to plot. Possible options
#'    are "max_gust" (maximum gust wind speeds) and "max_sust" (maximum
#'    sustained wind speeds).
#' @param break_point An numeric value giving the value of the "value"
#'    parameter to break at for a binary map showing exposure versus no exposure.
#' @param wind_metric A character vector with the wind metric to use for the map.
#'    Possible values include \code{"knots"}, \code{"mph"}, \code{"ftps"} (ft / s),
#'    \code{"kmph"} (km / hr), and \code{"mps"} (m / s, the default).
#'
#' @return This function returns a map of the "ggplot" class, plotting
#'    exposure to hurricane winds by county for the eastern half of the United
#'    States.
#'
#' @examples
#' \donttest{
#' data("katrina_tracks")
#' data("county_point")
#' grid_winds_katrina <- get_grid_winds(hurr_track = katrina_tracks,
#'                                      grid_df = county_points)
#' map_wind(grid_winds_katrina)
#' map_wind(grid_winds_katrina, break_point = 20)
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
map_wind <- function(grid_winds, value = "vmax_sust", break_point = NULL,
                     wind_metric = "mps"){
  # if(!(value %in% c("vmax_gust", "vmax_sust"))){
  #   stop("`value` must be either `vmax_gust` or `vsust_gust`.")
  # }

  grid_winds$value <- grid_winds[ , value]
  if(wind_metric != "mps"){
    grid_winds$value <- weathermetrics::convert_wind_speed(grid_winds$value,
                                                           old_metric = "mps",
                                                           new_metric = wind_metric)
  }

  if(!is.null(break_point)){
    cut_values <- cut(grid_winds$value,
                      breaks = c(0, break_point, max(grid_winds$value)),
                      include.lowest = TRUE)
    grid_winds$value <- as.numeric(cut_values)
    num_colors <- 2
  } else {
    if(wind_metric == "mps"){
      breaks <- c(0, seq(15, 45, 5))
      exposure_palette <- c("#FEE5D9", "#FCBBA1", "#FC9272", "#FB6A4A",
                            "#DE2D26", "#A50F15")
    } else if(wind_metric == "knots"){
      breaks <- c(0, 34, 50, 64, 100)
      exposure_palette <- c("#FEE0D2", "#FC9272", "#DE2D26")
    }
    palette_name <- "Reds"

    # Adjust for right outliers
    if(max(grid_winds$value) > max(breaks)){
      breaks <- c(breaks, max(grid_winds$value))
    }

    exposure_palette <- c("#f7f7f7", exposure_palette, "#1a1a1a")

    grid_winds <- grid_winds %>%
      dplyr::mutate_(value = ~ cut(value, breaks = breaks,
                                   include.lowest = TRUE))
  }

  map_data <- dplyr::mutate_(grid_winds,
                             region = ~ as.numeric(gridid)) %>%
    dplyr::select_(~ region, ~ value)

  eastern_states <- c("alabama", "arkansas",
                      "connecticut", "delaware",
                      "district of columbia", "florida",
                      "georgia", "illinois", "indiana",
                      "iowa", "kansas", "kentucky", "louisiana",
                      "maine", "maryland", "massachusetts",
                      "michigan", "mississippi",
                      "missouri", "new hampshire", "new jersey",
                      "new york", "north carolina", "ohio",
                      "oklahoma", "pennsylvania", "rhode island",
                      "south carolina", "tennessee", "texas",
                      "vermont", "virginia", "west virginia",
                      "wisconsin")

  out <- choroplethr::CountyChoropleth$new(map_data)
  out$set_zoom(eastern_states)

  exposure_legend <- paste0("Wind speed (",
                            ifelse(wind_metric == "mps", "m / s",
                                   wind_metric),
                            ")")

  if(!is.null(break_point)){
    out$set_num_colors(num_colors = num_colors)
    out$ggplot_scale <- ggplot2::scale_fill_manual(name = exposure_legend,
                                                   values = c("white", "blue"),
                                                   labels = levels(cut_values))
  } else{
    out$ggplot_scale <- ggplot2::scale_fill_manual(name = exposure_legend,
                                                   values = exposure_palette)
  }
  return(out$render())
}

#' Plot Atlantic basin hurricane tracks
#'
#' Plot the tracks of any selected storms in the hurricane tracking
#'    dataset for the Atlantic basin. This function allows you to
#'    plot a new map or add the tracks to an existing ggplot object.
#'
#' @param storm_tracks Character vector with the names of all storms to plot.
#'    This parameter must use the unique storm identifiers from the
#'    `storm_id` column of the `hurr_tracks` dataframe.
#' @param plot_object NULL or the name of a ggplot object to use as the
#'    underlying plot object. If NULL, the function will generate a new
#'    map of the eastern US states using `default_map`.
#' @param padding Numerical value giving the number of degrees to add to the
#'    outer limits of the plot object (or default map if `plot_object` is
#'    left as NULL) when cropping hurricane tracks.
#' @param plot_points TRUE / FALSE indicator of whether to include points,
#'    as well as lines, when plotting the hurricane tracks.
#' @param alpha Numerical value designating the amount of transparency to
#'    use for plotting tracks.
#' @param color Character string giving the color to use to plot the tracks.
#'
#' @return Returns a ggplot object with plotting data for the storm tracks
#'    of the selected storms. This object can be printed directly or added
#'    on to with other ggplot commands.
#'
#' @export
#'
#' @examples \dontrun{
#' library(ggplot2)
#' data("county_points")
#' grid_winds_floyd <- get_grid_winds(hurr_track = floyd_tracks,
#'                                    grid_df = county_points)
#' floyd_map <- map_wind(grid_winds_floyd, value = "vmax_sust",
#'                       wind_metric = "knots") +
#'              ggtitle("Maximum sustained wind speeds")
#' add_storm_track(floyd_tracks, plot_object = floyd_map)
#' }
#'
#' @importFrom dplyr %>%
#' @export
add_storm_track <- function(storm_tracks, plot_object, padding = 2,
                       plot_points = FALSE, alpha = 1,
                       color = "firebrick"){

  map_data <- plot_object$data
  map_dim <- apply(map_data[ , c("long", "lat")], MARGIN = 2,
                   function(x) range(x) + c(-1, 1) * padding)
  tracks <- storm_tracks %>%
    dplyr::select_(~ latitude, ~ longitude, ~ date) %>%
    dplyr::filter_(~ longitude > map_dim[1, 1] &
                     longitude < map_dim[2, 1] &
                     latitude > map_dim[1, 2] &
                     latitude < map_dim[2, 2]) %>%
    dplyr::mutate_(date = ~ lubridate::ymd_hm(date))

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
#' @param track A dataframe with hurricane track data for a single storm
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
