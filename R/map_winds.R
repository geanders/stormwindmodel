#' Map wind exposure at the county level
#'
#' @param grid_winds A dataframe that is the output of
#'    \code{\link{get_grid_winds}}.
#' @param value A character string giving the value to plot. Possible options
#'    are "maxwindspd" and "duration".
#' @param break_point An numeric value giving the value of the "value"
#'    parameter to break at for a binary map showing exposure versus no exposure.
#'
#' @return This function returns a map of the "ggplot" class, plotting
#'    exposure to hurricane winds by county for the eastern half of the United
#'    States.
#'
#' @examples
#' \donttest{
#' load("writing/grid_winds_katrina.Rdata")
#' map_wind(grid_winds_katrina)
#' map_wind(grid_winds_katrina, break_point = 20)
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
map_wind <- function(grid_winds, value = "maxwindspd", break_point = NULL){
  if(!is.null(break_point)){
    cut_values <- cut(grid_winds[ , value],
                      breaks = c(0, break_point, max(grid_winds[ , value])),
                      include.lowest = TRUE)
    grid_winds$value <- as.numeric(cut_values)
    num_colors <- 2
  } else {
    grid_winds$value <- grid_winds[ , value]
    num_colors <- 1
  }

  map_data <- dplyr::mutate(grid_winds,
                            region = as.numeric(gridid)) %>%
    dplyr::select(region, value)
  out <- choroplethr::county_choropleth(map_data,
                                        num_colors = num_colors,
                                        state_zoom = c("alabama", "arkansas",
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
                                                       "wisconsin"))
  if(!is.null(break_point)){
    out <- out + ggplot2::scale_fill_manual(values = c("white", "blue"),
                                            labels = levels(cut_values))
  } else{
    out <- out +
      ggplot2::scale_fill_gradient(low = "white", high = "red") +
      ggplot2::scale_color_gradient(low = "white", high = "red")
  }
  return(out)
}
