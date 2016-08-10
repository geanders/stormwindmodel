#' Add asymmetry due to storm motion
#'
#' This function adds in a factor that accounts for asymmetry when adding back
#' in the forward motion of the storm to the wind speed at a location.
#'
#' @param windspd Surface rotational wind speed at the location, in meters per
#'    second.
#' @param swd Surface wind direction, in degrees. Due east is 0 degrees, due
#'    north is 90 degrees, etc.
#' @param mda Bearing of the storm (i.e., direction of its forward motion),
#'    in degrees. Due east is 0 degrees, due north is 90 degrees, etc.
#' @param forward_speed Forward motion of the storm, in meters per second
#'
#' @note Only do this where windspd > 0 m/s. From NOAA Technical Report 23,
#'    Schwerdt et al., pg. 25
#'
#' @return Numeric vector with wind speed at grid location, with wind
#'    component due to storm motion added back in.
#'
#' @examples
#' add_asymmetry(windspd = 30, swd = 200, mda = 80, forward_speed = 10)
#' plot(add_asymmetry(windspd = 30, swd = 0:360, mda = 80, forward_speed = 10))
#'
#' @references
#'
#' NOAA Technical Report NWS 23, Schwerdt and Watkins, 1979.
#'
#' @export
add_asymmetry <- function(windspd, swd, mda, forward_speed){
  if(is.na(windspd) || is.na(swd) || is.na(mda) || is.na(forward_speed)){
    return(NA)
  } else {
    if(windspd >= 0){
      beta <- swd - mda
      A <- 1.5 * (forward_speed ^ 0.63) * (0.514751 ^ 0.37) *
        cos(beta * pi / 180)
      windspd <- windspd + A
    } else{
      windspd <- 0
    }
    return(windspd)
  }
}

#' Add overland inflow angle
#'
#' This function adds an inflow angle to the angle of the wind direction.
#' It calculates an inflow angle as a function of the distance from the
#' storm center to a location, and then adds 20 degrees to this inflow angle to
#' account for the location being over land rather than overwater.
#'
#' @param gwd Wind direction of the gradient wind at a location, in degrees. Due
#'    east is 0 degrees, due north 90 degrees, etc.
#' @param r Radius (in kilometers) from the storm center to a location.
#' @inheritParams will3_right
#'
#' @return Numeric vector with the gradient wind direction (in degrees),
#'    adjusted with an inflow angle appropriate for being overland.
#'
#' @examples
#' add_inflow(gwd = 160, r = 100, Rmax = 20)
#'
#' @export
add_inflow <- function(gwd, r, Rmax){
  if(is.na(gwd) | is.na(r) | is.na(Rmax)){
    return(NA)
  }

  # Calculate inflow angle over water based on radius of location from storm
  # center in comparison to radius of maximum winds (Phadke et al. 2003)
  if(r < Rmax){
    inflow_angle <- 10 + (1 + (r / Rmax))
  } else if(Rmax <= r & r < 1.2 * Rmax){
    inflow_angle <- 20 + 25 * ((r / Rmax) - 1)
  } else {
    inflow_angle <- 25
  }

  # Add 20 degrees to inflow angle since location is over land, not water
  overland_inflow_angle <- inflow_angle + 20

  # Add inflow angle to gradient wind direction
  gwd_with_inflow <- (gwd + overland_inflow_angle) %% 360

  return(gwd_with_inflow)
}
