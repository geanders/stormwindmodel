#' Add overland inflow angle
#'
#' This function adds an inflow angle to the angle of the wind direction.
#' It calculates an inflow angle as a function of the distance from the
#' storm center to a location, and then adds 20 degrees to this inflow angle to
#' account for the location being over land rather than overwater.
#'
#' @param gwd Wind direction of the gradient wind at a location, in degrees. Due
#'    east is 0 degrees, due north 90 degrees, etc.
#' @param cdist Radius (in kilometers) from the storm center to a location.
#' @inheritParams will3_right
#'
#' @return Numeric vector with the gradient wind direction (in degrees),
#'    adjusted with an inflow angle appropriate for being overland.
#'
#' @examples
#' add_inflow(gwd = 160, r = 100, Rmax = 20)
#'
#' @export
add_inflow <- function(gwd, cdist, Rmax){
  if(is.na(gwd) | is.na(cdist) | is.na(Rmax)){
    return(NA)
  }

  # Calculate inflow angle over water based on radius of location from storm
  # center in comparison to radius of maximum winds (Phadke et al. 2003)
  if(cdist < Rmax){
    inflow_angle <- 10 + (1 + (cdist / Rmax))
  } else if(Rmax <= cdist & cdist < 1.2 * Rmax){
    inflow_angle <- 20 + 25 * ((cdist / Rmax) - 1)
  } else {
    inflow_angle <- 25
  }

  # Add 20 degrees to inflow angle since location is over land, not water
  overland_inflow_angle <- inflow_angle + 20

  # Add inflow angle to gradient wind direction
  gwd_with_inflow <- (gwd + overland_inflow_angle) %% 360

  return(gwd_with_inflow)
}

#' Adds forward speed component
#'
#' Adds the storm's forward speed component back into the estimated
#' surface wind speed.
add_forward_speed <- function(wind_sfc_sym, tcspd_u, tcspd_v, swd, cdist, Rmax){
  # Calculate u- and v-components of surface wind speed
  wind_sfc_sym_u <- wind_sfc_sym * cos(degrees_to_radians(swd))
  wind_sfc_sym_v <-  wind_sfc_sym * sin(degrees_to_radians(swd))

  # Add back in component from forward motion of the storm
  correction_factor <- (Rmax * cdist) / (Rmax ^ 2 + cdist ^ 2)

  # Add tangential and forward speed components and calculate
  # magnitude of this total wind
  wind_sfc_u <- wind_sfc_sym_u + correction_factor * tcspd_u
  wind_sfc_v <- wind_sfc_sym_v + correction_factor * tcspd_v
  wind_sfc <- sqrt(wind_sfc_u ^ 2 + wind_sfc_v ^ 2)

  # Reset any negative values to 0
  wind_sfc <- ifelse(wind_sfc > 0, wind_sfc, 0)

  return(wind_sfc)
}
