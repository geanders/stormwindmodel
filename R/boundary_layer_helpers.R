#' Add inflow angle
#'
#' This function adds an inflow angle to the angle of the wind direction.
#' It calculates an inflow angle as a function of the distance from the
#' storm center to a location (Phadke et al. 2003), and then adds 20 degrees to
#' this inflow angle to account for the location being over land rather than
#' over water.
#'
#' @param gwd A numeric vector giving direction of gradient wind at a location,
#'    in degrees. Due east is 0 degrees, due north 90 degrees, etc.
#' @param cdist A numeric vector giving radius (in kilometers) from the storm
#'    center to the location being modeled.
#' @inheritParams will3_right
#'
#' @return Numeric vector with the gradient wind direction (in degrees),
#'    adjusted with an inflow angle appropriate for being over land and for the
#'    location's distance from the storm's center.
#'
#' @details
#'
#' This function uses equations 11a-c from Phadke et al. (2003).
#'
#' @note
#'
#' This function is only appropriate for modeling wind speeds for locations
#' that are over land.
#'
#' @references
#'
#' Phadke AC, Martino CD, Cheung KF, and Houston SH. 2003. Modeling of
#'    tropical cyclone winds and waves for emergency management. Ocean
#'    Engineering 30(4):553-578.
#'
#' @examples
#' add_inflow(gwd = 160, cdist = 100, Rmax = 20)
#'
#' @export
add_inflow <- function(gwd, cdist, Rmax){
  if (is.na(gwd) | is.na(cdist) | is.na(Rmax)){
    return(NA)
  }

  # Calculate inflow angle over water based on radius of location from storm
  # center in comparison to radius of maximum winds (Phadke et al. 2003)
  if (cdist < Rmax){
    inflow_angle <- 10 + (1 + (cdist / Rmax))
  } else if (Rmax <= cdist & cdist < 1.2 * Rmax){
    inflow_angle <- 20 + 25 * ( (cdist / Rmax) - 1)
  } else {
    inflow_angle <- 25
  }

  # Add 20 degrees to inflow angle since location is over land, not water
  overland_inflow_angle <- inflow_angle + 20

  # Add inflow angle to gradient wind direction
  gwd_with_inflow <- (gwd + overland_inflow_angle) %% 360

  return(gwd_with_inflow)
}

#' Adds forward speed component to modeled surface wind
#'
#' Adds the storm's forward speed component (i.e., motion asymmetery) back
#' into the estimated surface wind speed at a grid point location after
#' rotational winds have been modeled for the location.
#'
#' @param wind_sfc_sym A numeric vector with maximum 10-meter 1-minute
#'    sustained wind with motion asymmetry removed (m / s).
#' @param tcspd_u A numeric vector with the tropical cyclone speed, u-component
#'    (m / s).
#' @param tcspd_v A numeric vector with the tropical cyclone speed, v-component
#'    (m / s).
#' @param swd A numeric vector with surface wind direction (in degrees).
#' @inheritParams add_inflow
#' @inheritParams will3_right
#'
#' @return A numeric vector giving asymmeric surface windspeed (m / s) at the
#'    location being modeled.
#'
#' @details
#'
#' This function uses equation 12 from Phadke et al. (2003).
#'
#' @references
#'
#' Phadke AC, Martino CD, Cheung KF, and Houston SH. 2003. Modeling of
#'    tropical cyclone winds and waves for emergency management. Ocean
#'    Engineering 30(4):553-578.
#' @export
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
