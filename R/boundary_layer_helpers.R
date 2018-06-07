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

#' Calculate the correction factor to add forward speed to modeled surface wind
#'
#' Calculates the correction factor (Phadke et al. 2003) to use to adds the
#' storm's forward speed component (i.e., motion asymmetery) back
#' into the estimated u- and v-components of the surface wind speed at a grid point
#' location after u- and v-components of rotational winds have been modeled for the
#' location.
#'
#' @inheritParams add_inflow
#' @inheritParams will3_right
#'
#' @return A numeric vector giving the correction factor, calculated based
#'   on Phadke et al. 2003
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
calc_corr_fct <- function(Rmax, cdist){
  correction_factor <- (Rmax * cdist) / (Rmax ^ 2 + cdist ^ 2)
  return(correction_factor)
}

#' Calculates the u-component of wind with forward motion added
#'
#' Adds the storm's forward speed component (i.e., motion asymmetery) back
#' into the estimated u-component of the surface wind speed at a grid point
#' location after u-component of rotational winds have been modeled for the
#' location.
#'
#' @param wind_sfc_sym A numeric vector with maximum 10-meter 1-minute
#'    sustained wind with motion asymmetry removed (m / s).
#' @param tcspd_u A numeric vector with the tropical cyclone speed, u-component
#'    (m / s).
#' @param swd A numeric vector with surface wind direction (in degrees).
#' @param correction_factor A numeric vector giving the correction factor,
#'  calculated based on Phadke et al. 2003
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
calc_sfc_u <- function(wind_sfc_sym, tcspd_u, swd,
                       correction_factor){
  # Calculate u-component of surface wind speed
  wind_sfc_sym_u <- wind_sfc_sym * cos(degrees_to_radians(swd))

  # Add tangential and forward speed components and calculate
  # magnitude of the u-component of the total wind
  wind_sfc_u <- wind_sfc_sym_u + correction_factor * tcspd_u

  return(wind_sfc_u)
}

#' Calculates the v-component of wind with forward motion added
#'
#' Adds the storm's forward speed component (i.e., motion asymmetery) back
#' into the estimated v-component of the surface wind speed at a grid point
#' location after v-component of rotational winds have been modeled for the
#' location.
#'
#' @param tcspd_v A numeric vector with the tropical cyclone speed, u-component
#'    (m / s).
#' @inheritParams calc_sfc_u
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
calc_sfc_v <- function(wind_sfc_sym, tcspd_v, swd,
                       correction_factor){
  # Calculate v-component of surface wind speed
  wind_sfc_sym_v <- wind_sfc_sym * sin(degrees_to_radians(swd))

  # Add tangential and forward speed components and calculate
  # magnitude of the v-component of the total wind
  wind_sfc_v <- wind_sfc_sym_v + correction_factor * tcspd_v

  return(wind_sfc_v)
}

#' Calculate total modeled surface wind speed
#'
#' Adds together the u- and v-components of the surface winds, after the
#' forward motion of the storm has been added back in, to calculate the
#' total windspeed at a grid point.
#'
#' @param wind_sfc_u A numeric vector with the tropical cyclone speed, u-component
#'    (m / s).
#' @param wind_sfc_v A numeric vector with the tropical cyclone speed, v-component
#'    (m / s).
#'
#' @return A numeric vector giving asymmeric surface windspeed (m / s) at the
#'    location being modeled.
add_forward_speed <- function(wind_sfc_u, wind_sfc_v){

  # Combine the u- and v-components of surface winds to determine
  # the total windspeed
  wind_sfc <- sqrt(wind_sfc_u ^ 2 + wind_sfc_v ^ 2)

  # Reset any negative values to 0
  wind_sfc <- ifelse(wind_sfc > 0, wind_sfc, 0)

  return(wind_sfc)
}

#' Calculate final surface wind direction
#'
#' Determines the final direction of the wind at a given grid point
#' for a given storm location, based on both the symmetrical storm
#' winds and the forward motion of the storm.
#'
#' @inheritParams add_forward_speed
#'
#' @return A numeric vector giving the final surface wind direction,
#'  including both symmetrical and forward storm motion components of
#'  the wind.
calc_sfc_final <- function(wind_sfc_u, wind_sfc_v){

  # Calculate final surface wind direction
  swd_final <- atan2(wind_sfc_v, wind_sfc_u)

  # Convert to degrees
  swd_final <- radians_to_degrees(swd_final)

  # Make sure the direction is between 0 and 360 degrees
  swd_final <- swd_final %% 360

  return(swd_final)
}
