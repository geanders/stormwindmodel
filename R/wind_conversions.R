#' Convert sustained wind to gradient wind.
#'
#' This function converts 1-minute sustained wind speed at 10 meters to gradient
#' level wind speed.
#'
#' @param sustained_vmax A numeric vector of 1-minute sustained wind speed at
#'    10 meters, in meters / second.
#'
#' @details This function uses the following conversion:
#'  \deqn{V_{max} = \frac{V_{max,sustained}}{0.9}}{
#'  Vmax = Vmax_sustained / 0.9}
#'  where:
#'  \itemize{
#'    \item{\eqn{V_{max}}{Vmax}: Mean wind speed at gradient level (m / s) }
#'    \item{\eqn{V_{max,sustained}}{Vmax_sustained}: Surface wind speed (10 meters above the water or ground) (m / s)}
#'  }
#'
#' @return A numeric vector with gradient-level wind speed, in meters / second.
#'
#' @export
calc_gradient_speed <- function(sustained_vmax){
  Vmax <- sustained_vmax / 0.9
  return(Vmax)
}

#' Remove forward speed estimate from maximum wind speed
#'
#' This function takes the forward speed of the storm and subtracts it from
#' the maximum storm wind speed, \eqn{V_{max}}.
#'
#' @inheritParams will1a
#' @inheritParams latlon_to_meters
#' @inheritParams calc_forward_speed
#' @inheritParams calc_gradient_speed
#'
#' @return A numerical vector with the maximum storm wind speed, with forward
#' storm motion speed removed, in m / s.
#'
#' @details This function is based on equation 12 (and accompanying
#'    text) in Phadke et al. 2003. Based on this paper, the correction
#'    factor for forward speed is at its highest at the radius of
#'    maximum winds, where it equals 0.5 times the forward wind speed.
#'
#' @references
#'
#' Padke AC, Martino CD, Cheung KF, and Houston SH. 2003. Modeling of
#'    tropical cyclone winds and waves for emergency management. Ocean
#'    Engineering 30:553-578.
remove_forward_speed <- function(sustained_vmax, phi_1, L_1, time_1,
                                 phi_2, L_2, time_2){
  forward_speed <- calc_forward_speed(phi_1, L_1, time_1,
                                      phi_2, L_2, time_2)
  sustained_vmax <- sustained_vmax - 0.5 * forward_speed
  sustained_vmax[sustained_vmax < 0] <- 0
  return(sustained_vmax)
}
