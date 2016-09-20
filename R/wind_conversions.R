#' Convert sustained wind to gradient wind.
#'
#' This function converts 1-minute sustained wind speed at 10 meters to gradient
#' level wind speed.
#'
#' @param sustained_vmax A numeric vector of 1-minute sustained wind speed at
#'    10 meters, in meters / second.
#' @param over_land TRUE / FALSE or whether the storm is over land (the
#'    alternative is that the storm is over water).
#'
#' @details This function uses the following conversion:
#'  \deqn{V_{max} = \frac{V_{max,sustained}}{0.9}}{
#'  Vmax = Vmax_sustained / reduction_factor}
#'  where:
#'  \itemize{
#'    \item{\eqn{V_{max}}{Vmax}: Mean wind speed at gradient level (m / s) }
#'    \item{\eqn{V_{max,sustained}}{Vmax_sustained}: Surface wind speed (10 meters above the water or ground) (m / s)}
#'  }
#'  We make this adjustment based on Figure 3 in Knaff et al., 2011.
#'    If over water and within 100 kilometers of the storm's center,
#'    the ratio of gradient wind speed to surface wind speed is
#'    assumed to be 0.90. If over land, this reduction factor is reduced
#'    by 20\% (\eqn{0.9 * 0.8 = 0.72}).
#'
#'    For this calculation, we assume
#'    that the radius of
#'    maximum wind for all storms is 100 kilometers or smaller (in the code,
#'    we do not calculate \eqn{R_{max}}{R_max} until after we estimate
#'    gradient wind speed from surface wind speed, so we don't have that
#'    storm-specific estimate to use here).
#'
#' @return A numeric vector with gradient-level wind speed, in meters / second.
#'
#' @export
calc_gradient_speed <- function(sustained_vmax, over_land){
  reduction_factor <- 0.9
  if(over_land){
    reduction_factor <- reduction_factor * 0.8
  }
  Vmax <- sustained_vmax / reduction_factor
  return(Vmax)
}

#' Estimate if storm is over land or water
check_over_land <- function(phi, lon){
  dist_to_counties <- mapply(latlon_to_km,
                             phi_1 = phi, L_1 = lon,
                             phi_2 = stormwindmodel::county_points$glat,
                             L_2 = -stormwindmodel::county_points$glon)
  over_land <- min(dist_to_counties) <= 30
  return(over_land)
}

#' Remove forward speed estimate from maximum wind speed
#'
#' This function takes the forward speed of the storm and subtracts it from
#' the maximum storm wind speed, \eqn{V_{max}}.
#'
#' @inheritParams will1a
#' @inheritParams latlon_to_km
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
remove_forward_speed <- function(sustained_vmax, forward_speed){
  sustained_vmax <- sustained_vmax - 0.5 * forward_speed
  sustained_vmax[sustained_vmax < 0] <- 0
  return(sustained_vmax)
}

#' Calculate surface wind speed from gradient
#'
#' Calculates the surface wind speed based on an estimated gradient
#' wind speed at a point and the radius from the storm center to
#' the grid point.
#'
#' @param track Estimated gradient-level wind speed (m / s) at a grid
#'    point.
#' @param r Radius from storm center to the grid point, in kilometers.
#'
#' @return Estimate of surface wind speed at grid point, in meters / second.
#'
#' @details The reduction factor is based on Figure 3 of Knaff et al., 2003.
#' It is estimated to be, for over water, 0.9 up to a radius of 100 km,
#' 0.75 for a radius of 700 km or more, and decrease linearly between
#' a radius of 100 km and 700 km. Points over land should use a reduction
#' factor that is 20% lower. Because all of the counties are over
#' land, we make this adjustment for all grid points.
gradient_to_surface <- function(track, r){
  if(r <= 100){
    reduction_factor <- 0.9
  } else if(r >= 700){
    reduction_factor <- 0.75
  } else {
    reduction_factor <- 0.90 - (r - 100) * (0.15/ 600)
  }
  # Since all counties are over land, reduction factor should
  # be 20% lower than if it were over water
  reduction_factor <- reduction_factor * 0.8
  windspd <- track * reduction_factor
  return(windspd)
}
