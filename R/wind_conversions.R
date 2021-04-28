#' Convert symmetric surface wind to gradient wind.
#'
#' Converts maximum 10-m 1-minute symmetric sustained wind speed to gradient
#' wind speed. The conversion factor depends on whether the storm is over land
#' or water.
#'
#' @param vmax_sfc_sym A numeric vector of 1-minute sustained wind speed at
#'    10 meters, with motion asymmetry removed (m / s).
#' @param over_land TRUE / FALSE of whether the storm is over land (TRUE) or
#'    water (FALSE).
#'
#' @details This function uses the following conversion:
#'  \deqn{V_{max,G} = \frac{V_{max,sym}}{f_r}}{
#'  Vmax,G = Vmax,sym / fr}
#'  where:
#'  \itemize{
#'    \item{\eqn{V_{max,G}}{Vmax,G}: Max gradient-level 1-min sustained wind (m / s)}
#'    \item{\eqn{V_{max,sym}}{Vmax,sym}: Max 10-m 1-min sustained wind with motion
#'         asymmetry removed (m / s)}
#'    \item{\eqn{f_r}{fr}: Reduction factor (see below)}
#'  }
#'  The function uses a reduction factor based on Figure 3 in Knaff et al., 2011.
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
#' @return A numeric vector with maximum gradient-level 1-min wind speed (m / s).
#'
#' @references
#'
#' Knaff JA, DeMaria M, Molenar DA, Sampson CR, and Seybold MG. 2011. An
#' automated, objective, multiple-satellite-platform tropical cyclone surface
#' wind speed analysis. Journal of Applied Meteorology and Climatology
#' 50(10):2149-2166
#'
#' @export
calc_gradient_speed <- function(vmax_sfc_sym, over_land){
  reduction_factor <- 0.9
  if(over_land){
    reduction_factor <- reduction_factor * 0.8
  }
  vmax_gl <- vmax_sfc_sym / reduction_factor
  return(vmax_gl)
}

#' Determine if storm is over land or water
#'
#' Determines if the storm is over land or water at its observed location. This
#' function finds the closest grid point in the \code{\link{landmask}} dataframe, then
#' checks if that grid point is over land or water.
#'
#' @param tclon Numeric vector of the absolute value of latitude, in degrees.
#' @inheritParams will7a
#'
#' @return A logical vector of whether the storm is over land (TRUE) or water
#'    (FALSE)
#'
#' @export
check_over_land <- function(tclat, tclon){
  lat_diffs <- abs(tclat - stormwindmodel::landmask$latitude)
  closest_grid_lat <- stormwindmodel::landmask$latitude[which(lat_diffs ==
                                                                min(lat_diffs))][1]

  lon_diffs <- abs(tclon - stormwindmodel::landmask$longitude)
  closest_grid_lon <- stormwindmodel::landmask$longitude[which(lon_diffs ==
                                                                 min(lon_diffs))][1]

  over_land <- stormwindmodel::landmask %>%
    dplyr::filter(.data$latitude == closest_grid_lat &
                     .data$longitude == closest_grid_lon) %>%
    dplyr::mutate(land = .data$land == "land") %>%
    dplyr::select(.data$land)
  over_land <- as.vector(over_land$land[1])

  return(over_land)
}

#' Remove forward speed from maximum wind speed
#'
#' Removes the forward speed of the storm from the maximum storm wind speed,
#' \eqn{V_{max}}{Vmax}, to estimate \eqn{V_{max,sym}}{Vmax,sym}, the storm's
#' maximum 10-m 1-min sustained wind with motion asymmetry removed.
#'
#' @param tcspd A numeric vector giving the tropical cyclone's forward speed (m / s).
#' @param vmax A numeric vector giving maximum 10-m 1-minute sustained wind (m / s)
#'
#' @return A numerical vector with \eqn{V_{max,sym}}{Vmax,sym}, the storm's
#' maximum 10-m 1-min sustained wind with motion asymmetry removed, in m / s.
#'
#' @details This function is based on equation 12 (and accompanying
#'    text) in Phadke et al. 2003. Based on this paper, the correction
#'    factor for forward speed is at its highest at the radius of
#'    maximum winds, where it equals 0.5 times the forward wind speed.
#'
#' @references
#'
#' Phadke AC, Martino CD, Cheung KF, and Houston SH. 2003. Modeling of
#'    tropical cyclone winds and waves for emergency management. Ocean
#'    Engineering 30(4):553-578.
remove_forward_speed <- function(vmax, tcspd){
  vmax_sfc_sym <- vmax - 0.5 * tcspd
  vmax_sfc_sym[vmax_sfc_sym < 0] <- 0
  return(vmax_sfc_sym)
}

#' Calculate surface wind speed from gradient
#'
#' Calculates the surface wind speed based on an estimated gradient
#' wind speed at a point and the radius from the storm center to
#' the grid point.
#'
#' @param wind_gl_aa A numerical value with estimated gradient-level wind speed
#'    (m / s) at a grid point.
#' @param cdist A numerical value with radius from the storm center to the grid
#'    point, in kilometers.
#'
#' @return A numeric vector with the estimated symmetric surface wind speed at
#'    the grid point, in meters / second.
#'
#' @details The reduction factor is based on Figure 3 of Knaff et al., 2003.
#' Over water, it is estimated to be 0.9 up to a radius of 100 km,
#' 0.75 for a radius of 700 km or more, and decrease linearly between
#' a radius of 100 km and 700 km. Points over land should use a reduction
#' factor that is 20\% lower. Because all of the counties are over
#' land, the function makes this adjustment for all grid points.
#'
#' @note This function is only appropriate to use for points that are over
#'    land.
#'
#' @references
#'
#' Knaff JA, DeMaria M, Molenar DA, Sampson CR, and Seybold MG. 2011. An
#' automated, objective, multiple-satellite-platform tropical cyclone surface
#' wind speed analysis. Journal of Applied Meteorology and Climatology
#' 50(10):2149-2166
#'
#' @export
gradient_to_surface <- function(wind_gl_aa, cdist){
  if(cdist <= 100){
    reduction_factor <- 0.9
  } else if(cdist >= 700){
    reduction_factor <- 0.75
  } else {
    reduction_factor <- 0.90 - (cdist - 100) * (0.15/ 600)
  }
  # Since all counties are over land, reduction factor should
  # be 20% lower than if it were over water
  reduction_factor <- reduction_factor * 0.8
  wind_sfc_sym <- wind_gl_aa * reduction_factor
  return(wind_sfc_sym)
}
