#' Calculate distance between two locations
#'
#' This function takes latitudes and longitudes for two locations and
#' calculates the distance (in meters) between the locations using the
#' Haversine method.
#'
#' @param tclat_1 A numeric vector giving latitude of the first location
#'    (degrees)
#' @param tclon_1 A numeric vector giving longitude of the first location
#'    (degrees). This value should be expressed as a positive value for Western
#'    hemisphere longitudes.
#' @param tclat_2 A numeric vector giving latitude of the second location
#'    (degrees)
#' @param tclon_2 A numeric vector giving longitude of the second location
#'    (degrees). This value should be expressed as a positive value for Western
#'    hemisphere longitudes.
#' @param Rearth Radius of the earth (km). Default is 6378.14 km.
#'
#' @return A vector with the distance between the two locations, in kilometers.
#'
#' @details This function uses the Haversine method with great circle distance
#'    to calculate this distance. It is applying the following equations to
#'    determine distance (in kilometers) between two latitude-longitude pairs:
#'    \deqn{hav(\gamma) = hav(\phi_1 - \phi_2) + cos(\phi_1)*cos(\phi_2)*hav(L_1 - L_2)}{
#'    hav(\gamma) = hav(\phi1 - \phi2) + cos(\phi1)*cos(\phi2)*hav(L1 - L2)}
#'    where:
#'    \itemize{
#'      \item{\eqn{\phi_1}{\phi1}: Latitude of first location, in radians}
#'      \item{\eqn{\phi_2}{\phi2}: Latitude of second location, in radians}
#'      \item{\eqn{L_1}{L1}: Longitude of first location, in radians}
#'      \item{\eqn{L_2}{L2}: Longitude of second location, in radians}
#'      \item{\eqn{hav(\gamma)}: The haversine function,
#'         \eqn{hav(\gamma) = sin^2 \left(\frac{\gamma}{2}\right)}{
#'         hav(\gamma) = sin^2 (\gamma / 2)}}
#'      \item{\eqn{R_earth}{Rearth}: Radius of the earth, here assumed to be 6378.14 kilometers}
#'      \item{\eqn{D}}: Distance between the two locations, in kilometers
#'    }
#'
#' @export
latlon_to_km <- function(tclat_1, tclon_1, tclat_2, tclon_2, Rearth = 6378.14){
  tclat_1 <- degrees_to_radians(tclat_1)
  tclon_1 <- degrees_to_radians(tclon_1)
  tclat_2 <- degrees_to_radians(tclat_2)
  tclon_2 <- degrees_to_radians(tclon_2)

  delta_L <- tclon_1 - tclon_2
  delta_tclat <- tclat_1 - tclat_2

  hav_L <- sin(delta_L / 2) ^ 2
  hav_tclat <- sin(delta_tclat / 2) ^ 2

  hav_gamma <- hav_tclat + cos(tclat_1) * cos(tclat_2) * hav_L
  gamma <- 2 * asin(sqrt(hav_gamma))

  dist <- Rearth * gamma
  return(dist)
}

#' Calculate storm's forward speed
#'
#' This storm takes two storm locations and their observations times and
#' calculates the average speed of the storm between the two observations.
#'
#' @param time_1 A date-time vector giving the time of the first observation.
#' @param time_2 A date-time vector giving the time of the second observation.
#' @inheritParams latlon_to_km
#'
#' @return A numeric vector with the average forward speed of the storm between
#'    the two observations, in meters per second.
#'
#' @export
calc_forward_speed <- function(tclat_1, tclon_1, time_1, tclat_2, tclon_2, time_2){
  dist <- latlon_to_km(tclat_1, tclon_1, tclat_2, tclon_2) * 1000
  time <- as.numeric(difftime(time_2, time_1, units = "secs"))
  tcspd <- dist / time
  return(tcspd)
}

#' Calculate bearing from one location to another
#'
#' Calculates the bearing of a second location, as seen from
#' the first location, based on latitude and longitude coordinates for both
#' locations.
#'
#' @inheritParams latlon_to_km
#'
#' @return A numeric vector giving the direction of the second location from the first location,
#'    in degrees. A direction of 0 degrees indicates the second location is
#'    due east of the first, 90 degrees indicates the second location is due
#'    north of the first, etc (i.e., polar, rather than meteorological, coordinate system).
#'
#' @details This function uses the following equations to calculate the bearing
#'    from one latitude-longitude pair to another:
#'
#'    \deqn{S = cos(\phi_2) * sin(L_1 - L_2)}{
#'    S = cos(\phi2) * sin(L1 - L1)}
#'
#'    \deqn{C = cos(\phi_1) * sin(\phi_2) - sin(\phi_1) * cos(\phi_2) * cos(L_1 - L_2)}{
#'    C = cos(\phi1) * sin(\phi2) - sin(\phi1) * cos(\phi2) * cos(L1 - L2)}
#'
#'    \deqn{\theta = atan2(S, C) * \frac{180}{\pi} + 90}
#'
#'    where:
#'    \itemize{
#'      \item{\eqn{\phi_1}{\phi1}: Latitude of first location, in radians}
#'      \item{\eqn{L_1}{L1}: Longitude of first location, in radians}
#'      \item{\eqn{\phi_2}{\phi2}: Latitude of second location, in radians}
#'      \item{\eqn{L_2}{L2}: Longitude of second location, in radians}
#'      \item{\eqn{S, C}: Intermediary results}
#'      \item{\eqn{\theta}: Direction of the storm movement, in degrees}
#'    }
#'
#'    In cases where this equation results in values below 0 degrees or above
#'    360 degrees, the function applies modular arithmetic to bring the value
#'    back within the 0--360 degree range.
#'
#' @export
calc_bearing <- function(tclat_1, tclon_1, tclat_2, tclon_2){
  tclat_1 <- degrees_to_radians(tclat_1)
  tclon_1 <- degrees_to_radians(tclon_1)
  tclat_2 <- degrees_to_radians(tclat_2)
  tclon_2 <- degrees_to_radians(tclon_2)

  S <- cos(tclat_2) * sin(tclon_1 - tclon_2)
  C <- cos(tclat_1) * sin(tclat_2) - sin(tclat_1) * cos(tclat_2) * cos(tclon_1 - tclon_2)

  theta_rad <- atan2(S, C)
  theta <- radians_to_degrees(theta_rad) + 90
  theta <- theta %% 360 # restrict to be between 0 and 360 degrees
  return(theta)
}

#' Convert from degrees to radians
#'
#' Convert an angle from degrees to radians.
#'
#' @param degrees A numeric vector with measurements in degrees.
#'
#' @return A numeric vector with measurement in radians.
degrees_to_radians <- function(degrees){
  degrees * pi / 180
}

#' Convert from radians to degrees
#'
#' @param radians A numeric vector with measurements in radians.
#'
#' @return A numeric vector with the measurement in degrees.
radians_to_degrees <- function(radians){
  radians * 180 / pi
}
