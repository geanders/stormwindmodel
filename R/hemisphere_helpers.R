#' Check if storm track is in Northern Hemisphere
#'
#' This function determines if the storm track is located in the Northern
#' Hemisphere or Southern Hemisphere.
#'
#' @inheritParams create_full_track
#'
#' @return Logical object with a value of TRUE if the storm track is located in
#'     the Northern Hemisphere (all latitudes are positive), a value of FALSE if
#'     in the Southern Hemisphere (all latitudes are negative).
#'
#' @note
#'
#' If all latitudes are not either positive or negative the function returns NaN
#' with a warning (as storm tracks should not cross the equator).
#'
#' @export
check_north_hemisphere <- function(hurr_track) {
  if (all(hurr_track$latitude > 0)) {
    TRUE
  } else if (all(hurr_track$latitude < 0)) {
    FALSE
  } else {
    warning("hemisphere (north/south) undefined")
    NaN
  }
}

#' Check if storm track is in Western Hemisphere
#'
#' This function determines if the storm track is located in the Western
#' Hemisphere or Eastern Hemisphere.
#'
#' @inheritParams create_full_track
#'
#' @return Logical object with a value of TRUE if any part of the the storm track
#'     is located in the Western Hemisphere (any longitudes are negative), a value
#'     of FALSE if the storm track is located in the Eastern Hemisphere (all
#'     latitudes are positive).
#'
#' @note
#'
#' Storms with some positive and some negative latitudes are classified as Western
#' Hemisphere by this function.
#'
#' @export
check_west_hemisphere <- function(hurr_track) {
  if (all(hurr_track$longitude < 0 & hurr_track$longitude > -180)) {
    TRUE
  } else if (all(hurr_track$longitude > 0 & hurr_track$longitude < 180)) {
    FALSE
  } else {
    warning("hemisphere (east/west) undefined")
    NaN
  }
}

#' Hemispheres of storm track
#'
#' This function determines if the storm track is located in the Northern
#' Hemisphere or Southern Hemisphere and Eastern or Western Hemisphere.
#'
#' @inheritParams create_full_track
#'
#' @return Factor object in which NW (level = 1) indicates that the storm track
#'     is located in the Northern and Western Hemispheres; NE (level = 2)
#'     indicates that the storm track is located in the Northern and Eastern
#'     Hemispheres; SW (level = 3) indicates that the storm track is located in
#'     the Southern and Western Hemispheres; and SE (level = 4) indicates that
#'     the storm track is located in the Southern and Eastern Hemispheres; NB
#'     (level = 5) indicates that the storm track is in the Northern Hemisphere
#'     but crosses between the Eastern and Western Hemispheres; SB (level = 6)
#'     indicates that the storm track is in the Southern Hemisphere but crosses
#'     between the Eastern and Western Hemispheres.
#'
#' @note
#'
#' Storms with some positive and some negative latitudes are classified as Western
#' Hemisphere by this function.
#'
#' @export
check_hemisphere <- function(hurr_track) {
  north_hem <- check_north_hemisphere(hurr_track = hurr_track)
  west_hem <- check_west_hemisphere(hurr_track = hurr_track)
  hemisphere <- factor(levels = c(1,2,3,4,5,6), labels = c("NW","NE","SW","SE","NB","SB"))
  if (north_hem == FALSE && is.na (west_hem)) {
    hemisphere <- 6
  } else if (north_hem == TRUE && is.na (west_hem)) {
    hemisphere <- 5
  } else if (north_hem == FALSE && west_hem == FALSE) {
    hemisphere <- 4
  } else if (north_hem == FALSE && west_hem == TRUE) {
    hemisphere <- 3
  } else if (north_hem == TRUE && west_hem == FALSE) {
    hemisphere <- 2
  } else if (north_hem == TRUE && west_hem == TRUE) {
    hemisphere <- 1
  }
  return(hemisphere)
}

#' Modify storm latitude and longitude values based on Hemispheres
#'
#' This function modifies the latitude and longitude values in a storm track
#' data frame on the basis of Earth Hemispheres. Because this package was
#' originally designed for work in the Northern and Western Hemispheres, some
#' functions in this package will not work optimally for storms with negative
#' latitudes and positive longitudes.
#'
#' @inheritParams create_full_track
#' @param hemisphere Factor object in which NW (level = 1) indicates that the
#'     storm track is located in the Northern and Western Hemispheres; NE
#'     (level = 2) indicates that the storm track is located in the Northern and
#'     Eastern Hemispheres; SW (level = 3) indicates that the storm track is
#'     located in the Southern and Western Hemispheres; and SE (level = 4)
#'     indicates that the storm track is located in the Southern and Eastern
#'     Hemispheres.
#'
#' @return A modified storm track dataframe, in which the longitudes for Eastern
#'     Hemisphere storms have been multiplied by negative one, and the latitudes
#'     for Southern Hemisphere storms have been multiplied by negative one.
#'
#' @note
#'
#' Storms with some positive and some negative latitudes are classified as Western
#' Hemisphere by this function.
#'
#' @export
hem_adjust_track <- function (hurr_track, hemisphere) {
  if (hemisphere == 2) {
    hurr_track <- hurr_track %>%
      dplyr::mutate_(longitude = ~ -longitude)
  }
  if (hemisphere == 3) {
    hurr_track <- hurr_track %>%
      dplyr::mutate_(latitude = ~ -latitude)
  }
  if (hemisphere == 4) {
    hurr_track <- hurr_track %>%
      dplyr::mutate_(longitude = ~ -longitude) %>%
      dplyr::mutate_(latitude = ~ -latitude)
  }
  return(hurr_track)
}

#' Modify storm latitude and longitude values based on Hemispheres
#'
#' This function modifies the latitude and longitude values in a storm track
#' data frame on the basis of Earth Hemispheres. Because this package was
#' originally designed for work in the Northern and Western Hemispheres, some
#' functions in this package will not work optimally for storms with negative
#' latitudes and positive longitudes.
#'
#' @inheritParams create_full_track
#' @inheritParams hem_adjust_track
#'
#' @return A modified dataframe, in which the longitudes for dataframes
#'     associated with Eastern Hemisphere storm tracks have been multiplied
#'     by negative one, and the latitudes for dataframes associated with Southern
#'     Hemisphere storms have been multiplied by negative one.
#'
#' @note
#'
#' Storms with some positive and some negative latitudes are classified as Western
#' Hemisphere by this function.
#'
#' @export
hem_adjust_grid <- function (grid, hurr_track, hemisphere) {
  if (hemisphere == 2) {
    grid <- grid %>%
      dplyr::mutate_(glon = ~ -glon)
  }
  if (hemisphere == 3) {
    grid <- grid %>%
      dplyr::mutate_(glat = ~ - glat)
  }
  if (hemisphere == 4) {
    grid <- grid %>%
      dplyr::mutate_(glon = ~ -glon) %>%
      dplyr::mutate_(glat = ~ - glat)
  }
  return(grid)
}

