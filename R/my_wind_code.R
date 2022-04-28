#' Add Willoughby inputs and parameters
#'
#' Calculates a number of inputs and parameters needed for the Willoughby model
#' for each observation of a storm track. This function intakes an imputed storm
#' track and calculates these parameters for each observation in the storm
#' tracks.These inputs and parameters are later used to model wind speed
#' at each grid point for every observation.
#'
#' @param full_track A dataframe with interpolated hurricane track data, as
#'    created by \code{\link{create_full_track}}. This dataframe must have the
#'    following columns:
#'    \itemize{
#'      \item{\code{date:} Date-time, in POSIXct format and UTC time zone}
#'      \item{\code{tclat:} Latitude (decimal degree)}
#'      \item{\code{tclon:} Longitude (decimal degrees), expressed as positive
#'            values (this model assumes all longitudes will be in the
#'            Western hemisphere)}
#'      \item{\code{vmax:} Maximum 10-meter sustained wind speed (m / s)}
#'    }
#'
#' @return The input dataset, but with columns added for the Willoughby
#'    parameters for every observations. Added columns include:
#'    \itemize{
#'      \item{\code{tcdir:} The bearing of the storm, in degrees, with 0 degrees
#'        indicating the storm is moving due east, 90 degrees indicating the
#'        storm is moving due north, etc.}
#'      \item{\code{tcspd_u, tcspd_v:} The u- and v-components of the forward
#'      speed of the storm, in meters per second}
#'      \item{\code{vmax_gl:} The maximum gradient-level 1-minute sustained
#'        wind, in m / s}
#'      \item{\code{Rmax:} The radius to maximum winds, in kilometers}
#'      \item{\code{X1, n, A, R1, R2:} Parameters needed for
#'        the Willoughby wind model}
#'    }
#'
#' @importFrom rlang .data
#'
#' @references
#'
#' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
#' of the primary hurricane vortex. Part II: A new family of sectionally
#' continuous profiles. Monthly Weather Review 134(4):1102-1120.
#'
#' @examples
#' \dontrun{
#' data("floyd_tracks")
#' full_track <- create_full_track(hurr_track = floyd_tracks)
#' with_wind_radii <- add_wind_radii(full_track = full_track)
#' head(with_wind_radii)
#' }
#'
#' @export
add_wind_radii <- function(full_track = create_full_track()){

        with_wind_radii <- full_track %>%
          dplyr::mutate(tcspd = calc_forward_speed(tclat_1 = .data$tclat,
                                                     tclon_1 = .data$tclon,
                                                     time_1 = .data$date,
                                                     tclat_2 = dplyr::lead(.data$tclat),
                                                     tclon_2 = dplyr::lead(.data$tclon),
                                                     time_2 = dplyr::lead(.data$date)),
                        tcdir = calc_bearing(tclat_1 = .data$tclat,
                                               tclon_1 = .data$tclon,
                                               tclat_2 = dplyr::lead(.data$tclat),
                                               tclon_2 = dplyr::lead(.data$tclon)),
                        tcspd_u = .data$tcspd * cos(degrees_to_radians(.data$tcdir)),
                        tcspd_v = .data$tcspd * sin(degrees_to_radians(.data$tcdir)),
                        vmax_sfc_sym = remove_forward_speed(vmax = .data$vmax,
                                                              tcspd = .data$tcspd),
                        over_land = mapply(check_over_land,
                                             tclat = .data$tclat,
                                             tclon = .data$tclon),
                        vmax_gl = mapply(calc_gradient_speed,
                                           vmax_sfc_sym = .data$vmax_sfc_sym,
                                           over_land = .data$over_land),
                        Rmax = will7a(.data$vmax_gl, .data$tclat),
                        X1 = will10a(.data$vmax_gl, .data$tclat),
                        n = will10b(.data$vmax_gl, .data$tclat),
                        A = will10c(.data$vmax_gl, .data$tclat),
                        eq3_right = will3_right(.data$n, .data$A,
                                                  .data$X1, .data$Rmax),
                        xi = mapply(solve_for_xi, eq3_right = .data$eq3_right),
                        R1 = calc_R1(.data$Rmax, .data$xi),
                        R2 = ifelse(.data$Rmax > 20, .data$R1 + 25, .data$R1 + 15)
                        ) %>%
          dplyr::select(c(.data$date, .data$tclat, .data$tclon, .data$tcdir, .data$tcspd_u,
                          .data$tcspd_v, .data$vmax_gl, .data$Rmax, .data$X1, .data$n,
                          .data$A, .data$R1, .data$R2))
        return(with_wind_radii)
}

#' Calculate wind speed at grid points
#'
#' Uses the Willoughby wind model (Willoughby et al. 2006) to calculate wind
#' speed at a grid point location. This function calculates a wind time series at
#' just one location.
#'
#' @param with_wind_radii A dataframe of storm tracks, including inputs and
#'    parameters for the Willoughby wind model, as created by
#'    \code{\link{add_wind_radii}}.
#' @param grid_point A one-row dataframe with the grid id, latitude,
#'    longitude, and a logical value for whether the point is over land (TRUE) or
#'    water (FALSE) for a single location for which you want to model winds.
#' @param max_dist A numeric value giving (in kilometers) the maximum distance
#'    from the storm's center to model storm-associated winds. Any value beyond
#'    this distance will be automatically set to 0 m / s for storm-associated winds.
#'    The default value is 2222.4 km (1200 nautical miles).
#'
#' @return A dataframe with date (\code{date}) and modeled wind speed
#'    (\code{windspeed}, in m / s) at the grid point location for all storm
#'    observations. There are also columns for the distance (how far the stomr
#'    was from the location at that timepoint) and surface wind direction (in polar
#'    conventions, so 0 degrees is due East, 90 is due North, etc.). If the storm
#'    was further than the specified \code{max_dist} from the location, then surface
#'    wind direction will not be calculated and instead will have a missing (\code{NA})
#'    value.
#'
#' @examples \dontrun{
#' data("floyd_tracks")
#' data("county_points")
#' full_track <- create_full_track(hurr_track = floyd_tracks)
#' with_wind_radii <- add_wind_radii(full_track = full_track)
#' wind_grid <- calc_grid_wind(grid_point = county_points[county_points$gridid == "37055", ],
#'                             with_wind_radii = with_wind_radii)
#' head(wind_grid)
#' }
#'
#' @references
#'
#' Knaff JA, DeMaria M, Molenar DA, Sampson CR, and Seybold MG. 2011. An
#' automated, objective, multiple-satellite-platform tropical cyclone surface
#' wind speed analysis. Journal of Applied Meteorology and Climatology
#' 50(10):2149-2166
#'
#' Phadke AC, Martino CD, Cheung KF, and Houston SH. 2003. Modeling of
#'    tropical cyclone winds and waves for emergency management. Ocean
#'    Engineering 30(4):553-578.
#'
#' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
#' of the primary hurricane vortex. Part II: A new family of sectionally
#' continuous profiles. Monthly Weather Review 134(4):1102-1120.
#'
#' @export
calc_grid_wind <- function(grid_point = stormwindmodel::county_points[1, ],
                           with_wind_radii = add_wind_radii(),
                           max_dist = 2222.4){   # 2222.4 km equals 1200 n. miles

  grid_calc <- calc_grid_winds_cpp(glat = grid_point$glat, glon = grid_point$glon,
                                                   glandsea = grid_point$glandsea, max_dist = max_dist,
                                                   tclat = with_wind_radii$tclat, tclon = with_wind_radii$tclon,
                                                   Rmax = with_wind_radii$Rmax, R1 = with_wind_radii$R2,
                                                   R2 = with_wind_radii$R2, vmax_gl = with_wind_radii$vmax_gl,
                                                   n = with_wind_radii$n, A = with_wind_radii$A,
                                                   X1 = with_wind_radii$X1, tcspd_u = with_wind_radii$tcspd_u,
                                                   tcspd_v = with_wind_radii$tcspd_v)

  grid_wind <- tibble::tibble(date = with_wind_radii$date,
                      windspeed = grid_calc[["vmax_sust"]][ , 1],
                      dist_from_storm = grid_calc[["distance_from_storm"]][ , 1],
                      surface_wind_direction = grid_calc[["surface_wind_direction"]][ , 1])

        return(grid_wind)
}

#' Determine hurricane winds at locations
#'
#' This function inputs a storm track and a dataset of locations and calculates
#' highest wind speeds (sustained and maximum) and duration of winds above a
#' certain speed at each location. It is assumed that track data entered has been
#' measured in knots, at 10 m above surface level, and with 1-minute averaging
#' period. The dataset of locations can either be a regularly-spaced grid or can
#' be the central points of locations, like counties or cities. For counties in
#' the eastern half of the United States, the \code{county_points} dataset that
#' comes with the package can be used as the \code{grid_point} input.
#'
#' @inheritParams create_full_track
#' @inheritParams calc_grid_wind
#' @inheritParams summarize_grid_wind
#' @param grid_df A dataframe of locations at which to calculate wind characteristics.
#'    This dataframe must include columns for latitude and longitude for each
#'    point, and these columns must be named \code{glat} and \code{glon}.
#'    The latitudes and longitudes should be in decimal degrees, with longitudes
#'    being entered in degrees East. Therefore Western hemisphere (so, almost all
#'    those for Atlantic basin storms) should be expressed as negative values.
#'
#' @return The dataframe of locations input, with the following columns of wind
#' characteristics added for each location:
#'    \itemize{
#'      \item{\code{vmax_gust}: Maximum value of surface-level (10 meters)
#'        gust winds, in meters per second, over the length of the
#'        storm at the given location}
#'      \item{\code{vmax_sust}: Maximum value of surface-level (10 meters)
#'        sustained winds, in meters per second, over the length of the
#'        storm at the given location}
#'      \item{\code{gust_duration}: Length of time, in minutes, that
#'        surface-level gust winds were above a specified value (default
#'        is 20 m / s)}
#'      \item{\code{sust_duration}: Length of time, in minutes, that
#'        surface-level sustained winds were above a specified value (default
#'        is 20 m / s)}
#'    }
#'
#' @note
#' This function can take a few minutes to run, depending on the number
#' of locations that are being modeled.
#'
#' @examples \dontrun{
#' data("floyd_tracks")
#' data("county_points")
#' grid_winds <- get_grid_winds(hurr_track = floyd_tracks,
#'                              grid_df = county_points)
#' }
#'
#' @export
get_grid_winds <- function(hurr_track = stormwindmodel::floyd_tracks,
                           grid_df = stormwindmodel::county_points,
                           tint = 0.25,
                           gust_duration_cut = 20,
                           sust_duration_cut = 20,
                           max_dist = 2222.4){

  grid_winds <- calc_grid_winds(hurr_track = hurr_track,
                                 grid_df = grid_df,
                                 tint = tint,
                                 max_dist = max_dist)
  grid_winds_summary <- summarize_grid_winds(grid_winds = grid_winds$vmax_sust,
                                             gust_duration_cut = gust_duration_cut,
                                             sust_duration_cut = sust_duration_cut,
                                             tint = tint)

  return(grid_winds_summary)
}

#' Calculate hurricane winds at locations
#'
#' This function inputs a storm track and a dataset of locations and calculates the
#' full time series of windspeeds over the course of the storm at each location.
#' It also returns the distance of the storm from the location at each time
#' point, as well as the surface wind direction at each time point that the storm
#' is within the distance specified by \code{max_dist}. It is assumed that the
#' track data entered has been measured in knots, at 10 m above surface level, and with
#' 1-minute averaging period. The dataset of locations can either be a regularly-spaced
#' grid or can be the central points of locations, like counties or cities. For counties
#' in the eastern half of the United States, the \code{county_points} dataset that
#' comes with the package can be used as the \code{grid_point} input.
#'
#' @inheritParams create_full_track
#' @inheritParams calc_grid_wind
#'
#' @return An array with three elements, the first with modeled wind speeds, the
#'   second with distance of the storm from the location, at the third with the
#'   angle of surface winds. Each of the elements is a matrix, where the rows give
#'   time points over the course of the storm and the columns give each of the locations.
#'   By extracting a column from one of the matrices, you can get the time series
#'   at that location over the course of the storm (for example, by extracting a
#'   column from the first element, you can get a time series of windspeed at that
#'   location over the course of the storm).
#'
#' @note
#' This function can take a few minutes to run, depending on the number of locations
#' that are being modeled.
#'
#' @examples \dontrun{
#' library(tibble)
#' library(ggplot2)
#' library(lubridate)
#' data("floyd_tracks")
#' data("county_points")
#'
#' floyd_winds <- calc_grid_winds(hurr_track = floyd_tracks, grid_df = county_points)
#'
#' dare_county_fips <- "37055"
#' dare_floyd_winds <- floyd_winds[["vmax_sust"]][ , dare_county_fips] %>%
#'   enframe(name = "timepoint", value = "sustained_wind")
#' ggplot(dare_floyd_winds, aes(x = ymd_hms(timepoint), y = sustained_wind)) +
#'   geom_line()
#' }
#'
#' @export
calc_grid_winds <- function(hurr_track = stormwindmodel::floyd_tracks,
                            grid_df = stormwindmodel::county_points,
                            tint = 0.25,
                            max_dist = 2222.4){

  full_track <- create_full_track(hurr_track = hurr_track, tint = tint)
  with_wind_radii <- add_wind_radii(full_track = full_track)

  grid_winds <- stormwindmodel:::calc_grid_winds_cpp(glat = grid_df$glat, glon = grid_df$glon,
                                    glandsea = grid_df$glandsea, max_dist =  max_dist,
                                    tclat = with_wind_radii$tclat, tclon = with_wind_radii$tclon,
                                    Rmax = with_wind_radii$Rmax, R1 = with_wind_radii$R1,
                                    R2 = with_wind_radii$R2, vmax_gl = with_wind_radii$vmax_gl,
                                    n = with_wind_radii$n, A = with_wind_radii$A, X1 = with_wind_radii$X1,
                                    tcspd_u = with_wind_radii$tcspd_u, tcspd_v = with_wind_radii$tcspd_v)

  colnames(grid_winds[[1]]) <- as.character(grid_df$gridid)
  rownames(grid_winds[[1]]) <- as.character(with_wind_radii$date)
  colnames(grid_winds[[2]]) <- as.character(grid_df$gridid)
  rownames(grid_winds[[2]]) <- as.character(with_wind_radii$date)
  colnames(grid_winds[[3]]) <- as.character(grid_df$gridid)
  rownames(grid_winds[[3]]) <- as.character(with_wind_radii$date)

  return(grid_winds)
}

#' Generate wind summaries for grid points
#'
#' Summarizes the wind time series for grid points, as
#' created by \code{\link{calc_grid_winds}}.
#'
#' @param grid_winds A matrix where each column is a time series of modeled wind speeds at
#'    a location, as created by \code{\link{calc_grid_winds}}.
#' @param gust_duration_cut The wind speed, in meters per second, to use as a
#'    cutoff point for determining the duration of gust winds. The function
#'    will calculate the minutes during the storm when surface-level gust winds
#'    were above this speed at the location.
#' @param sust_duration_cut The wind speed, in meters per second, to use as a
#'    cutoff point for determining the duration of gust winds. The function
#'    will calculate the minutes during the storm when surface-level gust winds
#'    were above this speed at the location.
#' @inheritParams create_full_track
#'
#' @importFrom rlang .data
#'
#' @return Returns a dataframe with wind characteristics for each
#'    location. The wind characteristics given are:
#'    \itemize{
#'      \item{\code{vmax_gust}: Maximum value of surface-level (10 meters)
#'        gust winds, in meters per second, over the length of the
#'        storm at the given location}
#'      \item{\code{vmax_sust}: Maximum value of surface-level (10 meters)
#'        sustained winds, in meters per second, over the length of the
#'        storm at the given location}
#'      \item{\code{gust_duration}: Length of time, in minutes, that
#'        surface-level gust winds were above a specified value (default is
#'        20 meters per second)}
#'      \item{\code{sust_duration}: Length of time, in minutes, that
#'        surface-level sustained winds were above a specified value (default is
#'        20 meters per second}
#'    }
#'
#' @export
summarize_grid_winds <- function(grid_winds,
                                 gust_duration_cut = 20,
                                 sust_duration_cut = 20,
                                 tint = 0.25){

  calc_sust_dur <- function(wind){
    60 * tint * sum(wind > sust_duration_cut, na.rm = TRUE)
  }

  calc_gust_dur <- function(wind){
    60 * tint * sum(wind > gust_duration_cut, na.rm = TRUE)
  }

  grid_wind_summary <- tibble::tibble(gridid = colnames(grid_winds),
                                     date_time_max_wind = rownames(grid_winds)[apply(grid_winds, MARGIN = 2, FUN = which.max)],
                                     vmax_sust = apply(grid_winds, MARGIN = 2, FUN = max, na.rm = TRUE),
                                     vmax_gust = .data$vmax_sust * 1.49,
                                     sust_dur = apply(grid_winds, MARGIN = 2, FUN = calc_sust_dur),
                                     gust_dur = apply(grid_winds, MARGIN = 2, FUN = calc_gust_dur)
                                     ) %>%
    dplyr::mutate(date_time_max_wind = ifelse(.data$vmax_sust == 0, NA, .data$date_time_max_wind),
           date_time_max_wind = lubridate::ymd_hms(.data$date_time_max_wind))

  return(grid_wind_summary)
}


