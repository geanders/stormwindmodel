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
                        tcspd_u = tcspd * cos(degrees_to_radians(.data$tcdir)),
                        tcspd_v = tcspd * sin(degrees_to_radians(.data$tcdir)),
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
          dplyr::select(c(date, tclat, tclon, tcdir, tcspd_u, tcspd_v,
                          vmax_gl, Rmax, X1, n, A, R1, R2))
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
#' @param grid_point A one-row dataframe with the grid id, latitude, and
#'    longitude for a single location for which you want to model winds.
#'
#' @return A dataframe with date (\code{date}) and modeled wind speed
#'    (\code{windspeed}, in m / s) at the grid point location for all storm
#'    observations.
#'
#' @examples \dontrun{
#' data("floyd_tracks")
#' data("county_points")
#' full_track <- create_full_track(hurr_track = floyd_tracks)
#' with_wind_radii <- add_wind_radii(full_track = full_track)
#' wind_grid <- calc_grid_wind(grid_point = county_points[1, ],
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
                           with_wind_radii = add_wind_radii()){

        grid_wind <- dplyr::mutate(with_wind_radii,
                      # Calculated distance from storm center to location
                      cdist = latlon_to_km(.data$tclat, .data$tclon,
                                           # Need the following to have the right type for Rcpp function latlon_to_km
                                           rep(grid_point$glat,
                                               times = nrow(with_wind_radii)),
                                           rep(grid_point$glon,
                                               times = nrow(with_wind_radii)))) %>%
          dplyr::mutate(# Calculate gradient winds at the point
                      wind_gl_aa = mapply(will1, cdist = .data$cdist,
                                          Rmax = .data$Rmax,
                                          R1 = .data$R1, R2 = .data$R2,
                                          vmax_gl = .data$vmax_gl,
                                          n = .data$n, A = .data$A,
                                          X1 = .data$X1),
                      # calculate the gradient wind direction (gwd) at this
                      # grid point
                      gwd = calc_gwd(tclat = .data$tclat, tclon = .data$tclon,
                                     glat = grid_point$glat,
                                     glon = grid_point$glon),
                      # Bring back to surface level (surface wind reduction factor)
                      wind_sfc_sym = mapply(gradient_to_surface,
                                            wind_gl_aa = .data$wind_gl_aa,
                                            cdist = .data$cdist),
                      # Get surface wind direction
                      swd = mapply(add_inflow, gwd = .data$gwd,
                                     cdist = .data$cdist,
                                     Rmax = .data$Rmax, tclat = .data$tclat),
                      # Add back in storm forward motion component
                      windspeed = add_forward_speed(wind_sfc_sym,
                                                      .data$tcspd_u, .data$tcspd_v,
                                                      .data$swd, .data$cdist,
                                                      .data$Rmax)) %>%
          dplyr::select(c(date, windspeed))
        return(grid_wind)
}

#' Generate wind summaries for grid point
#'
#' Summarizes the wind time series for a single grid point, as
#' created by \code{\link{calc_grid_wind}}.
#'
#' @param grid_wind A dataframe with a time series of modeled wind speeds at
#'    a location, as created by \code{\link{calc_grid_wind}}.
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
#' @return Returns a one-row matrix with wind characteristics for a single
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
summarize_grid_wind <- function(grid_wind, tint = 0.25, gust_duration_cut = 20,
                                sust_duration_cut = 20){
  grid_wind_summary <- grid_wind %>%
    dplyr::mutate(gustspeed = .data$windspeed * 1.49) %>%
    # Determine max of windspeed and duration of wind over 20
    dplyr::summarize(vmax_gust = max(.data$gustspeed, na.rm = TRUE),
                     vmax_sust = max(.data$windspeed, na.rm = TRUE),
                     gust_dur = 60 * sum(.data$gustspeed > gust_duration_cut,
                                         na.rm = TRUE),
                     sust_dur = 60 * sum(.data$windspeed > sust_duration_cut,
                                         na.rm = TRUE)) %>%
    dplyr::mutate(gust_dur = .data$gust_dur * tint,
                  sust_dur = .data$sust_dur * tint)
  grid_wind_summary <- as.matrix(grid_wind_summary)
  return(grid_wind_summary)
}

#' Calculate and summarize grid winds
#'
#' This function combines the \code{\link{calc_grid_wind}} and
#' \code{\link{summarize_grid_wind}} functions so they can be run jointly in
#' the overall \code{\link{get_grid_winds}} function. This function calculates
#' wind characteristics at just one location. Within the package, the function
#' used within another function (\code{\link{get_grid_winds}}) to model wind speeds at
#' all grid locations.
#'
#' @inheritParams calc_grid_wind
#' @inheritParams summarize_grid_wind
#'
#' @return Returns a one-row matrix with wind characteristics for a single
#'    location. The wind characteristics given are:
#'    \itemize{
#'      \item{\code{vmax_gust}: Maximum value of surface-level (10 meters)
#'        gust winds, in meters per second, over the length of the
#'        storm at the given location}
#'      \item{\code{vmax_sust}: Maximum value of surface-level (10 meters)
#'        sustained winds, in meters per second, over the length of the
#'        storm at the given location}
#'      \item{\code{gust_dur}: Length of time, in minutes, that
#'        surface-level gust winds were above a specified speed (default is
#'        20 m / s)}
#'      \item{\code{sust_dur}: Length of time, in minutes, that
#'        surface-level sustained winds were above a specified speed (default is
#'        20 m / s)}
#'    }
#'
#' @examples \dontrun{
#' library(dplyr)
#' data(county_points)
#' data("floyd_tracks")
#' full_track <- create_full_track(hurr_track = floyd_tracks, tint = 0.25)
#' with_wind_radii <- add_wind_radii(full_track = full_track)
#' grid_point <- county_points %>% filter(gridid == "37055")
#' grid_wind_summary <- calc_and_summarize_grid_wind(grid_point = grid_point,
#'    with_wind_radii = with_wind_radii, gust_duration_cut = 15,
#'    sust_duration_cut = 15)
#' }
#'
#' @export
calc_and_summarize_grid_wind <- function(grid_point = stormwindmodel::county_points[1, ],
                                         with_wind_radii = add_wind_radii(),
                                         tint = 0.25, gust_duration_cut = 20,
                                         sust_duration_cut = 20){
  grid_wind <- calc_grid_wind(grid_point = grid_point,
                              with_wind_radii = with_wind_radii)
  grid_wind_summary <- summarize_grid_wind(grid_wind = grid_wind, tint = tint,
                                           gust_duration_cut = gust_duration_cut,
                                           sust_duration_cut = sust_duration_cut)
  return(grid_wind_summary)

}

#' Determine hurricane winds at locations
#'
#' This function inputs a storm track and a dataset of locations and calculates
#' highest wind speeds (sustained and maximum) and duration of winds above a
#' certain speed at each location. The dataset of locations can
#' either be a regularly-spaced grid or can be the central points of locations,
#' like counties or cities. For counties in the eastern half of the United
#' States, the \code{county_points} dataset that comes with the package can
#' be used as the \code{grid_point} input.
#'
#' @inheritParams create_full_track
#' @inheritParams calc_grid_wind
#' @inheritParams summarize_grid_wind
#' @param grid_df A dataframe of locations at which to calculate wind characteristics.
#'    This dataframe must include columns for latitude and longitude for each
#'    point, and these columns must be named \code{glat} and \code{glon}.
#'    The latitudes and longitudes should be in decimal degrees, with longitudes in the
#'    Western hemisphere (so, almost all those for Atlantic basin storms)
#'    expressed as negative values.
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
                           sust_duration_cut = 20){
        full_track <- create_full_track(hurr_track = hurr_track, tint = tint)
        with_wind_radii <- add_wind_radii(full_track = full_track)

        grid_winds <- plyr::adply(grid_df, 1, calc_and_summarize_grid_wind,
                                  with_wind_radii = with_wind_radii,
                                  tint = tint,
                                  gust_duration_cut = gust_duration_cut,
                                  sust_duration_cut = sust_duration_cut)

        return(grid_winds)
}

