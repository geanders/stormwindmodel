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
          dplyr::mutate_(tcspd = ~ calc_forward_speed(tclat, tclon, date,
                                                      dplyr::lead(tclat),
                                                      dplyr::lead(tclon),
                                                      dplyr::lead(date)),
                        tcdir = ~ calc_bearing(tclat, tclon,
                                               dplyr::lead(tclat),
                                               dplyr::lead(tclon)),
                        tcspd_u = ~ tcspd * cos(degrees_to_radians(tcdir)),
                        tcspd_v = ~ tcspd * sin(degrees_to_radians(tcdir)),
                        vmax_sfc_sym = ~ remove_forward_speed(vmax, tcspd),
                        over_land = ~ mapply(check_over_land, tclat, tclon),
                        vmax_gl = ~ mapply(calc_gradient_speed,
                                           vmax_sfc_sym = vmax_sfc_sym,
                                           over_land = over_land),
                        Rmax = ~ will7a(vmax_gl, tclat),
                        X1 = ~ will10a(vmax_gl, tclat),
                        n = ~ will10b(vmax_gl, tclat),
                        A = ~ will10c(vmax_gl, tclat),
                        eq3_right = ~ will3_right(n, A, X1, Rmax),
                        xi = ~ mapply(solve_for_xi, eq3_right = eq3_right),
                        R1 = ~ calc_R1(Rmax, xi),
                        R2 = ~ ifelse(Rmax > 20, R1 + 25, R1 + 15)
                        ) %>%
          dplyr::select_(quote(-vmax), quote(-tcspd), quote(-vmax_sfc_sym),
                         quote(-over_land), quote(-eq3_right), quote(-xi))
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

        grid_wind <- dplyr::mutate_(with_wind_radii,
                      # Calculated distance from storm center to location
                      cdist = ~ latlon_to_km(tclat, tclon,
                                             grid_point$glat, grid_point$glon),
                      # Calculate gradient winds at the point
                      wind_gl_aa = ~ mapply(will1, cdist = cdist, Rmax = Rmax,
                                            R1 = R1, R2 = R2, vmax_gl = vmax_gl,
                                            n = n, A = A, X1 = X1),
                      # calculate the gradient wind direction (gwd) at this
                      # grid point
                      chead = ~ calc_bearing(tclat, tclon,
                                             grid_point$glat, grid_point$glon),
                      gwd = ~ (90 + chead) %% 360,
                      # Bring back to surface level (surface wind reduction factor)
                      wind_sfc_sym = ~ mapply(gradient_to_surface,
                                              wind_gl_aa = wind_gl_aa,
                                              cdist = cdist),
                      # Get surface wind direction
                      swd = ~ mapply(add_inflow, gwd = gwd, cdist = cdist,
                                     Rmax = Rmax),
                      # Add back in storm forward motion component
                      windspeed = ~ add_forward_speed(wind_sfc_sym,
                                                      tcspd_u, tcspd_v,
                                                      swd, cdist, Rmax)) %>%
          dplyr::select_(~ date, ~ windspeed)
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
    dplyr::mutate_(gustspeed = ~ windspeed * 1.49) %>%
    # Determine max of windspeed and duration of wind over 20
    dplyr::summarize_(vmax_gust = ~ max(gustspeed, na.rm = TRUE),
                      vmax_sust = ~ max(windspeed, na.rm = TRUE),
                      gust_dur = ~ 60 * sum(gustspeed > gust_duration_cut,
                                            na.rm = TRUE),
                      sust_dur = ~ 60 * sum(windspeed > sust_duration_cut,
                                            na.rm = TRUE)) %>%
    dplyr::mutate_(gust_dur = ~ gust_dur * tint,
                  sust_dur = ~ sust_dur * tint)
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

