#' Calculate Willoughby parameters for the storm
#'
#' There are a number of parameters needed for the Willoughby model for each
#' observation time for a storm. This function intakes the storm track for
#' the storm, interpolated first if you want, and calculates these parameters
#' for each observation in the storm tracks, based on the latitude and longitude
#' of the storm at that observation, its forward speed and direction, and
#' its maximum windspeed.These parameters are later used to model wind speed
#' at each grid point for every observation.
#'
#' @param full_track An interpolated version of the hurricane track data, as
#'    created by \code{create_full_track}. This dataframe must have the
#'    following columns:
#'    \itemize{
#'      \item{date: Date-time, in POSIXct format and UTC time zone}
#'      \item{phi: Latitude, in decimal degrees}
#'      \item{lon: Longitude, in decimal degrees and expresses as positive
#'            values (this model assumes all longitudes will be in the
#'            Western hemisphere)}
#'      \item{sustained_Vmax: Sustained maximum wind speed, in meters per
#'           second}
#'    }
#'
#' @return The input dataset, but with columns added for the Willoughby
#'    parameters for every observations. Columns added include:
#'    \itemize{
#'      \item{forward_speed: The forward (translational) speed of the storm,
#'        in meters per second}
#'      \item{mda: The bearing of the storm, in degrees, with 0 degrees
#'        indicating the storm is moving due east, 90 degrees indicating the
#'        storm is moving due north, etc.}
#'      \item{Vmax: The gradient wind speed, based on the best track's
#'        sustained surface wind speed}
#'      \item{Rmax: The radius to maximum winds, in kilometers}
#'      \item{X1, n, A, eq3_right, xi, R1, R2: Different parameters needed for
#'        the Willoughby model}
#'    }
#'
#' @examples
#' data("hurr_tracks", package = "hurricaneexposuredata")
#' example_track <- subset(hurr_tracks, storm_id == "Floyd-1999")
#' full_track <- create_full_track(hurr_track = example_track,
#'                                 tint = 0.25)
#' with_wind_radii <- add_wind_radii(full_track = full_track)
#'
#' @export
add_wind_radii <- function(full_track = create_full_track()){

        with_wind_radii <- dplyr::mutate(full_track,
                                  lon2km = 111.32 * cos(3.14 / 180.0 * phi),
                                  lat2km = 110.54,
                                  dx = lon2km * (lead(-lon) - lag(-lon)),
                                  dy = lat2km * (lead(phi) - lag(phi)),
                                  tint = 0.25,
                                  c_x = (1000 * dx)/(2.0 * tint * 3600),
                                  c_y = (1000 * dy)/(2.0 * tint * 3600),
                                  cspeed = sqrt(dx * dx + dy * dy),
                                  forward_speed = calc_forward_speed(phi, lon,
                                                                     date,
                                                                     lead(phi),
                                                                     lead(lon),
                                                                     lead(date)),
                                  mda = calc_bearing(phi, lon,
                                                     lead(phi), lead(lon)),
                                  mda2 = calcangle(dx, dy),
                              Vmax = calc_gradient_speed(sustained_Vmax),
                              Vmax = remove_forward_speed(Vmax,
                                                          lag(phi), lag(lon),
                                                          lag(date),
                                                          lead(phi), lead(lon),
                                                          lead(date)),
                              Rmax = will7a(Vmax, phi),
                              X1 = will10a(Vmax, phi),
                              n = will10b(Vmax, phi),
                              A = will10c(Vmax, phi),
                              eq3_right = will3_right(n, A, X1, Rmax),
                              xi = mapply(solve_for_xi, eq3_right = eq3_right),
                              R1 = calc_R1(Rmax, xi),
                              R2 = ifelse(Rmax > 20, R1 + 25, R1 + 15)
                              )
        return(with_wind_radii)
}

#' Add back in wind component due to storm motion
#'
#' @note Only do this where windspd > 0 m/s. From NOAA Technical Report 23,
#'    Schwerdt et al., pg. 25
#'
#' @return Numeric vector with wind speed at grid location, with wind
#'    component due to storm motion added back in
add_storm_motion_wind <- function(windspd, swd, mda, forward_speed){
        if(is.na(windspd) || is.na(swd) || is.na(mda) || is.na(forward_speed)){
                return(NA)
        } else {
                if(windspd >= 0){
                  beta <- swd - mda
                  A <- 1.5 * (forward_speed ^ 0.63) * (0.514751 ^ 0.37) *
                    cos(beta * pi / 180)
                  windspd <- windspd + A
                  } else{
                    windspd <- 0
                    }
          return(windspd)
        }
}

#' Calculate wind speed at grid points
#'
#' This function uses the Willoughby wind model to calculate wind speed at
#' a grid point location. This function calculates wind characteristics at
#' just one location, and then it is applied to all grid locations in a higher-
#' level function (\code{get_grid_winds}).
#'
#' @param with_wind_radii A dataframe with interpolated tracks for a storm,
#'    including wind radii, as created by \code{add_wind_radii}.
#' @param grid_point A one-row dataframe with the grid id, latitude, longitude,
#'    and population for a single grid point of the projection grid.
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
#'      \item{\code{max_gust}: Maximum value of surface-level (10 meters)
#'        sustained winds, in meters per second, over the length of the
#'        storm at the given location}
#'      \item{\code{max_sust}: Maximum value of surface-level (10 meters)
#'        gust winds, in meters per second, over the length of the
#'        storm at the given location}
#'      \item{\code{gust_duration}: Length of time, in minutes, that
#'        surface-level sustained winds were above 20 meters per second}
#'      \item{\code{sust_duration}: Length of time, in minutes, that
#'        surface-level gust winds were above 20 meters per second}
#'    }
#'
#' @examples \dontrun{
#' data("hurr_tracks", package = "hurricaneexposuredata")
#' data("county_points")
#' example_track <- subset(hurr_tracks, storm_id == "Floyd-1999")
#' full_track <- create_full_track(hurr_track = example_track,
#'                                 tint = 0.25)
#' with_wind_radii <- add_wind_radii(full_track = full_track)
#' wind_grid <- calc_grid_wind(grid_point = county_points[1, ],
#'                             with_wind_radii = with_wind_radii)
#' }
#'
#' @export
calc_grid_wind <- function(grid_point = stormwindmodel::county_points[1, ],
                           with_wind_radii = add_wind_radii(),
                           tint = 0.25,
                           gust_duration_cut = 20,
                           sust_duration_cut = 20){

        grid_wind <- mutate(with_wind_radii,
                            lon2km = 111.32 * cos(pi * phi / 180),
                      dx = lon2km * (-lon - grid_point$glon),
                      dy = 110.54 * (grid_point$glat - phi),
                      r2 = sqrt(dx^2 + dy^2),
                      r = latlon_to_meters(phi, -lon,
                                            grid_point$glat,
                                            grid_point$glon),
                      track = mapply(will1, r = r, Rmax = Rmax,
                                     R1 = R1, R2 = R2,
                                     Vmax = Vmax, n = n, A = A, X1 = X1),
                      # calculate the gradient wind direction (gwd) at this
                      # grid point
                      bearing_from_storm = (180 - (calc_bearing(phi, -lon,
                                                              grid_point$glat,
                                                              grid_point$glon))),
                      gwd2 = (180 - calcangle(dx, dy))  %% 360,
                      gwd = (bearing_from_storm) %% 360,
                      # Begin Willoughby model to calculate gradient windspeed
                      # distribution
                      swd = (gwd + 20) %% 360,
                      # Calculate the u and v components of surface wind
                      uwind = 0.9 * cos(swd * pi / 180) * track,
                      vwind = 0.9 * sin(swd * pi / 180) * track,
                      # Calculate total wind speed
                      windspd = sqrt(uwind^2 + vwind^2),
                      windspd = mapply(add_storm_motion_wind,
                                       windspd, swd, mda, forward_speed),
                      # Convert 1-min winds at 10-m to 3-sec gust at surface,
                      sust_windspd = windspd,
                      windspd = windspd * 1.3) %>%
                # Determine max of windspeed and duration of wind over 20
                summarize(max_gust = max(windspd, na.rm = TRUE),
                          max_sust = max(sust_windspd, na.rm = TRUE),
                          gust_duration = tint * 60 *
                            sum(windspd > gust_duration_cut,
                                na.rm = TRUE),
                          sust_duration = tint * 60 *
                            sum(sust_windspd > sust_duration_cut,
                                na.rm = TRUE))
        grid_wind <- as.matrix(grid_wind)
        return(grid_wind)
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
#' @param grid_df A dataframe of locations at which to calculate wind characteristics.
#'    This dataframe must include columns for latitude and longitude for each
#'    point, and these columns must be named "glat" and "glon". The latitudes
#'    and longitudes should be in decimal degrees, with longitudes in the
#'    Western hemisphere (so, almost all those for Atlantic basin storms)
#'    expressed as negative values.
#'
#' @return The dataframe of locations input, with the following columns of wind
#' characteristics added for each location:
#'    \itemize{
#'      \item{\code{max_gust}: Maximum value of surface-level (10 meters)
#'        sustained winds, in meters per second, over the length of the
#'        storm at the given location}
#'      \item{\code{max_sust}: Maximum value of surface-level (10 meters)
#'        gust winds, in meters per second, over the length of the
#'        storm at the given location}
#'      \item{\code{gust_duration}: Length of time, in minutes, that
#'        surface-level sustained winds were above 20 meters per second}
#'      \item{\code{sust_duration}: Length of time, in minutes, that
#'        surface-level gust winds were above 20 meters per second}
#'    }
#'
#' @examples
#' \dontrun{
#' data("hurr_tracks", package = "hurricaneexposuredata")
#' data("county_points")
#' example_track <- subset(hurr_tracks, storm_id == "Floyd-1999")
#' grid_winds <- get_grid_winds(hurr_track = example_track,
#'                              grid_df = county_points,
#'                              tint = 0.25)
#' }
#'
#' @export
get_grid_winds <- function(hurr_track = subset(hurr_tracks,
                                               storm_id == "Floyd-1999"),
                           grid_df = stormwindmodel::county_points,
                           tint = 0.25,
                           gust_duration_cut = 20,
                           sust_duration_cut = 20){
        full_track <- create_full_track(hurr_track = hurr_track, tint = tint)
        with_wind_radii <- add_wind_radii(full_track = full_track)

        grid_winds <- plyr::adply(grid_df, 1, calc_grid_wind,
                                  with_wind_radii = with_wind_radii)

        return(grid_winds)
}

