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

        with_wind_radii <- full_track %>%
          dplyr::mutate(forward_speed = calc_forward_speed(phi, lon, date,
                                                           lead(phi), lead(lon),
                                                           lead(date)),
                        mda = calc_bearing(phi, lon, lead(phi), lead(lon)),
                        forward_speed_u = forward_speed * cos(degrees_to_radians(mda)),
                        forward_speed_v = forward_speed * sin(degrees_to_radians(mda)),
                        sustained_Vmax = remove_forward_speed(sustained_Vmax,
                                                              forward_speed),
                        over_land = TRUE,
                        #over_land = mapply(check_over_land, phi, lon),
                        Vmax = mapply(calc_gradient_speed,
                                      sustained_vmax = sustained_Vmax,
                                      over_land = over_land),
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
#' @references
#'
#' NOAA Technical Report NWS 23, Schwerdt and Watkins, 1979.
#'
#' @export
calc_grid_wind <- function(grid_point = stormwindmodel::county_points[1, ],
                           with_wind_radii = add_wind_radii(),
                           tint = 0.25,
                           gust_duration_cut = 20,
                           sust_duration_cut = 20){

        grid_wind <- mutate(with_wind_radii,
                      # Calculated distance from storm center to location
                      r = latlon_to_km(phi, lon, grid_point$glat,
                                       -grid_point$glon),
                      # Calculate rotational windspeed at the point
                      track = mapply(will1, r = r, Rmax = Rmax,
                                     R1 = R1, R2 = R2,
                                     Vmax = Vmax, n = n, A = A, X1 = X1),
                      # calculate the gradient wind direction (gwd) at this
                      # grid point
                      bearing_from_storm = calc_bearing(phi, lon,
                                                        grid_point$glat,
                                                        - grid_point$glon),
                      gwd = (90 + bearing_from_storm) %% 360,
                      # Bring back to surface level (surface wind reduction factor)
                      windspd = mapply(gradient_to_surface, track = track, r = r),
                      # Get surface wind direction
                      swd = mapply(add_inflow, gwd = gwd, r = r, Rmax = Rmax),
                      # Add back in storm forward motion component
                      windspd = add_forward_speed(windspd,
                                                  forward_speed_u,
                                                  forward_speed_v,
                                                  swd, r, Rmax),
                      # Convert 1-min winds at 10-m to 3-sec gust at surface,
                      sust_windspd = windspd,
                      gust_windspd = windspd * 1.49) %>%
                # Determine max of windspeed and duration of wind over 20
                dplyr::summarize(max_gust = max(gust_windspd, na.rm = TRUE),
                          max_sust = max(sust_windspd, na.rm = TRUE),
                          gust_duration = 60 *
                            sum(gust_windspd > gust_duration_cut,
                                na.rm = TRUE),
                          sust_duration = 60 *
                            sum(sust_windspd > sust_duration_cut,
                                na.rm = TRUE)) %>%
          dplyr::mutate(gust_duration = gust_duration * tint,
                        sust_duration = sust_duration * tint)
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

