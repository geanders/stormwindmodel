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
#'    created by \code{create_full_track}
#'
#' @return The input dataset, but with columns added for the Willoughby
#'    parameters for every observations.
#'
#' @export
add_wind_radii <- function(full_track = create_full_track()){

        with_wind_radii <- dplyr::mutate(full_track,
                                  lon2km = 111.32*cos(3.14/180.0*(phi)),
                                  lat2km = 110.54,
                                  dx = lon2km*(lead(-lon)-lag(-lon)),
                                  dy = lat2km*(lead(phi)-lag(phi)),
                                  tint = 0.25,
                                  c_x = (1000*dx)/(2.0*tint*3600),
                                  c_y = (1000*dy)/(2.0*tint*3600),
                                  cspeed = sqrt(dx*dx+dy*dy),
                                  forward_speed = calc_forward_speed(lag(phi), lag(lon),
                                                                     lag(date), lead(phi),
                                                                     lead(lon), lead(date)),
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
#' @param with_wind_radii A dataframe with interpolated tracks for a storm,
#'    including wind radii, as created by \code{add_wind_radii}.
#' @param grid_point A one-row dataframe with the grid id, latitude, longitude,
#'    and population for a single grid point of the projection grid.
#'
#' @export
calc_grid_wind <- function(grid_point = tracts[1, ],
                           with_wind_radii = add_wind_radii()){

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
                      # Convert 1-min winds at 10-m to 3-sec gust at surface
                      windspd = windspd * 1.3) %>%
                # Determine max of windspeed and duration of wind over 20
                summarize(maxwindspd = max(windspd, na.rm = TRUE),
                          duration = 15 * sum(windspd > 20, na.rm = TRUE))
        grid_wind <- as.matrix(grid_wind)
        return(grid_wind)
}

#' @inheritParams create_full_track
#'
#' @export
get_grid_winds <- function(hurr_track = subset(hurr_tracks,
                                               storm_id == "Floyd-1999"),
                           grid_df = tracts, tint = 0.25){
        full_track <- create_full_track(hurr_track = hurr_track, tint = tint)
        with_wind_radii <- add_wind_radii(full_track = full_track)

        grid_winds <- plyr::adply(grid_df, 1, calc_grid_wind,
                                  with_wind_radii = with_wind_radii)

        return(grid_winds)
}

