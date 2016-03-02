#' @param hurr_track Dataframe with columns the hurricane track for a single
#'    storm. This must include columns for datetime (year, month, day, hour,
#'    minute), latitude, longitude, and wind speed (in knots)
#' @param tint Interval (in hours) for the estimates
#'
#' @return The function returns an extended version of the track data, with
#'    latitude, longitude, and wind speed linearly interpolated between
#'    observed values. Also, wind speed is converted in this function to m / s.
create_full_track <- function(hurr_track = hurr_tracks[hurr_tracks$storm_id == "Floyd-1999", ],
                              tint = 0.25){
        hurr_track <- select(hurr_track, date, latitude, longitude, wind) %>%
                rename(lonr = longitude,
                       latr = latitude,
                       vmax = wind) %>%
                mutate(date = ymd_hm(date),
                       latr = as.numeric(latr),
                       lonr = -1 * as.numeric(lonr),
                       vmax = 0.51444 * as.numeric(vmax), # Convert from ks to m / s
                       dhr = as.numeric(difftime(lead(date), date, units = "hours")),
                       interval = floor(dhr / tint),
                       dellat = (lead(latr) - latr) / interval,
                       dellon = (lead(lonr) - lonr) / interval,
                       delvmax = (lead(vmax) - vmax) / interval)

        for(i in 1:(nrow(hurr_track) - 1)){
                start_obs <- hurr_track[i, ]
                for(k in 1:hurr_track[i, "interval"]){
                        new_date <- start_obs$date + (k - 1) * dhours(tint)
                        new_lat <- start_obs$latr + (k - 1) * start_obs$dellat
                        new_lon <- start_obs$lonr + (k - 1) * start_obs$dellon
                        new_vmax <- start_obs$vmax + (k - 1) * start_obs$delvmax
                        if(i == 1 && k == 1){
                                track_date <- new_date
                                lat <- new_lat
                                lon <- new_lon
                                vmax <- new_vmax
                        } else {
                                track_date <- c(track_date, new_date)
                                lat <- c(lat, new_lat)
                                lon <- c(lon, new_lon)
                                vmax <- c(vmax, new_vmax)
                        }
                }
        }
        full_track <- data.frame(date = track_date,
                                 lat = lat,
                                 lon = lon,
                                 vmax = vmax)
        return(full_track)
}

#' Calculate motion direction angle from ...
#'
#' Use trig angles, so E=0, N=90, W=180, S=270)
#'
#' @param dpair Numeric vector of length 2 with the values for "dx" and "dy"
#'
#' @return This function returns the motion direction angle ...
calcangle_pair <- function(dx, dy) {
        if(is.na(dx) | is.na(dy)){
                return(NA)
        } else {
                if (dx > 0) {
                        angle <- atan(dy / dx) * 180 / pi
                } else if (dx < 0) {
                        angle <- 180 + atan(dy / dx) * 180 / pi
                } else {
                        if (dy == 0) {
                                angle <- 0
                        } else{
                                angle <- atan2(dy, dx) * 180 / pi
                        }
                }
                if (angle < 0) {
                        angle <- 360 + angle
                }
                else if (angle > 360) {
                        angle <- angle - 360
                }
                return(angle)
        }
}

#' Apply the angle calculation across a vector
#'
#' @param dx Numeric vector with x-component of forward speed, in m / s
#' @param dy Numeric vector with y-component of forward speed, in m / s
#'
#' @return This function returns a vector with wind direction angles.
calcangle <- function(dx, dy){
        calcd_angle <- mapply(calcangle_pair, dx, dy)
        return(calcd_angle)
}

#' @param full_track An interpolated version of the hurricane track data, as
#'    created by \code{create_full_track}
add_wind_radii <- function(full_track = create_full_track()){

        with_wind_radii <- mutate(full_track,
                              # convert 30 minute wind at 10 m to central pressure
                              cp = (262.8 - vmax * 0.8139)/0.23,
                              # convert 1-min sustained wind at 10m to gradient
                              # level wind speed (for use in Holland wind
                              # profile calculation)
                              r_vmax = vmax / 0.9,
                              # calculate the x and y components of forward
                              # speed, in m/s
                              lon2km = 111.32 * cos(lat * pi / 180),
                              dx = lon2km * (lead(lon) - lag(lon)),
                              dy = 110.54 * (lead(lat) - lag(lat)),
                              cspeed = sqrt(dx^2 + dy^2),
                              # Reduce VMAX by forward speed (will be added back
                              # in after calculating wind profile)
                              r_vmax = r_vmax - cspeed,
                              r_vmax = ifelse(r_vmax < 0, 0, r_vmax),
                              # Calculate radii for wind model calculations
                              # Willoughby et al. 2006, Eqn 7a
                              Rmax = 46.4 * exp(-0.0155 * r_vmax + 0.0169 * lat),
                              # Willoughby et al. 2006, Eqn 10c
                              A = 0.0696 + 0.0049 * r_vmax - 0.0064 * lat,
                              A = ifelse(A < 0, 0, A),
                              # Willoughby et al. 2006, Eqn 10b
                              n = 0.4067 + 0.0144 * r_vmax - 0.0038 * lat,
                              # Willoughby et al. 2006, Eqn 10a
                              X1 = 317.1 - 2.026 * r_vmax + 1.915 * lat,
                              # Willoughby et al. 2006, Eqn 3 (RHS)
                              known = n * ((1 - A) * X1 + A * 25) /
                                      (n * ((1 - A) *X1 + A * 25) + Rmax),
                              # calculate motion direction angle
                              mda = calcangle(dx, dy)
                              )

        return(with_wind_radii)
}

#' Holland2 model to calculate gradient windspeed distribution
#'
#' @note Wind speed scaled by 100 - must undo after calculation
calc_track <- function(r, Rmax,  known, r_vmax, n, A, X1){

        if(is.na(Rmax) || is.na(known) || is.na(r_vmax) ||
           is.na(n) || is.na(A) || is.na(X1)){
                return(NA)
        } else {

                sigma <- newton2(known)

                if(Rmax > 20){
                        R1 <- Rmax - 25 * sigma
                        R2 <- R1 + 25
                } else {
                        R1 <- Rmax - 15 * sigma
                        R2 <- R1 + 15
                }

                if(r < R1){
                        track <- r_vmax * (r / Rmax)^n * 100
                } else if (r > R2){
                        track <- r_vmax*((1 - A) * exp((Rmax - r) / X1) +
                                                 A * exp((Rmax - r) / 25)) * 100
                } else {
                        eps <- (r - R1) / 25
                        w <- 126 * eps^5 - 420 * eps^6 + 540 * eps^7- 315 * eps^8 +
                                70 * eps^9
                        v_temp1 <- r_vmax * (r / Rmax)^n
                        v_temp2 <- r_vmax * ((1-A) * exp((Rmax - r) / X1) +
                                                     A * exp((Rmax - r) / 25))
                        track <- (v_temp1 * (1 - w) + v_temp2 * w) * 100
                }

                track <- track / 100
                track <- ifelse(track < 0, 0, track)

                return(track)
        }
}

#' Add back in wind component due to storm motion
#'
#' @note Only do this where windspd > 0 m/s. From NOAA Technical Report 23,
#'    Schwerdt et al., pg. 25
#'
#' @return Numeric vector with wind speed at grid location, with wind
#'    component due to storm motion added back in
add_storm_motion_wind <- function(windspd, swd, mda, cspeed){
        if(is.na(windspd) || is.na(swd) || is.na(mda) || is.na(cspeed)){
                return(NA)
        } else {
                if(windspd > 0){
                        beta <- swd - mda
                        windspd <- windspd + 1.5 * (cspeed^0.63)^(0.514751^0.37) *
                                cos(beta * pi / 180)
                        if (windspd < 0){
                                windspd <- 0
                        }
                }
                return(windspd)
        }
}

#' @param with_wind_radii A dataframe with interpolated tracks for a storm,
#'    including wind radii, as created by \code{add_wind_radii}.
#' @param grid_point A one-row dataframe with the grid id, latitude, longitude,
#'    and population for a single grid point of the projection grid.
calc_grid_wind <- function(grid_point = tracts[1, ],
                           with_wind_radii = add_wind_radii()){

        grid_wind <- mutate(with_wind_radii,
                      dx = lon2km * (lon - grid_point$glon),
                      dy = 110.54 * (grid_point$glat - lat),
                      r = sqrt(dx^2 + dy^2),
                      # calculate the gradient wind direction (gwd) at this
                      # grid point
                      gwd = calcangle(dx, dy) - 90,
                      gwd = ifelse(gwd < 0, 360.0 + gwd, gwd - 360.0),
                      # Begin Holland2 model to calculate gradient windspeed
                      # distribution *Note:  Wind speed scaled by 100 - must
                      # undo after calculation
                      track = mapply(calc_track, r, Rmax,  known,
                                     r_vmax, n, A, X1),
                      swd = gwd + 20,
                      swd = ifelse(swd < 0, 360 + swd, swd),
                      swd = ifelse(swd > 360, swd - 360, swd),
                      # Calculate the u and v components of surface wind
                      uwind = 0.9 * cos(swd * pi / 180) * track,
                      vwind = 0.9 * sin(swd * pi / 180) * track,
                      # Calculate total wind speed
                      windspd = sqrt(uwind^2 + vwind^2),
                      windspd = mapply(add_storm_motion_wind,
                                       windspd, swd, mda, cspeed),
                      # Convert 1-min winds at 10-m to 3-sec gust at surface
                      windspd = windspd * 1.3) %>%
                # Determine max of windspeed and duration of wind over 20
                summarize(maxwindspd = max(windspd, na.rm = TRUE),
                          duration = 15 * sum(windspd > 20, na.rm = TRUE))
        grid_wind <- as.matrix(grid_wind)
        return(grid_wind)
}

#' @inheritParams create_full_track
get_grid_winds <- function(hurr_track = hurr.tracks[["Floyd-1999"]],
                           grid_df = tracts, tint = 0.25){
        full_track <- create_full_track(hurr_track = hurr_track, tint = tint)
        with_wind_radii <- add_wind_radii(full_track = full_track)

        grid_winds <- plyr::adply(grid_df, 1, calc_grid_wind,
                                  with_wind_radii = with_wind_radii)

        return(grid_winds)
}

#' Map wind exposure at the county level
map_wind <- function(grid_winds, value = "maxwindspd", break_point = NULL){
        if(!is.null(break_point)){
                cut_values <- cut(grid_winds[ , value],
                                  breaks = c(0, break_point, max(grid_winds[ , value])),
                                  include.lowest = TRUE)
                grid_winds$value <- as.numeric(cut_values)
                num_colors <- 2
        } else {
                grid_winds$value <- grid_winds[ , value]
                num_colors <- 1
        }

        map_data <- mutate(grid_winds,
                           region = as.numeric(gridid)) %>%
                select(region, value)
        out <- choroplethr::county_choropleth(map_data,
                                              num_colors = num_colors,
                                              state_zoom = c("alabama", "arkansas",
                                                             "connecticut", "delaware",
                                                             "district of columbia", "florida",
                                                             "georgia", "illinois", "indiana",
                                                             "iowa", "kansas", "kentucky", "louisiana",
                                                             "maine", "maryland", "massachusetts",
                                                             "michigan", "mississippi",
                                                             "missouri", "new hampshire", "new jersey",
                                                             "new york", "north carolina", "ohio",
                                                             "oklahoma", "pennsylvania", "rhode island",
                                                             "south carolina", "tennessee", "texas",
                                                             "vermont", "virginia", "west virginia",
                                                             "wisconsin"))
        if(!is.null(break_point)){
                out <- out + scale_fill_manual(values = c("white", "blue"),
                                               labels = levels(cut_values))
        } else{
                out <- out +
                        scale_fill_gradient(low = "white", high = "red") +
                        scale_color_gradient(low = "white", high = "red")
        }
        return(out)
}
