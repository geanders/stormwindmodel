## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## ----message = FALSE, echo = FALSE--------------------------------------------
#  library(dplyr)

## -----------------------------------------------------------------------------
#  library(stormwindmodel)
#  data("floyd_tracks")
#  head(floyd_tracks)
#  data("katrina_tracks")
#  head(katrina_tracks)

## -----------------------------------------------------------------------------
#  data(county_points)
#  head(county_points)

## ----eval = FALSE, echo = c(1)------------------------------------------------
#  floyd_winds <- get_grid_winds(hurr_track = floyd_tracks, grid_df = county_points)
#  save(floyd_winds, file = "data/floyd_winds.Rdata")

## ----echo = c(2:4)------------------------------------------------------------
#  load("data/floyd_winds.Rdata")
#  floyd_winds %>%
#    dplyr::select(gridid, vmax_gust, vmax_sust, gust_dur, sust_dur) %>%
#    slice(1:6)

## ----fig.width = 8------------------------------------------------------------
#  map_wind(floyd_winds)

## ----eval = FALSE, message = FALSE, warning = FALSE, results = "hide"---------
#  library(tigris)
#  new_orleans <- tracts(state = "LA", county = c("Orleans"),
#                        class = "sp")
#  save(new_orleans, file = "data/new_orleans.Rdata")

## ---- echo = FALSE------------------------------------------------------------
#  load("data/new_orleans.Rdata")

## ----eval = FALSE, message = FALSE, echo = c(1, 2)----------------------------
#  library(rgeos)
#  new_orleans_tract_centers <- gCentroid(new_orleans, byid = TRUE)@coords
#  save(new_orleans_tract_centers, file = "data/new_orleans_tract_centers.Rdata")

## ----echo = c(2)--------------------------------------------------------------
#  load("data/new_orleans_tract_centers.Rdata")
#  head(new_orleans_tract_centers)

## ----eval = FALSE, echo = c(1, 2, 3, 4, 5)------------------------------------
#  new_orleans_tract_centers2 <- new_orleans_tract_centers %>%
#    as_tibble() %>%
#    mutate(gridid = unique(new_orleans@data$TRACTCE)) %>%
#    dplyr::rename(glat = y,
#                  glon = x)
#  save(new_orleans_tract_centers2, file = "data/new_orleans_tract_centers2.Rdata")

## ----echo = c(2)--------------------------------------------------------------
#  load("data/new_orleans_tract_centers2.Rdata")
#  head(new_orleans_tract_centers2)

## ----message = FALSE, warning = FALSE, fig.width = 7--------------------------
#  library(sf)
#  new_orleans <- new_orleans %>%
#    st_as_sf()
#  new_orleans_centers <- new_orleans_tract_centers2 %>%
#    st_as_sf(coords = c("glon", "glat")) %>%
#    st_set_crs(4269)
#  
#  library(ggplot2)
#  ggplot() +
#    geom_sf(data = new_orleans) +
#    geom_sf(data = new_orleans_centers, color = "red", size = 0.6)

## ----eval = FALSE, echo = c(1:2)----------------------------------------------
#  new_orleans_tracts_katrina <- get_grid_winds(hurr_track = katrina_tracks,
#                                               grid_df = new_orleans_tract_centers2)
#  save(new_orleans_tracts_katrina, file = "data/new_orleans_tracts_katrina.Rdata")

## ----echo = c(2)--------------------------------------------------------------
#  load("data/new_orleans_tracts_katrina.Rdata")
#  head(new_orleans_tracts_katrina)

## -----------------------------------------------------------------------------
#  new_orleans <- new_orleans %>%
#    left_join(new_orleans_tracts_katrina, by = c("TRACTCE" = "gridid"))

## ---- fig.width = 7, message = FALSE, warning = FALSE-------------------------
#  library(viridis)
#  ggplot() +
#    geom_sf(data = new_orleans, aes(fill = vmax_sust)) +
#    geom_sf(data = new_orleans_centers, color = "red", size = 0.6) +
#    scale_fill_viridis(name = "Maximum\nsustained\nwinds (m/s)")

## ----warning = FALSE, message = FALSE, fig.width = 6, fig.height = 2.5--------
#  dare_county <- county_points %>% # Get grid point information for Dare County
#    filter(gridid == "37055")
#  
#  with_wind_radii <- floyd_tracks %>%
#    create_full_track() %>% # Interpolate tracks to every 15 minutes
#    add_wind_radii()        # Calculate required inputs for Willoughby wind model
#  
#  dare_winds <- calc_grid_wind(grid_point = dare_county,          # Model winds at one grid point
#                               with_wind_radii = with_wind_radii)
#  
#  ggplot(dare_winds, aes(x = date, y = windspeed)) +
#    geom_line() +
#    xlab("Observation time (UTC)") +
#    ylab("Modeled surface wind (m / s)")

## ----fig.width = 8------------------------------------------------------------
#  floyd_map <- map_wind(floyd_winds)
#  add_storm_track(floyd_tracks, plot_object = floyd_map)

## ----fig.width = 8------------------------------------------------------------
#  map_wind(floyd_winds, value = "vmax_gust", wind_metric = "knots")

## ----fig.width = 8------------------------------------------------------------
#  map_wind(floyd_winds, value = "vmax_sust", wind_metric = "knots", break_point = 34)

