## ----echo = FALSE, message = FALSE--------------------------------------------
library(stormwindmodel)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

## ----echo = FALSE-------------------------------------------------------------
var_names <- dplyr::tibble(var = c("`vmax`",
                                       "`vmax_sfc_sym`",
                                       "`vmax_gl`",
                                       "`tclat`",
                                       "`tclon`",
                                       "`Rearth`",
                                       "`tcspd`",
                                       "`tcspd_u`", 
                                       "`tcspd_v`",
                                       "`tcdir`",
                                       "`X1`",
                                       "`X2`",
                                       "`n`",
                                       "`A`",
                                       "`xi`",
                                       "`R1`",
                                       "`R2`",
                                       "`gwd`",
                                       "`beta`",
                                       "`swd`",
                                       "`Vi`",
                                       "`V0`",
                                       "`wind_gl_aa`",
                                       "`wind_gl`",
                                       "`cdist`",
                                       "`chead`",
                                       "`wind_sfc_sym_u`",
                                       "`wind_sfc_sym_v`",
                                       "`wind_sfc_u`",
                                       "`wind_sfc_v`",
                                       "`r`",
                                       "`Rmax`",
                                       "`reduction_factor`",
                                       "`windspeed`",
                                       "`gustspeed`",
                                       "`vmax_sust`",
                                       "`vmax_gust`",
                                       "`sust_dur`",
                                       "`gust_dur`",
                                       "`gust_factor`"),
                               math = c("$V_{max}$",
                                        "$V_{max,sym}$",
                                        "$V_{max,G}$",
                                        "$\\phi$",
                                        "$L$",
                                        "$R_{earth}$",
                                        "$F$",
                                        "$F_{u}$",
                                        "$F_{v}$",
                                        "$\\theta_{F}$",
                                        "$X_1$",
                                        "$X_2$",
                                        "$n$",
                                        "$A$",
                                        "$\\xi$",
                                        "$R_1$",
                                        "$R_2$",
                                        "$\\theta_{G}$",
                                        "$\\beta$",
                                        "$\\theta_{S}$",
                                        "$V_i$",
                                        "$V_0$",
                                        "$V_G(r)$",
                                        "V_G",
                                        "$C_{dist}$",
                                        "$C_{head}$",
                                        "$V_{sfc,sym,u}$",
                                        "$V_{sfc,sym,v}$",
                                        "$V_{sfc,u}$",
                                        "$V_{sfc,v}$",
                                        "$r$",
                                        "$R_{max}$",
                                        "$f_r$",
                                        "$V_{sfc}$",
                                        "$V_{sfc,gust}$",
                                        "$V_{max,sust}$",
                                        "$V_{max,gust}$",
                                        "$T_{sust}$",
                                        "$T_{gust}$",
                                        "$G_{3,60}$"),
                               defn = c("Maximum 10-m 1-min sustained wind for the tropical cyclone",
                                        "Maximum 10-m 1-min sustained wind for the tropical cyclone with motion asymmetry removed",
                                        "Maximum gradient-level 1-min sustained wind for the tropical cyclone",
                                        "Tropical cyclone center position latitude",
                                        "Tropical cyclone center position longitude (0 to 360)",
                                        "Radius of the earth",
                                        "Tropical cyclone forward speed",
                                        "Tropical cyclone forward speed, u-component",
                                        "Tropical cyclone forward speed, v-component",
                                        "Tropical cyclone forward direction",
                                        "Parameter for Willoughby model",
                                        "Parameter for Willoughby model",
                                        "Parameter for Willoughby model",
                                        "Parameter for Willoughby model",
                                        "Parameter for Willoughby model",
                                        "Lower boundary of the transition zone for Willoughby model",
                                        "Upper boundary of the transition zone for Willoughby model",
                                        "Gradient wind direction",
                                        "Inflow angle (0 to 360)",
                                        "Surface wind direction",
                                        "Azimuthal average winds inside $R_1$",
                                        "Azimuthal average winds outside $R_2$",
                                        "Azimuthal average winds, varies by radius $r$", 
                                        "Gradient level winds at grid point",
                                        "Distance from tropical cyclone to grid point",
                                        "Heading of grid point from tropical cyclone center",
                                        "Symmetric surface winds at grid point, u-component",
                                        "Symmetric surface winds at grid point, v-component",
                                        "Asymmetric surface winds at grid point, u-component",
                                        "Asymmetric surface winds at grid point, v-component",
                                        "Radius from the center of tropical cyclone",
                                        "Radius of maximum winds",
                                        "Reduction factor for converting between surface and gradient winds",
                                        "Asymmetric surface sustained wind at grid point",
                                        "Asymmetric surface wind gust at grid point",
                                        "Max 10-m 1-min sustained wind experienced at grid point",
                                        "Max 10-m 1-min gust wind experienced at grid point",
                                        "Duration of time a certain sustained wind was experienced at grid point",
                                        "Duration of time a certain gust wind was experienced at grid point",
                                        "10-m gust factor to convert from 1-min averaged mean wind to highest 3-sec mean (gust) within a 1-min observation period"),
                               units = c("m/s",
                                         "m/s",
                                         "m/s",
                                         "degrees North",
                                         "degrees East",
                                         "km",
                                         "m/s",
                                         "m/s",
                                         "m/s",
                                         "degrees (trigonomical)",
                                         "--",
                                         "--",
                                         "--",
                                         "--",
                                         "--",
                                         "km",
                                         "km",
                                         "degrees",
                                         "degrees",
                                         "degrees",
                                         "m/s",
                                         "m/s",
                                         "km",
                                         "degrees (trigonomical)",
                                         "m/s",
                                         "m/s",
                                         "m/s",
                                         "m/s",
                                         "m/s",
                                         "m/s",
                                         "km",
                                         "km",
                                         "--",
                                         "m/s",
                                         "m/s",
                                         "m/s",
                                         "m/s",
                                         "minutes",
                                         "minutes",
                                         "--"))
knitr::kable(var_names, col.names = c("R variable", "Expression in equations",
                                      "Definition", "Units"))

## -----------------------------------------------------------------------------
stormwindmodel::create_full_track

## -----------------------------------------------------------------------------
data("floyd_tracks")
full_track <- create_full_track(hurr_track = floyd_tracks, tint = 0.25)
full_track %>% slice(1:3)

## ----results = "hide", warning = FALSE, message = FALSE-----------------------
library(sf)
library(maps)
library(ggplot2)

floyd_states <- sf::st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>% 
  dplyr::filter(ID %in% c("north carolina", "south carolina", "maryland",
                          "georgia", "florida", "virginia", "delaware", 
                          "pennsylvania", "west virginia", "new jersey",
                          "new york"))

## -----------------------------------------------------------------------------
floyd_15_min <- create_full_track(floyd_tracks)
floyd_2_hrs <- create_full_track(floyd_tracks, tint = 2)

## ----message = FALSE, warning = FALSE, fig.align = "center", fig.width = 8, fig.height = 4----
floyd_15_min <- floyd_15_min %>% 
  mutate(tclon = -tclon) %>% 
  st_as_sf(coords = c("tclon", "tclat")) %>% 
  st_set_crs(4326)
floyd_2_hrs <- floyd_2_hrs %>% 
  mutate(tclon = -tclon) %>% 
  st_as_sf(coords = c("tclon", "tclat")) %>% 
  st_set_crs(4326)

a <- ggplot() + 
  geom_sf(data = floyd_states, 
          fill = "aliceblue") + 
  xlim(c(-90, -70)) + 
  ylim(c(24, 46))
b <- a +
  geom_sf(data = floyd_15_min,
          size = 0.5, color = "red") + 
  ggtitle("Interpolated to 15 minutes")
c <- a + 
    geom_sf(data = floyd_2_hrs,
            size = 0.5, color = "red") + 
  ggtitle("Interpolated to 2 hours") 

gridExtra::grid.arrange(b, c, ncol = 2)

## ----eval = c(3:4), echo = c(1, 4)--------------------------------------------
with_wind_radii <- add_wind_radii(full_track = full_track)
save(with_wind_radii, file = "data/with_wind_radii.RData")
load("data/with_wind_radii.RData")
with_wind_radii %>% slice(c(1:3, (n()-3):n()))

## ----echo = FALSE, warning = FALSE, fig.align = "center", fig.width = 4, fig.height = 4, eval = FALSE----
#  a + geom_point(data = with_wind_radii,
#                 aes(x = -tclon, y = tclat, size = Rmax, color = vmax_gl),
#                 alpha = 0.2) +
#    scale_color_continuous(name = expression(V[maxG])) +
#    scale_size_continuous(name = expression(R[max]))

## -----------------------------------------------------------------------------
stormwindmodel::add_wind_radii

## -----------------------------------------------------------------------------
stormwindmodel:::degrees_to_radians
stormwindmodel:::latlon_to_km

## -----------------------------------------------------------------------------
stormwindmodel:::calc_forward_speed

## -----------------------------------------------------------------------------
stormwindmodel::calc_bearing

## ----eval = FALSE-------------------------------------------------------------
#  tcspd_u = tcspd * cos(degrees_to_radians(tcdir))
#  tcspd_v = tcspd * sin(degrees_to_radians(tcdir))

## -----------------------------------------------------------------------------
stormwindmodel:::remove_forward_speed

## -----------------------------------------------------------------------------
stormwindmodel::calc_gradient_speed

## -----------------------------------------------------------------------------
data(landmask)
head(landmask)

## -----------------------------------------------------------------------------
stormwindmodel:::check_over_land

## ----fig.width = 8, fig.height = 5--------------------------------------------
floyd_tracks$land <- mapply(stormwindmodel:::check_over_land,
                            tclat = floyd_tracks$latitude,
                            tclon = -floyd_tracks$longitude)
ggplot(landmask, aes(x = longitude - 360, y = latitude, color = land)) +
  geom_point() + 
  geom_point(data = floyd_tracks, aes(x = longitude, y = latitude, 
                                     color = NULL, shape = land)) + 
  scale_color_discrete("Land mask") + 
  scale_shape_discrete("Track over land")

## ----eval = FALSE-------------------------------------------------------------
#  over_land = mapply(check_over_land, tclat, tclon),
#  vmax_gl = mapply(calc_gradient_speed,
#                  vmax_sfc_sym = vmax_sfc_sym,
#                  over_land = over_land)

## -----------------------------------------------------------------------------
stormwindmodel::will7a

## -----------------------------------------------------------------------------
stormwindmodel::will10a

## -----------------------------------------------------------------------------
stormwindmodel::will10b

## -----------------------------------------------------------------------------
stormwindmodel::will10c

## -----------------------------------------------------------------------------
stormwindmodel::will3_right
stormwindmodel::will3_deriv_func
stormwindmodel::solve_for_xi

## -----------------------------------------------------------------------------
stormwindmodel::calc_R1

## ----eval = FALSE-------------------------------------------------------------
#  R2 = ifelse(Rmax > 20, R1 + 25, R1 + 15)

## -----------------------------------------------------------------------------
stormwindmodel::calc_grid_wind

## ----echo = FALSE-------------------------------------------------------------
data(county_points)
county_points[1, ]

## -----------------------------------------------------------------------------
grid_point <- county_points %>% filter(gridid == "37055")
grid_wind <- calc_grid_wind(grid_point = grid_point,
                            with_wind_radii = with_wind_radii)
grid_wind %>% slice(1:5)

## ----warning = FALSE, fig.width = 6, fig.align = "center"---------------------
ggplot(grid_wind, aes(x = date, y = windspeed)) + 
  geom_line() + 
  xlab("Observation time (UTC)") + 
  ylab("Modeled surface wind (m / s)")

## ----fig.width = 8------------------------------------------------------------
county_list <- split(county_points, f = county_points$gridid)
county_winds <- lapply(county_list, FUN = calc_grid_wind,
                       with_wind_radii = with_wind_radii)
names(county_winds) <- county_points$gridid
county_winds <- bind_rows(county_winds, .id = "gridid")

county_winds %>%
  filter(date == "1999-09-16 08:00:00 UTC") %>%
  map_wind(value = "windspeed")

## -----------------------------------------------------------------------------
stormwindmodel::summarize_grid_wind

## -----------------------------------------------------------------------------
summarize_grid_wind(grid_wind = grid_wind)

## -----------------------------------------------------------------------------
summarize_grid_wind(grid_wind = grid_wind, gust_duration_cut = 15, 
                    sust_duration_cut = 15)

## -----------------------------------------------------------------------------
calc_and_summarize_grid_wind(grid_point = grid_point, 
                             with_wind_radii = with_wind_radii,
                             gust_duration_cut = 15, 
                             sust_duration_cut = 15)

## -----------------------------------------------------------------------------
stormwindmodel:::will1

## -----------------------------------------------------------------------------
stormwindmodel:::gradient_to_surface

## ---- fig.align = "center", fig.width = 4, fig.height = 2.5-------------------
rf_example <- data.frame(r = 0:800,
                         rf = mapply(
                           stormwindmodel:::gradient_to_surface, 
                           wind_gl_aa = 1, cdist = 0:800))
ggplot(rf_example, aes(x = r, y = rf)) + 
  geom_line() + 
  theme_classic() + 
  xlab("Radius (km)") + 
  ylab("Reduction factor") + 
  ylim(c(0.5, 0.9))

## -----------------------------------------------------------------------------
stormwindmodel:::calc_bearing

## ----eval = FALSE-------------------------------------------------------------
#  gwd = (90 + chead) %% 360

## -----------------------------------------------------------------------------
add_inflow

## -----------------------------------------------------------------------------
stormwindmodel:::add_forward_speed

## ----echo = FALSE-------------------------------------------------------------
gust_factors <- data.frame(loc = c("In-land", 
                                   "Just offshore",
                                   "Just onshore",
                                   "At sea"),
                           gust_factor = c(1.49, 1.36, 1.23, 1.11))
knitr::kable(gust_factors, col.names = c("Location", "Gust factor ($G_{3,60}$)"))

## ----eval = FALSE-------------------------------------------------------------
#  gustspeed = windspeed * 1.49

## ---- eval = FALSE------------------------------------------------------------
#  data("katrina_tracks")
#  grid_winds_katrina <- get_grid_winds(hurr_track = katrina_tracks,
#                                       grid_df = county_points)

## ----echo = c(3), eval = c(2:3)-----------------------------------------------
save(grid_winds_katrina, file = "data/grid_winds_katrina.Rdata")
load("data/grid_winds_katrina.Rdata")
head(grid_winds_katrina)

## -----------------------------------------------------------------------------
stormwindmodel::get_grid_winds

## ----eval = FALSE-------------------------------------------------------------
#  grid_winds_katrina <- get_grid_winds(hurr_track = katrina_tracks,
#                                       grid_df = county_points)

## ----fig.width = 7, fig.height = 4, warning = FALSE, message = FALSE----------
katrina_winds <- map_wind(grid_winds_katrina, value = "vmax_gust") + 
  ggtitle("Maximum gust wind speeds")
add_storm_track(katrina_tracks, plot_object = katrina_winds)

## ----fig.width = 7, fig.height = 4, warning = FALSE, message = FALSE----------
katrina_winds <- map_wind(grid_winds_katrina, value = "vmax_sust") + 
  ggtitle("Maximum sustained wind speeds")
add_storm_track(katrina_tracks, plot_object = katrina_winds)

## ----fig.width = 7, fig.height = 4, warning = FALSE, message = FALSE----------
# Show in knots
katrina_winds <- map_wind(grid_winds_katrina, value = "vmax_gust",
              wind_metric = "knots") + 
  ggtitle("Maximum gust wind speeds")
add_storm_track(katrina_tracks, plot_object = katrina_winds)

## ----fig.width = 7, fig.height = 4, warning = FALSE, message = FALSE----------
katrina_winds <- map_wind(grid_winds_katrina, value = "vmax_sust",
              wind_metric = "knots") + 
  ggtitle("Maximum sustained wind speeds")
add_storm_track(katrina_tracks, plot_object = katrina_winds)

## ----fig.width = 7, fig.height = 4, warning = FALSE, message = FALSE----------
# Sustained winds of 20 m / s or more
katrina_winds <- map_wind(grid_winds_katrina, value = "vmax_sust", 
         break_point = 20)
add_storm_track(katrina_tracks, plot_object = katrina_winds)

## ----fig.width = 7, fig.height = 4, warning = FALSE, message = FALSE----------
# Sustained winds of 34 knots or more
katrina_winds <- map_wind(grid_winds_katrina, value = "vmax_sust",
                          wind_metric = "knots", break_point = 34)
add_storm_track(katrina_tracks, plot_object = katrina_winds)

## ----fig.width = 7, fig.height = 4, warning = FALSE, message = FALSE----------
# Sustained winds of 50 knots or more
katrina_winds <- map_wind(grid_winds_katrina, value = "vmax_sust",
                          wind_metric = "knots", break_point = 50)
add_storm_track(katrina_tracks, plot_object = katrina_winds)

## ----fig.width = 7, fig.height = 4, warning = FALSE, message = FALSE----------
# Sustained winds of 64 knots or more
katrina_winds <- map_wind(grid_winds_katrina, value = "vmax_sust",
                          wind_metric = "knots",  break_point = 64)
add_storm_track(katrina_tracks, plot_object = katrina_winds)

