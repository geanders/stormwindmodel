## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## ----echo = FALSE, message = FALSE--------------------------------------------
#  library(stormwindmodel)
#  library(tidyverse)
#  library(gridExtra)

## ----echo = FALSE-------------------------------------------------------------
#  var_names <- dplyr::tibble(var = c("`vmax`",
#                                         "`vmax_sfc_sym`",
#                                         "`vmax_gl`",
#                                         "`tclat`",
#                                         "`tclon`",
#                                         "`Rearth`",
#                                         "`tcspd`",
#                                         "`tcspd_u`",
#                                         "`tcspd_v`",
#                                         "`tcdir`",
#                                         "`X1`",
#                                         "`X2`",
#                                         "`n`",
#                                         "`A`",
#                                         "`xi`",
#                                         "`R1`",
#                                         "`R2`",
#                                         "`gwd`",
#                                         "`beta`",
#                                         "`swd`",
#                                         "`Vi`",
#                                         "`V0`",
#                                         "`wind_gl_aa`",
#                                         "`wind_gl`",
#                                         "`cdist`",
#                                         "`chead`",
#                                         "`wind_sfc_sym_u`",
#                                         "`wind_sfc_sym_v`",
#                                         "`wind_sfc_u`",
#                                         "`wind_sfc_v`",
#                                         "`r`",
#                                         "`Rmax`",
#                                         "`reduction_factor`",
#                                         "`windspeed`",
#                                         "`gustspeed`",
#                                         "`vmax_sust`",
#                                         "`vmax_gust`",
#                                         "`sust_dur`",
#                                         "`gust_dur`",
#                                         "`gust_factor`"),
#                                 math = c("$V_{max}$",
#                                          "$V_{max,sym}$",
#                                          "$V_{max,G}$",
#                                          "$\\phi$",
#                                          "$L$",
#                                          "$R_{earth}$",
#                                          "$F$",
#                                          "$F_{u}$",
#                                          "$F_{v}$",
#                                          "$\\theta_{F}$",
#                                          "$X_1$",
#                                          "$X_2$",
#                                          "$n$",
#                                          "$A$",
#                                          "$\\xi$",
#                                          "$R_1$",
#                                          "$R_2$",
#                                          "$\\theta_{G}$",
#                                          "$\\beta$",
#                                          "$\\theta_{S}$",
#                                          "$V_i$",
#                                          "$V_0$",
#                                          "$V_G(r)$",
#                                          "V_G",
#                                          "$C_{dist}$",
#                                          "$C_{head}$",
#                                          "$V_{sfc,sym,u}$",
#                                          "$V_{sfc,sym,v}$",
#                                          "$V_{sfc,u}$",
#                                          "$V_{sfc,v}$",
#                                          "$r$",
#                                          "$R_{max}$",
#                                          "$f_r$",
#                                          "$V_{sfc}$",
#                                          "$V_{sfc,gust}$",
#                                          "$V_{max,sust}$",
#                                          "$V_{max,gust}$",
#                                          "$T_{sust}$",
#                                          "$T_{gust}$",
#                                          "$G_{3,60}$"),
#                                 defn = c("Maximum 10-m 1-min sustained wind for the tropical cyclone",
#                                          "Maximum 10-m 1-min sustained wind for the tropical cyclone with motion asymmetry removed",
#                                          "Maximum gradient-level 1-min sustained wind for the tropical cyclone",
#                                          "Tropical cyclone center position latitude",
#                                          "Tropical cyclone center position longitude (0 to 360)",
#                                          "Radius of the earth",
#                                          "Tropical cyclone forward speed",
#                                          "Tropical cyclone forward speed, u-component",
#                                          "Tropical cyclone forward speed, v-component",
#                                          "Tropical cyclone forward direction",
#                                          "Parameter for Willoughby model",
#                                          "Parameter for Willoughby model",
#                                          "Parameter for Willoughby model",
#                                          "Parameter for Willoughby model",
#                                          "Parameter for Willoughby model",
#                                          "Lower boundary of the transition zone for Willoughby model",
#                                          "Upper boundary of the transition zone for Willoughby model",
#                                          "Gradient wind direction",
#                                          "Inflow angle (0 to 360)",
#                                          "Surface wind direction",
#                                          "Azimuthal average winds inside $R_1$",
#                                          "Azimuthal average winds outside $R_2$",
#                                          "Azimuthal average winds, varies by radius $r$",
#                                          "Gradient level winds at grid point",
#                                          "Distance from tropical cyclone to grid point",
#                                          "Heading of grid point from tropical cyclone center",
#                                          "Symmetric surface winds at grid point, u-component",
#                                          "Symmetric surface winds at grid point, v-component",
#                                          "Asymmetric surface winds at grid point, u-component",
#                                          "Asymmetric surface winds at grid point, v-component",
#                                          "Radius from the center of tropical cyclone",
#                                          "Radius of maximum winds",
#                                          "Reduction factor for converting between surface and gradient winds",
#                                          "Asymmetric surface sustained wind at grid point",
#                                          "Asymmetric surface wind gust at grid point",
#                                          "Max 10-m 1-min sustained wind experienced at grid point",
#                                          "Max 10-m 1-min gust wind experienced at grid point",
#                                          "Duration of time a certain sustained wind was experienced at grid point",
#                                          "Duration of time a certain gust wind was experienced at grid point",
#                                          "10-m gust factor to convert from 1-min averaged mean wind to highest 3-sec mean (gust) within a 1-min observation period"),
#                                 units = c("m/s",
#                                           "m/s",
#                                           "m/s",
#                                           "degrees North",
#                                           "degrees East",
#                                           "km",
#                                           "m/s",
#                                           "m/s",
#                                           "m/s",
#                                           "degrees (trigonomical)",
#                                           "--",
#                                           "--",
#                                           "--",
#                                           "--",
#                                           "--",
#                                           "km",
#                                           "km",
#                                           "degrees",
#                                           "degrees",
#                                           "degrees",
#                                           "m/s",
#                                           "m/s",
#                                           "km",
#                                           "degrees (trigonomical)",
#                                           "m/s",
#                                           "m/s",
#                                           "m/s",
#                                           "m/s",
#                                           "m/s",
#                                           "m/s",
#                                           "km",
#                                           "km",
#                                           "--",
#                                           "m/s",
#                                           "m/s",
#                                           "m/s",
#                                           "m/s",
#                                           "minutes",
#                                           "minutes",
#                                           "--"))
#  knitr::kable(var_names, col.names = c("R variable", "Expression in equations",
#                                        "Definition", "Units"))

## -----------------------------------------------------------------------------
#  stormwindmodel::create_full_track

## -----------------------------------------------------------------------------
#  data("floyd_tracks")
#  full_track <- create_full_track(hurr_track = floyd_tracks, tint = 0.25)
#  full_track %>% slice(1:3)

## ----results = "hide", warning = FALSE, message = FALSE-----------------------
#  library(sf)
#  library(maps)
#  library(ggplot2)
#  
#  floyd_states <- sf::st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
#    dplyr::filter(ID %in% c("north carolina", "south carolina", "maryland",
#                            "georgia", "florida", "virginia", "delaware",
#                            "pennsylvania", "west virginia", "new jersey",
#                            "new york"))

## -----------------------------------------------------------------------------
#  floyd_15_min <- create_full_track(floyd_tracks)
#  floyd_2_hrs <- create_full_track(floyd_tracks, tint = 2)

## ----message = FALSE, warning = FALSE, fig.align = "center", fig.width = 8, fig.height = 4----
#  floyd_15_min <- floyd_15_min %>%
#    st_as_sf(coords = c("tclon", "tclat")) %>%
#    st_set_crs(4326)
#  floyd_2_hrs <- floyd_2_hrs %>%
#    st_as_sf(coords = c("tclon", "tclat")) %>%
#    st_set_crs(4326)
#  
#  a <- ggplot() +
#    geom_sf(data = floyd_states,
#            fill = "aliceblue") +
#    xlim(c(-90, -70)) +
#    ylim(c(24, 46))
#  b <- a +
#    geom_sf(data = floyd_15_min,
#            size = 0.5, color = "red") +
#    ggtitle("Interpolated to 15 minutes")
#  c <- a +
#      geom_sf(data = floyd_2_hrs,
#              size = 0.5, color = "red") +
#    ggtitle("Interpolated to 2 hours")
#  
#  gridExtra::grid.arrange(b, c, ncol = 2)

## ----eval = FALSE-------------------------------------------------------------
#    # Identify cases where a storm goes over the international date line, and
#    # longitudes change from about 180 to about -180, or vice versa. Correct this
#    # before interpolating (then later we get everything back within the 180 to -180
#    # range).
#    if(diff(range(hurr_track$tclon)) > 300){
#      hurr_track <- hurr_track %>%
#        dplyr::mutate(tclon = ifelse(tclon > 0, tclon, tclon + 360))
#    }

## ----eval = FALSE-------------------------------------------------------------
#      # Make sure that longitude is between -180 and 180
#      dplyr::mutate(tclon = ((tclon + 180) %% 360) - 180)

