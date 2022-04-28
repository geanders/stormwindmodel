library(tidyverse)
library(usethis)

saomai_tracks <- read_csv("data-raw/ibtracs.since1980.list.v04r00.csv") %>%
  filter(NAME == "SAOMAI" & SEASON == 2006) %>%
  rename_all(str_to_lower) %>%
  select(iso_time, lat, lon, usa_wind, usa_rmw) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon),
         usa_wind = as.numeric(usa_wind),
         usa_rmw = as.numeric(usa_rmw))

use_data(saomai_tracks, overwrite = TRUE)

mangkhut_tracks <- read_csv("data-raw/ibtracs.since1980.list.v04r00.csv") %>%
  filter(NAME == "MANGKHUT" & SEASON == 2018) %>%
  rename_all(str_to_lower) %>%
  select(iso_time, lat, lon, usa_wind, usa_rmw) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon),
         usa_wind = as.numeric(usa_wind),
         usa_rmw = as.numeric(usa_rmw))

use_data(mangkhut_tracks, overwrite = TRUE)
