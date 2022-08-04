library(tidyverse)
library(usethis)

dorian_tracks <- read_csv("data-raw/ibtracs.since1980.list.v04r00.csv") %>%
  filter(NAME == "DORIAN" & SEASON == 2019) %>%
  rename_all(str_to_lower) %>%
  select(iso_time, lat, lon, usa_wind, usa_rmw) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon),
         usa_wind = as.numeric(usa_wind),
         usa_rmw = as.numeric(usa_rmw)) %>%
  transmute(date = format(iso_time, format = "%Y%m%d%H%M"),
            latitude = as.numeric(lat),
            longitude = as.numeric(lon),
            wind = as.numeric(usa_wind))

use_data(dorian_tracks, overwrite = TRUE)
