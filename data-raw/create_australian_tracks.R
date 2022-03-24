library(tidyverse)

australian_tracks <- read_csv("data-raw/ibtracs.since1980.list.v04r00.csv") %>%
  filter((NAME == "MARCIA" & SEASON == 2015) |
           (NAME == "YASI" & SEASON == 2011) |
           (NAME == "GEORGE" & SEASON == 2007) |
           ((NAME == "LARRY" & SEASON == 2006))) %>%
  rename_all(str_to_lower) %>%
  select(name, season, iso_time, lat, lon, usa_wind, usa_rmw, bom_wind)

use_data(australian_tracks, overwrite = TRUE)
