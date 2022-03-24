library(tidyverse)

saomai_tracks <- read_csv("data-raw/ibtracs.since1980.list.v04r00.csv") %>%
  filter(NAME == "SAOMAI" & SEASON == 2006) %>%
  rename_all(str_to_lower) %>%
  select(iso_time, lat, lon, usa_wind, usa_rmw)

use_data(saomai_tracks, overwrite = TRUE)
