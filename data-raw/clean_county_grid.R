library(dplyr)
library(devtools)
library(stringi)

# Read in and clean up `county_centers`
county_points <- read.csv(paste0("http://www2.census.gov/geo/docs/reference/",
                                  "cenpop2010/county/CenPop2010_Mean_CO.txt"),
                           as.is = TRUE) %>%
  mutate(fips = paste0(sprintf("%02d", STATEFP),
                       sprintf("%03d", COUNTYFP)),
         COUNAME = stri_trans_general(COUNAME, "latin-ascii")) %>%
  select(fips, LATITUDE, LONGITUDE, STNAME, POPULATION) %>%
  rename(state_name = STNAME, population = POPULATION,
         latitude = LATITUDE, longitude = LONGITUDE) %>%
  filter(tolower(state_name) %in% c("alabama", "arkansas",
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
                                            "wisconsin")) %>%
  rename(gridid = fips,
         glat = latitude,
         glon = longitude,
         gpop = population) %>%
  select(gridid, glat, glon, gpop)

use_data(county_points, overwrite = TRUE)
