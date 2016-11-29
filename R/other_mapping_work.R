library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(maps)
library(ggmap)

data(county.fips)
county.fips <- county.fips %>%
  mutate(polyname = as.character(polyname))
us_counties <- map_data("county") %>%
  filter(!(region %in% c("arizona", "california", "colorado", "idaho",
                         "montana", "nebraska", "nevada", "new mexico",
                         "north dakota", "oregon", "south dakota",
                         "utah", "washington", "wyoming", "minnesota"))) %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

floyd <- get_grid_winds(hurr_track = stormwindmodel::katrina_tracks, grid_df = county_points)
floyd <- floyd %>%
  mutate(fips = as.integer(gridid))

us_counties <- us_counties %>%
  left_join(floyd, by = "fips")

ggplot() +
  borders("state", colour = "black", fill = "white") +
  geom_polygon(data = us_counties,
               aes(x = long, y = lat, group = group, fill = vmax_sust),
               color = NA, alpha = 0.8) +
  scale_fill_viridis() +
  theme_void()


get_map(c(-86.5, 36.5), zoom = 4, source = "google", maptype = "satellite") %>%
  ggmap() +
  geom_polygon(data = us_counties,
               aes(x = long, y = lat, group = group, fill = vmax_sust),
               color = NA, alpha = 0.8) +
  scale_fill_viridis(option = "A") +
  theme_void()

library(leaflet)
library(tigris)
us_counties <- counties(state = c("NC", "VA", "SC", "TN", "KY", "DC",
                                  "MD", "PA", "DE", "WV", "OH", "GA",
                                  "FL", "AL", "LA", "MS", "AR", "TX",
                                  "MO", "OK", "KS", "IA", "WI", "IL",
                                  "IN", "MI", "NY", "NJ", "MA", "VT",
                                  "NH", "ME", "CT", "RI"), cb = TRUE)
county_winds <- geo_join(us_counties, floyd, by_sp = "GEOID", by_df = "gridid")
county_winds <- county_winds[county_winds$vmax_sust > 10, ]
# pal <- colorNumeric("Blues", domain = range(county_winds$vmax_sust), na.color = "#808080")
pal <- colorNumeric(viridis(6), domain = range(county_winds$vmax_sust), na.color = "#808080")

county_popup <- paste0("<b>County: </b>", county_winds@data$NAME, "<br/>",
                       "<b>FIPS: </b>", county_winds@data$GEOID, "<br/>",
                       "<b>Max. sustained wind: </b>",
                       round(county_winds@data$vmax_sust, 1), " m/s <br/>",
                       "<b>Max. gust wind: </b>",
                       round(county_winds@data$vmax_gust, 1), " m/s <br/>")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = county_winds,
              color = ~ pal(county_winds$vmax_sust),
              fillColor = ~ pal(county_winds$vmax_sust),
              fillOpacity = 0.6,
              popup = county_popup)
