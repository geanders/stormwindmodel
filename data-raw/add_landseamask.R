library(tidyverse)
landmask <- readr::read_csv("data-raw/landseamask_global.csv",
                col_names = c("longitude", "latitude", "land")) %>%
  dplyr::mutate(land = factor(land, levels = c(1, 0), labels = c("land", "water")),
                longitude = longitude - 360)

landmask <- landmask %>%
  arrange(latitude) %>%
  mutate(latitude = format(latitude, digits = 1, trim = TRUE)) %>%
  pivot_wider(names_from = "latitude", values_from = land) %>%
  arrange(longitude) %>%
  mutate(longitude = format(longitude, digits = 1, trim = TRUE)) %>%
  column_to_rownames("longitude") %>%
  as.matrix()

# ggplot(landmask, aes(x = longitude, y = latitude, color = land)) +
#   geom_point()

usethis::use_data(landmask, overwrite = TRUE)
