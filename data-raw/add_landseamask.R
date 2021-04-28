landmask <- readr::read_csv("data-raw/landseamask_global.csv",
                col_names = c("longitude", "latitude", "land")) %>%
  dplyr::mutate(land = factor(land, levels = c(1, 0), labels = c("land", "water")),
                longitude = longitude - 360)

# ggplot(landmask, aes(x = longitude, y = latitude, color = land)) +
#   geom_point()

usethis::use_data(landmask, overwrite = TRUE)
