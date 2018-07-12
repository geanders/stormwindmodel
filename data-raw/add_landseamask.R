landmask <- readr::read_csv("data-raw/landseamask.csv",
                            col_names = c("longitude", "latitude", "land"),
                            col_types = "ddi") %>%
  dplyr::mutate(land = factor(land, levels = c(1, 0), labels = c("land", "water")))

# ggplot(landmask, aes(x = longitude, y = latitude, color = land)) +
#   geom_point()

devtools::use_data(landmask)
