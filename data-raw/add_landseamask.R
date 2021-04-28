landmask <- readr::read_csv("data-raw/landmask_0p2deg_15to50N_260to300E.csv",
                            col_names = c("longitude", "latitude", "land")) %>%
  dplyr::mutate(land = factor(land, levels = c(1, 0), labels = c("land", "water")))

# ggplot(landmask, aes(x = longitude, y = latitude, color = land)) +
#   geom_point()

devtools::use_data(landmask)
