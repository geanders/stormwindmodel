library(tidyverse)

ex_storms <- tribble(
  ~ storm_name, ~ basin, ~ lat, ~ lon, ~ land,
  "Laura", "North Atlantic", 25.03, -88.90, FALSE,
  "Laura", "North Atlantic", 27.82, -92.76, FALSE,
  "Laura", "North Atlantic", 30.47, -93.36, TRUE,
  "Laura", "North Atlantic", 22.50, -84.18, TRUE,
  "Laura", "North Atlantic", 21.85, -82.35, FALSE,
  "Laura", "North Atlantic", 20.22, -76.86, TRUE,
  "Laura", "North Atlantic", 19.40, -74.40, FALSE,
  "Laura", "North Atlantic", 18.57, -70.17, TRUE,
  "Laura", "North Atlantic", 17.90, -67.50, FALSE
)

test_that("Land mask comparison correct for North Atlantic basin storm", {
  storm <- ex_storms %>%
    filter(basin == "North Atlantic") %>%
    mutate(calculated_land_sea = map2_lgl(.x = lat, .y = lon, .f = check_over_land))
  expect_equal(storm$calculated_land_sea, storm$land)
})
