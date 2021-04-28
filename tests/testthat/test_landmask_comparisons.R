library(tidyverse)

ex_storms <- tribble(
  ~ storm_name, ~ basin, ~ lat, ~ lon, ~ land,
  "Laura", "North Atlantic", 25.03, -88.90, FALSE,
  "Laura", "North Atlantic", 27.82, -92.76, FALSE,
  "Laura", "North Atlantic", 30.47, -93.36, TRUE,
  "Laura", "North Atlantic", 21.85, -82.35, FALSE,
  "Laura", "North Atlantic", 20.22, -76.86, TRUE,
  "Laura", "North Atlantic", 19.40, -74.40, FALSE,
  "Laura", "North Atlantic", 18.57, -70.17, TRUE,
  "Laura", "North Atlantic", 17.90, -67.50, FALSE,
  "Florence", "North Atlantic", 34.31, -77.20, FALSE,
  "Florence", "North Atlantic", 34.20, -77.90, TRUE,
  "Michael", "North Atlantic", 29.37, -85.93, FALSE,
  "Michael", "North Atlantic", 30.20, -85.40, TRUE,
  "Michael", "North Atlantic", 36.50, -77.70, TRUE,
  "Michael", "North Atlantic", 39.10, -70.60, FALSE
)

test_that("Land mask comparison correct for North Atlantic basin storm", {
  storm <- ex_storms %>%
    filter(basin == "North Atlantic") %>%
    mutate(calculated_land_sea = map2_lgl(.x = lat, .y = lon, .f = check_over_land))
  expect_equal(storm$calculated_land_sea, storm$land)
})
