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
  "Michael", "North Atlantic", 39.10, -70.60, FALSE,
  "Odile", "Eastern Pacific", 22.00, -109.30, FALSE,
  "Odile", "Eastern Pacific", 23.30, -110.10, TRUE,
  "Odile", "Eastern Pacific", 28.80, -113.50, TRUE,
  "Odile", "Eastern Pacific", 30.16, -113.53, FALSE,
  "Odile", "Eastern Pacific", 30.93, -112.46, TRUE,
  "Rammasun", "Western Pacific", 12.68, 126.25, FALSE,
  "Rammasun", "Western Pacific", 14.11, 121.25, TRUE,
  "Rammasun", "Western Pacific", 20.98, 109.04, FALSE,
  "Rammasun", "Western Pacific", 22.02, 107.72, TRUE,
  "Hudhud", "Northern Indian", 17.38, 83.78, FALSE,
  "Hudhud", "Northern Indian", 17.79, 83.07, TRUE,
  "Amphan", "Northern Indian", 20.50, 87.90, FALSE,
  "Amphan", "Northern Indian", 22.20, 88.30, TRUE,
  "Idai", "Southern Indian", -17.77, 37.59, FALSE,
  "Idai", "Southern Indian", -17.30, 37.30, TRUE,
  "Idai", "Southern Indian", -16.62, 38.56, TRUE,
  "Idai", "Southern Indian", -16.90, 39.65, FALSE,
  "Idai", "Southern Indian", -19.80, 35.60, FALSE,
  "Idai", "Southern Indian", -19.55, 34.47, TRUE,
  "Trevor", "Southern Pacific", -9.35, 150.00, FALSE,
  "Trevor", "Southern Pacific", -9.81, 149.00, TRUE,
  "Trevor", "Southern Pacific", -10.40, 147.45, FALSE,
  "Trevor", "Southern Pacific", -13.01, 143.82, FALSE,
  "Trevor", "Southern Pacific", -12.97, 143.45, TRUE,
  "Trevor", "Southern Pacific", -12.94, 141.88, TRUE,
  "Trevor", "Southern Pacific", -13.50, 141.31, FALSE,
  "Trevor", "Southern Pacific", -16.00, 137.55, FALSE,
  "Trevor", "Southern Pacific", -16.39, 137.08, TRUE,
  "Not Named", "Southern Atlantic", -29.17, -49.04, FALSE,
  "Not Named", "Southern Atlantic", -29.00, -49.60, TRUE

)

test_that("Land mask comparison correct for North Atlantic basin storms", {
  storm <- ex_storms %>%
    filter(basin == "North Atlantic") %>%
    mutate(calculated_land_sea = map2_lgl(.x = lat, .y = lon, .f = check_over_land))
  expect_equal(storm$calculated_land_sea, storm$land)
})

test_that("Land mask comparison correct for Eastern Pacific basin storms", {
  storm <- ex_storms %>%
    filter(basin == "Eastern Pacific") %>%
    mutate(calculated_land_sea = map2_lgl(.x = lat, .y = lon, .f = check_over_land))
  expect_equal(storm$calculated_land_sea, storm$land)
})

test_that("Land mask comparison correct for Western Pacific basin storms", {
  storm <- ex_storms %>%
    filter(basin == "Western Pacific") %>%
    mutate(calculated_land_sea = map2_lgl(.x = lat, .y = lon, .f = check_over_land))
  expect_equal(storm$calculated_land_sea, storm$land)
})

test_that("Land mask comparison correct for Northern Indian basin storms", {
  storm <- ex_storms %>%
    filter(basin == "Northern Indian") %>%
    mutate(calculated_land_sea = map2_lgl(.x = lat, .y = lon, .f = check_over_land))
  expect_equal(storm$calculated_land_sea, storm$land)
})

test_that("Land mask comparison correct for Southern Indian basin storms", {
  storm <- ex_storms %>%
    filter(basin == "Southern Indian") %>%
    mutate(calculated_land_sea = map2_lgl(.x = lat, .y = lon, .f = check_over_land))
  expect_equal(storm$calculated_land_sea, storm$land)
})

test_that("Land mask comparison correct for Southern Pacific basin storms", {
  storm <- ex_storms %>%
    filter(basin == "Southern Pacific") %>%
    mutate(calculated_land_sea = map2_lgl(.x = lat, .y = lon, .f = check_over_land))
  expect_equal(storm$calculated_land_sea, storm$land)
})

test_that("Land mask comparison correct for Southern Atlantic basin storms", {
  storm <- ex_storms %>%
    filter(basin == "Southern Atlantic") %>%
    mutate(calculated_land_sea = map2_lgl(.x = lat, .y = lon, .f = check_over_land))
  expect_equal(storm$calculated_land_sea, storm$land)
})
