library(tidyverse)

test_that("Interpolation works with North Atlantic storm", {

  # Floyd
  interp_track_floyd <- create_full_track(floyd_tracks[34:40, ], tint = 3)

  # Expectations are from 3-hourly IBTrACS data (expect long -77.49, which
  # IBTRaCS interpolates to -77.65)
  expected_interp_lats <- c(32.10, 32.95, 33.70, 34.53, 35.70, 36.81, 38.00,
                            39.36, 40.60, 41.45, 42.10, 42.74, 43.30)
  expected_interp_longs <- c(-78.70, -78.27, -78.00, -77.49, -76.80, -76.08,
                             -75.30, -74.40, -73.50, -72.77, -72.10, -71.37,
                             -70.60)

  expect_equal(round(interp_track_floyd$tclat), round(expected_interp_lats))
  expect_equal(round(interp_track_floyd$tclon), round(expected_interp_longs))

  # Katrina
  interp_track_katrina <- create_full_track(katrina_tracks[22:26, ], tint = 3)

  # Expectations are from 3-hourly IBTrACS data
  expected_interp_lats <- c(27.2, 27.67, 28.20, 28.81, 29.50, 30.27,
                            31.10, 31.87, 32.60)
  expected_interp_longs <- c(-89.20, -89.45, -89.60, -89.62, -89.60, -89.60,
                             -89.60, -89.40, -89.10)

  expect_equal(round(interp_track_katrina$tclat), round(expected_interp_lats))
  expect_equal(round(interp_track_katrina$tclon), round(expected_interp_longs))

})

test_that("Interpolation works for Southern Atlantic storm", {
  sample_track_1 <- tribble(
    ~ date, ~ latitude, ~ longitude, ~ wind,
    "200403270000", -29.10, -44.90, 70,
    "200403270600", -29.30, -45.60, 75,
    "200403271200", -29.50, -46.60, 75,
    "200403271800", -29.60, -47.40, 75,
    "200403280000", -29.30, -48.40, 75
  )
  interp_track_1 <- create_full_track(hurr_track = sample_track_1, tint = 3)

  # Expectations are from 3-hourly IBTrACS data (except -46.40 in
  # IBTRaCS replaced with -46.60)
  expected_interp_lats <- c(-29.10, -29.20, -29.30, -29.41, -29.50,
                            -29.59, -29.60, -29.48, -29.30)
  expected_interp_longs <- c(-44.90, -45.24, -45.60, -45.98, -46.60,
                             -46.88, -47.40, -47.89, -48.40)

  expect_equal(round(interp_track_1$tclat), round(expected_interp_lats))
  expect_equal(round(interp_track_1$tclon), round(expected_interp_longs))
})

test_that("Interpolation works for Western Pacific storm", {
  sample_track_1 <- tribble(
    ~ date, ~ latitude, ~ longitude, ~ wind,
    "202008311200", 22.90, 145.80, 25,
    "202008311800", 22.10, 145.31, 35,
    "202009010000", 21.80, 144.50, 35,
    "202009010600", 20.90, 144.40, 39,
    "202009011200", 20.50, 144.10, 39
  )
  interp_track_1 <- create_full_track(hurr_track = sample_track_1, tint = 3)

  # Expectations are from 3-hourly IBTrACS data (except 145.31 in
  # IBTRaCS replaced with 145.51)
  expected_interp_lats <- c(22.90, 22.44, 22.10, 21.96, 21.80,
                            21.36, 20.90, 20.65, 20.50)
  expected_interp_longs <- c(145.80, 145.51, 144.90, 144.64, 144.50,
                             144.44, 144.40, 144.31, 144.10)
  expected_interp_vmax <- c(25, 30, 35, 35, 35, 37, 39, 39, 39) %>%
    weathermetrics::knots_to_speed(unit = "mps", round = 1)

  expect_equal(round(interp_track_1$tclat), round(expected_interp_lats))
  expect_equal(round(interp_track_1$tclon), round(expected_interp_longs))
  expect_equal(round(interp_track_1$vmax), round(expected_interp_vmax))
})

# Harold crossed the international dateline. Try with both IBTrACs conventions
# (goes above 180) and other (resets at 180 to -180).
test_that("Interpolation works with IBTrACS convention across international dateline", {
  sample_track_1 <- tribble(
    ~ date, ~ latitude, ~ longitude, ~ wind,
    "202004071200", -17.40, 174.00, 109,
    "202004071800", -18.30, 175.80, 109,
    "202004080000", -18.90, 177.70, 119,
    "202004080600", -19.90, 179.70, 119,
    "202004081200", -20.60, 181.90, 115,
    "202004081800", -21.90, 184.40, 109,
    "202004090000", -23.10, 186.50, 109,
    "202004090600", -24.60, 189.30, 93,
    "202004091200", -25.90, 192.30, 80
  )
  interp_track_1 <- create_full_track(hurr_track = sample_track_1, tint = 3)

  sample_track_2 <- tribble(
    ~ date, ~ latitude, ~ longitude, ~ wind,
    "202004071200", -17.40, 174.00, 109,
    "202004071800", -18.30, 175.80, 109,
    "202004080000", -18.90, 177.70, 119,
    "202004080600", -19.90, 179.70, 119,
    "202004081200", -20.60, -178.1, 115,
    "202004081800", -21.90, -175.6, 109,
    "202004090000", -23.10, -173.5, 109,
    "202004090600", -24.60, -170.7, 93,
    "202004091200", -25.90, -167.7, 80
  )
  interp_track_2 <- create_full_track(hurr_track = sample_track_2, tint = 3)

  # Expectations are from 3-hourly IBTrACS data (with conversion to make all
  # longitudes between -180 and 180)
  expected_interp_lats <- c(-17.40, -17.83, -18.30, -18.61, -18.90, -19.28,
                            -19.90, -20.11, -20.60, -21.23, -21.90, -22.48,
                            -23.10, -23.84, -24.60, -25.23, -25.90)
  expected_interp_longs <- c(174.00, 174.86, 175.80, 	176.74, 	177.70,
                             178.68, 179.70, -179.24, -178.10, -176.84,
                             -175.60, -174.57, -173.50, -172.17, -170.70,
                             -169.23, -167.70)
  expected_interp_vmax <- c(109, 109, 109, 114, 119, 119, 119, 117, 115, 112,
                            109, 109, 109, 101, 93, 86, 80) %>%
    weathermetrics::knots_to_speed(unit = "mps", round = 1)

  expect_equal(round(interp_track_1$tclat), round(expected_interp_lats))
  expect_equal(round(interp_track_1$tclon), round(expected_interp_longs))
  expect_equal(round(interp_track_1$vmax), round(expected_interp_vmax))

  expect_equal(round(interp_track_2$tclat), round(expected_interp_lats))
  expect_equal(round(interp_track_2$tclon), round(expected_interp_longs))
  expect_equal(round(interp_track_2$vmax), round(expected_interp_vmax))
})

