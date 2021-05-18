library(tidyverse)
library(lubridate)

example_storms <- tribble(
  ~ storm_name, ~ basin, ~ time, ~ lat, ~ lon,
  "Laura", "North Atlantic", ymd_hms("2020-08-27 03:00:00"), 29.10, -93.15,
  "Laura", "North Atlantic", ymd_hms("2020-08-27 06:00:00"), 29.80, -93.30,
  "Douglas", "Eastern Pacific", ymd_hms("2020-07-24 00:00:00"), 14.60, -138.00,
  "Douglas", "Eastern Pacific", ymd_hms("2020-07-24 03:00:00"), 14.95, -138.74,
  "Haishen", "Western Pacific", ymd_hms("2020-09-06 21:00:00"), 34.31, 128.97,
  "Haishen", "Western Pacific", ymd_hms("2020-09-07 00:00:00"), 35.50, 128.90,
  "Amphan", "Northern Indian", ymd_hms("2020-05-20 00:00:00"), 19.20, 87.40,
  "Amphan", "Northern Indian", ymd_hms("2020-05-20 03:00:00"), 19.79, 87.66,
  "Belna", "Southern Indian", ymd_hms("2019-12-09 12:00:00"), -12.40, 46.50,
  "Belna", "Southern Indian", ymd_hms("2019-12-09 15:00:00"), -12.63, 46.41,
  "Harold", "Southern Pacific", ymd_hms("2020-04-03 12:00:00"), -12.70, 163.00,
  "Harold", "Southern Pacific", ymd_hms("2020-04-03 15:00:00"), -13.71, 163.41,
  "Not Named", "Southern Atlantic", ymd_hms("2004-03-28 06:00:00"), -29.00, -49.60,
  "Not Named", "Southern Atlantic", ymd_hms("2004-03-28 09:00:00"), -28.77, -49.93,
  "Ophelia", "crossed prime meridian", ymd_hms("2005-09-22 12:00:00"), 65.60, -1.00,
  "Ophelia", "crossed prime meridian", ymd_hms("2005-09-22 18:00:00"), 66.60, 1.90,
  "Harold", "crossed international date line", ymd_hms("2020-04-08 06:00:00"), -20.11, 179.70,
  "Harold", "crossed international date line", ymd_hms("2020-04-08 12:00:00"), -20.60, -178.1,
  ## IBTrACS continues above 180 when tracking storms like this, it seems (i.e., won't change hemisphere convention mid-storm)
  "Harold", "crossed international date line IBTrACS", ymd_hms("2020-04-08 06:00:00"), -20.11, 179.70,
  "Harold", "crossed international date line IBTrACS", ymd_hms("2020-04-08 12:00:00"), -20.60, 181.90
)

# Distance estimates compared with those made through NOAA widget, available
# at https://www.nhc.noaa.gov/gccalc.shtml

## Estimates of storm forward speed are also available in IBTrACS

test_that("Forward speed calculation correct for North Atlantic basin storm", {
  storm <- example_storms %>%
    filter(basin == "North Atlantic")
  dist <- latlon_to_km(tclat_1 = storm$lat[1],
                       tclon_1 = storm$lon[1],
                       tclat_2 = storm$lat[2],
                       tclon_2 = storm$lon[2]
                       )
  expect_equal(round(dist), 79)

  for_speed <- calc_forward_speed(tclat_1 = storm$lat[1],
                                  tclon_1 = storm$lon[1],
                                  time_1 = storm$time[1],
                                  tclat_2 = storm$lat[2],
                                  tclon_2 = storm$lon[2],
                                  time_2 = storm$time[2])
  expect_equal(round(for_speed, 1), 7.3)

  # Check that distance function works for a vector
  dist2 <- latlon_to_km(tclat_1 = storm$lat[c(1, 2)],
                        tclon_1 = storm$lon[c(1, 2)],
                        tclat_2 = storm$lat[c(2, 1)],
                        tclon_2 = storm$lon[c(2, 1)]
  )
  expect_equal(round(dist2, 1), c(79.3, 79.3))
})

test_that("Forward speed calculation correct for Eastern Pacific basin storm", {
  storm <- example_storms %>%
    filter(basin == "Eastern Pacific")
  dist <- latlon_to_km(tclat_1 = storm$lat[1],
                       tclon_1 = storm$lon[1],
                       tclat_2 = storm$lat[2],
                       tclon_2 = storm$lon[2]
  )
  expect_equal(round(dist), 89)

  for_speed <- calc_forward_speed(tclat_1 = storm$lat[1],
                                  tclon_1 = storm$lon[1],
                                  time_1 = storm$time[1],
                                  tclat_2 = storm$lat[2],
                                  tclon_2 = storm$lon[2],
                                  time_2 = storm$time[2])
  expect_equal(round(for_speed, 1), 8.2)
})

test_that("Forward speed calculation correct for Western Pacific basin storm", {
  storm <- example_storms %>%
    filter(basin == "Western Pacific")
  dist <- latlon_to_km(tclat_1 = storm$lat[1],
                       tclon_1 = storm$lon[1],
                       tclat_2 = storm$lat[2],
                       tclon_2 = storm$lon[2]
  )
  expect_equal(round(dist), 133)

  for_speed <- calc_forward_speed(tclat_1 = storm$lat[1],
                                  tclon_1 = storm$lon[1],
                                  time_1 = storm$time[1],
                                  tclat_2 = storm$lat[2],
                                  tclon_2 = storm$lon[2],
                                  time_2 = storm$time[2])
  expect_equal(round(for_speed, 1), 12.3)
})

test_that("Forward speed calculation correct for Northern Indian basin storm", {
  storm <- example_storms %>%
    filter(basin == "Northern Indian")
  dist <- latlon_to_km(tclat_1 = storm$lat[1],
                       tclon_1 = storm$lon[1],
                       tclat_2 = storm$lat[2],
                       tclon_2 = storm$lon[2]
  )
  expect_equal(round(dist), 71)

  for_speed <- calc_forward_speed(tclat_1 = storm$lat[1],
                                  tclon_1 = storm$lon[1],
                                  time_1 = storm$time[1],
                                  tclat_2 = storm$lat[2],
                                  tclon_2 = storm$lon[2],
                                  time_2 = storm$time[2])
  expect_equal(round(for_speed, 1), 6.6)
})

test_that("Forward speed calculation correct for Southern Indian basin storm", {
  storm <- example_storms %>%
    filter(basin == "Southern Indian")
  dist <- latlon_to_km(tclat_1 = storm$lat[1],
                       tclon_1 = storm$lon[1],
                       tclat_2 = storm$lat[2],
                       tclon_2 = storm$lon[2]
  )
  expect_equal(round(dist), 27)

  for_speed <- calc_forward_speed(tclat_1 = storm$lat[1],
                                  tclon_1 = storm$lon[1],
                                  time_1 = storm$time[1],
                                  tclat_2 = storm$lat[2],
                                  tclon_2 = storm$lon[2],
                                  time_2 = storm$time[2])
  expect_equal(round(for_speed, 1), 2.5)
})

test_that("Forward speed calculation correct for Southern Pacific basin storm", {
  storm <- example_storms %>%
    filter(basin == "Southern Pacific")
  dist <- latlon_to_km(tclat_1 = storm$lat[1],
                       tclon_1 = storm$lon[1],
                       tclat_2 = storm$lat[2],
                       tclon_2 = storm$lon[2]
  )
  expect_equal(round(dist), 121)

  for_speed <- calc_forward_speed(tclat_1 = storm$lat[1],
                                  tclon_1 = storm$lon[1],
                                  time_1 = storm$time[1],
                                  tclat_2 = storm$lat[2],
                                  tclon_2 = storm$lon[2],
                                  time_2 = storm$time[2])
  expect_equal(round(for_speed, 1), 11.2)
})

test_that("Forward speed calculation correct for Southern Atlantic basin storm", {
  storm <- example_storms %>%
    filter(basin == "Southern Atlantic")
  dist <- latlon_to_km(tclat_1 = storm$lat[1],
                       tclon_1 = storm$lon[1],
                       tclat_2 = storm$lat[2],
                       tclon_2 = storm$lon[2]
  )
  expect_equal(round(dist), 41)

  for_speed <- calc_forward_speed(tclat_1 = storm$lat[1],
                                  tclon_1 = storm$lon[1],
                                  time_1 = storm$time[1],
                                  tclat_2 = storm$lat[2],
                                  tclon_2 = storm$lon[2],
                                  time_2 = storm$time[2])
  expect_equal(round(for_speed, 1), 3.8)
})

test_that("Forward speed calculation correct when crossing prime meridian", {
  storm <- example_storms %>%
    filter(basin == "crossed prime meridian")
  dist <- latlon_to_km(tclat_1 = storm$lat[1],
                       tclon_1 = storm$lon[1],
                       tclat_2 = storm$lat[2],
                       tclon_2 = storm$lon[2]
  )
  expect_equal(round(dist), 172)

  for_speed <- calc_forward_speed(tclat_1 = storm$lat[1],
                                  tclon_1 = storm$lon[1],
                                  time_1 = storm$time[1],
                                  tclat_2 = storm$lat[2],
                                  tclon_2 = storm$lon[2],
                                  time_2 = storm$time[2])
  expect_equal(round(for_speed, 1), 8.0)
})

test_that("Forward speed calculation correct when crossing international date line", {
  storm <- example_storms %>%
    filter(basin == "crossed international date line")
  dist <- latlon_to_km(tclat_1 = storm$lat[1],
                       tclon_1 = storm$lon[1],
                       tclat_2 = storm$lat[2],
                       tclon_2 = storm$lon[2]
  )
  expect_equal(round(dist), 236)

  for_speed <- calc_forward_speed(tclat_1 = storm$lat[1],
                                  tclon_1 = storm$lon[1],
                                  time_1 = storm$time[1],
                                  tclat_2 = storm$lat[2],
                                  tclon_2 = storm$lon[2],
                                  time_2 = storm$time[2])
  expect_equal(round(for_speed, 1), 10.9)
})

test_that("Forward speed calculation correct when crossing international date line under IBTrACS conventions", {
  storm <- example_storms %>%
    filter(basin == "crossed international date line IBTrACS")
  dist <- latlon_to_km(tclat_1 = storm$lat[1],
                       tclon_1 = storm$lon[1],
                       tclat_2 = storm$lat[2],
                       tclon_2 = storm$lon[2]
  )
  expect_equal(round(dist), 236)

  for_speed <- calc_forward_speed(tclat_1 = storm$lat[1],
                                  tclon_1 = storm$lon[1],
                                  time_1 = storm$time[1],
                                  tclat_2 = storm$lat[2],
                                  tclon_2 = storm$lon[2],
                                  time_2 = storm$time[2])
  expect_equal(round(for_speed, 1), 10.9)
})
