context("Calculating forward speed")
library(tidyverse)
library(lubridate)

example_storms <- tribble(
  ~ storm_name, ~ basin, ~ time, ~ lat, ~ lon,
  "Laura", "North Atlantic", ymd_hms("2020-08-27 03:00:00"), 29.10, -93.15,
  "Laura", "North Atlantic", ymd_hms("2020-08-27 06:00:00"), 29.80, 93.30,
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
  "Not Named", "Southern Atlantic", ymd_hms("2020-03-28 06:00:00"), -29.00, -49.60,
  "Not Named", "Southern Atlantic", ymd_hms("2020-03-28 12:00:00"), -28.77, -49.93
)

test_that("Forward speed calculation correct for North Atlantic basin storm", {

})

test_that("Forward speed calculation correct for Eastern Pacific basin storm", {

})

test_that("Forward speed calculation correct for Western Pacific basin storm", {

})

test_that("Forward speed calculation correct for Northern Indian basin storm", {

})

test_that("Forward speed calculation correct for Southern Indian basin storm", {

})

test_that("Forward speed calculation correct for Southern Pacific basin storm", {

})

test_that("Forward speed calculation correct for Southern Atlantic basin storm", {

})

test_that("Forward speed calculation correct when crossing prime meridian", {

})
