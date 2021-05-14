library(tidyverse)

test_that("Wind speed estimates are reasonable for Hurricane Katrina", {
  katrina_full_track <- create_full_track(hurr_track = katrina_tracks, tint = 0.25)
  katrina_wind_radii <- add_wind_radii(full_track = katrina_full_track)

  orleans_location <- county_points %>%
    filter(gridid == "22071")

  miami_dade_location <- county_points %>%
    filter(gridid == "12086")

  orleans_katrina_wind <- calc_grid_wind(grid_point = orleans_location,
                                         with_wind_radii = katrina_wind_radii)

  miami_katrina_wind <- calc_grid_wind(grid_point = miami_dade_location,
                                       with_wind_radii = katrina_wind_radii)

  # Expectation based on https://www.nhc.noaa.gov/data/tcr/AL122005_Katrina.pdf
  # "The strongest sustained wind in New Orleans is subject to speculation since
  # observations are sparse, due in part to the power failures that disabled ASOS
  # stations in the area before peak wind conditions occurred. A few instrumented
  # towers placed in various locations in the metropolitan area by the Florida Coastal
  # Monitoring Program (FCMP) and by Texas Tech University measured sustained winds in
  # the range of 61-68 kt." 61 knots = 31.4 m/s, 68 knots = 35.0 m/s
  expect_true(30 <= max(orleans_katrina_wind$windspeed, na.rm = TRUE) &
                max(orleans_katrina_wind$windspeed, na.rm = TRUE) <= 40)

  # Expectation based on https://www.nhc.noaa.gov/data/tcr/AL122005_Katrina.pdf
  # "While the eye moved west-southwestward over northern Miami-Dade, it passed over the
  # NWS Miami Forecast Office / National Hurricane Center facility near Sweetwater, Florida
  # (Fig. 4), where a pressure of 983 mb was measured at 0105 UTC 26 August. The eastern
  # eyewall moved over the facility a few minutes later and sustained winds of 60 kt with a
  # gust to 76 kt were measured near 0115 UTC. The strongest sustained wind measured by a
  # land-based anemometer was 63 kt on Virginia Key" 60 knots = 30.9 m/s, 63 knots = 32.4 m/s
  expect_true(25 <= max(miami_katrina_wind$windspeed, na.rm = TRUE) &
                max(miami_katrina_wind$windspeed, na.rm = TRUE) <= 35)

})

test_that("Wind speed estimates are reasonable for Hurricane Michael", {
  michael_track <- hurricaneexposuredata::hurr_tracks %>%
    filter(storm_id == "Michael-2018")
  michael_full_track <- create_full_track(hurr_track = michael_track,
                                          tint = 0.25)
  michael_wind_radii <- add_wind_radii(full_track = michael_full_track)

  bay_county_location <- county_points %>%
    filter(gridid == "12005")

  bay_michael_wind <- calc_grid_wind(grid_point = bay_county_location,
                                     with_wind_radii = michael_wind_radii)

  # Expect sustained winds in Bay County, FL (county of Michael's landfall)
  # to be higher than Category 3 (The storm was Category 5 at landfall, but
  # the county center is not right at the landfall.)
  expect_true(max(bay_michael_wind$windspeed, na.rm = TRUE) >= 49.6)

  # County FIPS with winds over 64 knots (32.9 m/s) based on Best Tracks wind radii
  highest_wind_locs <- hurricaneexposuredata::ext_tracks_wind %>%
    filter(storm_id == "Michael-2018" & vmax_sust == 32.9216) %>%
    pull(fips)

  michael_high_winds <- get_grid_winds(hurr_track = michael_track,
                                       grid_df = county_points %>%
                                         filter(gridid %in% highest_wind_locs))

  # Leave about 5 m/s variation room around the 32.9216 threshold from the wind radii
  expect_true(all(michael_high_winds$vmax_sust > 28))
  expect_true(mean(michael_high_winds$vmax_sust) > 32.9216)

  # County FIPS with winds between 50 knots (25.72 m/s) and 64 knots (32.9 m/s) based on
  # Best Tracks wind radii
  mid_wind_locs <- hurricaneexposuredata::ext_tracks_wind %>%
    filter(storm_id == "Michael-2018" & vmax_sust == 25.7200) %>%
    pull(fips)

  michael_mid_winds <- get_grid_winds(hurr_track = michael_track,
                                       grid_df = county_points %>%
                                         filter(gridid %in% mid_wind_locs))

  # Allow some variation room around the 25.72 and 32.9216 thresholds
  # from the wind radii
  expect_true(all(15 <= michael_mid_winds$vmax_sust &
                    michael_mid_winds$vmax_sust <= 45))
  expect_true(25.72 <= mean(michael_mid_winds$vmax_sust) &
                mean(michael_mid_winds$vmax_sust) <= 32.9216)

  # County FIPS with winds between 34 knots (17.4896 m/s) and 50 knots (25.72 m/s) based on
  # Best Tracks wind radii
  low_wind_locs <- hurricaneexposuredata::ext_tracks_wind %>%
    filter(storm_id == "Michael-2018" & vmax_sust == 17.4896) %>%
    pull(fips)

  michael_low_winds <- get_grid_winds(hurr_track = michael_track,
                                      grid_df = county_points %>%
                                        filter(gridid %in% low_wind_locs))

  # Allow some variation room around the 25.72 upper threshold
  # from the wind radii
  expect_true(all(michael_low_winds$vmax_sust <= 36))
  expect_true(mean(michael_low_winds$vmax_sust) <= 25.72)

})

