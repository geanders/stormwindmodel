
test_that("Calculation of radius to maximum winds (Rmax, Will. eq. 7a) is reasonable", {
  # X1 will typically be between 200 km and 400 km, almost always between 0 km and 500 km
  expect_equal(round(will7a(vmax_gl = 30, tclat = 25), 1), 44.5)
  expect_equal(round(will7a(vmax_gl = 60, tclat = 15), 1), 23.6)
  expect_equal(round(will7a(vmax_gl = 20.5, tclat = 40.2), 1), 66.6)
  # Check that it works on vector inputs, rather than single inputs
  expect_equal(round(will7a(vmax_gl = c(30, 60), tclat = c(25, 15)), 1),
               c(44.5, 23.6))
  # Check that is works for Southern Hemisphere storms
  expect_equal(round(will7a(vmax_gl = 30, tclat = -25), 1), 44.5)
  expect_equal(round(will7a(vmax_gl = 30, tclat = -25), 1),
               round(will7a(vmax_gl = 30, tclat = 25), 1))
  # At equal latitudes, Rmax increase decrease with higher Vmax
  expect_gt(round(will7a(vmax_gl = 30, tclat = 25), 1),
            round(will7a(vmax_gl = 40, tclat = 25), 1))
  # At equal Vmax, Rmax should increase as you get further from the equator
  expect_lt(round(will7a(vmax_gl = 30, tclat = 25), 1),
            round(will7a(vmax_gl = 30, tclat = 35), 1))
  expect_lt(round(will7a(vmax_gl = 30, tclat = -25), 1),
            round(will7a(vmax_gl = 30, tclat = -35), 1))
})

test_that("Calculation of dual exponential decay length (X1, Will. eq. 10a) is correct", {
  # X1 will typically be between 200 km and 400 km, almost always between 0 km and 500 km
  expect_equal(will10a(vmax_gl = 30, tclat = 25), 304.195)
  expect_equal(will10a(vmax_gl = 60, tclat = 15), 224.265)
  expect_equal(will10a(vmax_gl = 20.5, tclat = 40.2), 352.55)
  # Check that it works on vector inputs, rather than single inputs
  expect_equal(will10a(vmax_gl = c(30, 60), tclat = c(25, 15)), c(304.195, 224.265))
  # Check that is works for Southern Hemisphere storms
  expect_equal(will10a(vmax_gl = 30, tclat = -25), 304.195)
  expect_equal(will10a(vmax_gl = 30, tclat = -25), will10a(vmax_gl = 30, tclat = 25))
  # At equal latitudes, X1 should decrease with higher Vmax
  expect_gt(will10a(vmax_gl = 30, tclat = 25), will10a(vmax_gl = 40, tclat = 25))
  # At equal Vmax, n should increase as you get further from the equator
  expect_lt(will10a(vmax_gl = 30, tclat = 25), will10a(vmax_gl = 30, tclat = 35))
  expect_lt(will10a(vmax_gl = 30, tclat = -25), will10a(vmax_gl = 30, tclat = -35))
})

test_that("Calculation of power-law exponent (n, Will. eq. 10b) is correct", {
  # n will typically be between 0.5 and 1.5, almost always between 0 and 3
  expect_equal(will10b(vmax_gl = 30, tclat = 25), 0.7437)
  expect_equal(will10b(vmax_gl = 60, tclat = 15), 1.2137)
  expect_equal(will10b(vmax_gl = 20.5, tclat = 40.2), 0.54914)
  # Check that it works on vector inputs, rather than single inputs
  expect_equal(will10b(vmax_gl = c(30, 60), tclat = c(25, 15)), c(0.7437, 1.2137))
  # Check that is works for Southern Hemisphere storms
  expect_equal(will10b(vmax_gl = 30, tclat = -25), 0.7437)
  # At equal latitudes, n should increase with higher Vmax
  expect_lt(will10b(vmax_gl = 30, tclat = 25), will10b(vmax_gl = 40, tclat = 25))
  # At equal Vmax, n should decrease as you get further from the equator
  expect_gt(will10b(vmax_gl = 30, tclat = 25), will10b(vmax_gl = 30, tclat = 35))
  expect_gt(will10b(vmax_gl = 30, tclat = -25), will10b(vmax_gl = 30, tclat = -35))
})

test_that("Calculation of contribution of shorter decay length (A, Will. eq. 10c) is correct", {
  # A will be between 0 and 1
  expect_equal(will10c(vmax_gl = 30, tclat = 25), 0.0566)
  expect_equal(will10c(vmax_gl = 60, tclat = 15), 0.2676)
  expect_equal(will10c(vmax_gl = 20.5, tclat = 40.2), 0)
  # Check that it works on vector inputs, rather than single inputs
  expect_equal(will10c(vmax_gl = c(30, 60), tclat = c(25, 15)), c(0.0566, 0.2676))
  # Check that is works for Southern Hemisphere storms
  expect_equal(will10c(vmax_gl = 30, tclat = -25), 0.0566)
  # At equal latitudes, A should increase with higher Vmax
  expect_lt(will10c(vmax_gl = 30, tclat = 25), will10c(vmax_gl = 40, tclat = 25))
  # At equal Vmax, A should decrease as you get further from the equator
  expect_gt(will10c(vmax_gl = 30, tclat = 25), will10c(vmax_gl = 30, tclat = 35))
  expect_gt(will10c(vmax_gl = 30, tclat = -25), will10c(vmax_gl = 30, tclat = -35))
})

test_that("Calculation of R1 is correct", {
  # Need to put together a few functions to get this values
  get_R1 <- function(vmax_gl, tclat){
    Rmax <- will7a(vmax_gl = vmax_gl, tclat = tclat)
    X1 <- will10a(vmax_gl = vmax_gl, tclat = tclat)
    A <- will10c(vmax_gl = vmax_gl, tclat = tclat)
    n <- will10b(vmax_gl = vmax_gl, tclat = tclat)
    eq3_right <- will3_right(n = n, A = A, X1 = X1, Rmax = Rmax)
    xi <- solve_for_xi(eq3_right = eq3_right)
    R1 <- calc_R1(Rmax = Rmax, xi = xi)
    return(R1)
  }

  # R1 will always be less than Rmax, which is usually 20--60 km, almost always under 100 km
  expect_equal(round(get_R1(vmax_gl = 30, tclat = 25), 1), 28.2)
  expect_equal(round(get_R1(vmax_gl = 60, tclat = 15), 1), 6.2)
  expect_equal(round(get_R1(vmax_gl = 20.5, tclat = 40.2), 1), 51.5)
  # Check that is works for Southern Hemisphere storms
  expect_equal(round(get_R1(vmax_gl = 30, tclat = -25), 1), 28.2)
  # At equal latitudes, A should decrease with higher Vmax
  expect_gt(round(get_R1(vmax_gl = 30, tclat = 25), 1),
            round(get_R1(vmax_gl = 40, tclat = 25), 1))
  # At equal Vmax, R1 should increase as you get further from the equator
  expect_lt(round(get_R1(vmax_gl = 30, tclat = 25), 1),
            round(get_R1(vmax_gl = 30, tclat = 35), 1))
  expect_lt(round(get_R1(vmax_gl = 30, tclat = -25), 1),
            round(get_R1(vmax_gl = 30, tclat = -35), 1))
})

test_that("Calculated parameters are reasonable for a sample N. Atlantic storm", {
  library(hurricaneexposuredata)
  data(hurr_tracks)

  sample_storm <- sample(unique(hurr_tracks$storm_id), 1)
  print(paste("Sample storm:", sample_storm))

  sample_storm_params <- hurr_tracks %>%
    filter(storm_id == sample_storm) %>%
    create_full_track() %>%
    add_wind_radii() %>%
    # Last line has some missing values, so remove it for checks
    slice(1:(n()-1))

  # X1 will typically be between 200 km and 400 km, almost always between 0 km and 500 km
  expect_true(all(sample_storm_params$X1 > 0))
  expect_true(all(sample_storm_params$X1 < 500))

  # n will typically be between 0.5 and 1.5, almost always between 0 and 3
  expect_true(all(sample_storm_params$n > 0))
  expect_true(all(sample_storm_params$n < 3))

  # A should be between 0 and 1 (often 0)
  expect_true(all(sample_storm_params$A >= 0))
  expect_true(all(sample_storm_params$A <= 1))

  # R2 should always be larger than R1
  expect_true(all(sample_storm_params$R2 >= sample_storm_params$R1))

  # Rmax should always be between R1 and R2
  expect_true(all(sample_storm_params$Rmax >= sample_storm_params$R1))
  expect_true(all(sample_storm_params$Rmax <= sample_storm_params$R2))

  # Rmax is usually below 100 km. Checking against 200 km
  expect_true(all(sample_storm_params$Rmax <= 200))
})
