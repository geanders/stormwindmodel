
test_that("Gradient wind direction at grid point is correct", {
  test_gwd1 <- calc_gwd(tclat = 41.4, tclon = -72.8,
                        glat = 32.50039, glon = -86.49416)
  expect_equal(round(test_gwd1, 2), 304.82)

  test_gwd2 <- calc_gwd(tclat = -41.4, tclon = -72.8,
                        glat = -32.50039, glon = -86.49416)
  expect_equal(round(test_gwd2, 2), 55.18)
})

test_that("Inflow angle is adjusted correctly", {
  test_inflow1 <- add_inflow(gwd = 304.82, cdist = 150, Rmax = 25, tclat = 41.4)
  expect_equal(round(test_inflow1), 350)

  # Wind direction changes less with added inflow as you get towards and
  # within the radius of maximum winds
  test_inflow2 <- add_inflow(gwd = 304.82, cdist = 28, Rmax = 25, tclat = 41.4)
  expect_equal(round(test_inflow2), 348)
  test_inflow3 <- add_inflow(gwd = 304.82, cdist = 15, Rmax = 25, tclat = 41.4)
  expect_equal(round(test_inflow3), 336)

  # Adjustment in opposite direction in the Southern Hemisphere
  test_inflow4 <- add_inflow(gwd = 304.82, cdist = 150, Rmax = 25, tclat = -41.4)
  expect_equal(round(test_inflow4), 260)

  test_inflow5 <- add_inflow(gwd = 304.82, cdist = 28, Rmax = 25, tclat = -41.4)
  expect_equal(round(test_inflow5), 262)

  test_inflow6 <- add_inflow(gwd = 304.82, cdist = 15, Rmax = 25, tclat = -41.4)
  expect_equal(round(test_inflow6), 273)
})
