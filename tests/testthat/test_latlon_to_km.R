context("Convert from lat-lon to kilometers")

test_that("Conversion from degrees to radians works", {
  expect_equal(degrees_to_radians(degrees = 180), pi)
  expect_equal(degrees_to_radians(degrees = -90), -pi / 2)
  expect_equal(degrees_to_radians(degrees = 0), 0)
  expect_equal(degrees_to_radians(degrees = 540), pi * 3)
  expect_equal(degrees_to_radians(degrees = c(180, 0)), c(pi, 0))
  expect_error(degrees_to_radians("180"))
})

test_that("Cpp conversion from degrees to radians works", {
  expect_equal(degrees_to_radians_Cpp2(degrees = 180), pi)
  expect_equal(degrees_to_radians_Cpp2(degrees = -90), -pi / 2)
  expect_equal(degrees_to_radians_Cpp2(degrees = 0), 0)
  expect_equal(degrees_to_radians_Cpp2(degrees = 540), pi * 3)
  expect_equal(degrees_to_radians_Cpp2(degrees = c(180, 0)), c(pi, 0))
  expect_error(degrees_to_radians_Cpp2("180"))
})
