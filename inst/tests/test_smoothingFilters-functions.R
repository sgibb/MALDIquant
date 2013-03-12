context("smoothingFilters")

test_that("movingAverage", {
  values <- c(rep(3, 3), 4:7, rep(8, 3))
  expect_equal(movingAverage(1:10, halfWindowSize=2), values)

  values <- c(rep(4, 4), 5:6, rep(7, 4))
  expect_equal(movingAverage(1:10, 3), values)
})

test_that("movingAverage throws errors", {
  expect_error(movingAverage(1:10, 0), "too small")
  expect_error(movingAverage(1:10, 100), "too large")
})

