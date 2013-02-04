context("movingAverage")

test_that("movingAverage", {
  values <- 1:10
  values[c(1, 2, 9, 10)] <- NA
  expect_equal(movingAverage(1:10, halfWindowSize=2), values)

  values <- 1:10
  values[c(1, 2, 3, 8, 9, 10)] <- NA
  expect_equal(movingAverage(1:10, 3), values)
})

test_that("movingAverage throws errors", {
  expect_error(movingAverage(1:10, 100), "is longer than time series")
})

