context("smoothingFilters")

test_that(".movingAverage", {
  values <- c(rep(3, 3), 4:7, rep(8, 3))
  expect_equal(MALDIquant:::.movingAverage(1:10, halfWindowSize=2), values)

  values <- c(rep(4, 4), 5:6, rep(7, 4))
  expect_equal(MALDIquant:::.movingAverage(1:10, 3), values)
})

test_that(".movingAverage throws errors", {
  expect_error(MALDIquant:::.movingAverage(1:10, halfWindowSize=0),
               "too small")
  expect_error(MALDIquant:::.movingAverage(1:10, halfWindowSize=100),
               "too large")
})

test_that(".savitzkyGolayCoefficients", {
  values <- list(rep(1, 5)/5,
                 c(-3, 12, 17, 12, -3)/35,
                 c(-2, 3, 6, 7, 6, 3, -2)/21,
                 c(-21, 14, 39, 54, 59, 54, 39, 14, -21)/231,
                 c(5, -30, 75, 131, 75, -30, 5)/231,
                 c(15, -55, 30, 135, 179, 135, 30, -55, 15)/429)
  hws <- c(2, 2, 3, 4, 3, 4)
  order <- c(0, 2, 3, 3:5)

  for (i in seq(along=values)) {
    expect_equal(.savitzkyGolayCoefficients(m=hws[i], k=order[i])[hws[i]+1, ],
                 values[[i]])
  }
})

test_that(".savitzkyGolay", {
  expect_equal(MALDIquant:::.savitzkyGolay(1:10, halfWindowSize=2,
                                           polynomialOrder=0),
               MALDIquant:::.movingAverage(1:10, halfWindowSize=2))

  values <- c(8L, 1L, 7L, 6L, 3L, 13L, 5L, 2L, 19L, 11L, 15L, 18L, 9L, 10L,
              20L, 12L, 17L, 14L, 16L, 4L)

  result <- c(7.64285714285716, 2.42857142857143, 4.85714285714286,
              5.14285714285714, 6.94285714285714, 8.37142857142857,
              5.68571428571428, 7.14285714285714, 11.9714285714286,
              15.2857142857143, 14.8285714285714, 15.1714285714286,
              10.9714285714286, 12.2285714285714, 15.0285714285714,
              16.4571428571429, 14.0857142857143, 16.7428571428571,
              14.1714285714286, 4.45714285714283)
  expect_equal(MALDIquant:::.savitzkyGolay(values, halfWindowSize=2,
                                           polynomialOrder=3), result)
})

test_that(".savitzkyGolay throws errors", {
  expect_error(MALDIquant:::.savitzkyGolay(1:10, halfWindowSize=0,
                                           polynomialOrder=3), "too small")
  expect_error(MALDIquant:::.savitzkyGolay(1:10, halfWindowSize=100,
                                           polynomialOrder=3), "too large")
  expect_error(MALDIquant:::.savitzkyGolay(1:10, halfWindowSize=2,
                                           polynomialOrder=10),
               "The window size has to be larger than the polynomial order.")
})
