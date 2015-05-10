context("warpingFunctions")

test_that(".warpingFunctionLowess", {
  al <- function(x) {
    l <- lowess(x=x, y=x^2)
    return(approxfun(l$x, l$y))
  }
  expect_equal(MALDIquant:::.warpingFunctionLowess(1:10, (1:10)^2)(1:10),
               al(1:10)(1:10))
})

test_that(".warpingFunctionLinear", {
  expect_equal(MALDIquant:::.warpingFunctionLinear(1:10, rep(1, 10))(1:10),
               rep(1, 10))
  expect_equal(MALDIquant:::.warpingFunctionLinear(1:10, 1:10)(1:10),
               1:10)
})

test_that(".warpingFunctionQuadratic", {
  expect_equal(MALDIquant:::.warpingFunctionQuadratic(1:10, (1:10)^2)(1:10),
               (1:10)^2)
})

test_that(".warpingFunctionCubic", {
  expect_equal(MALDIquant:::.warpingFunctionCubic(1:10, (1:10)^3)(1:10),
               (1:10)^3)
})
