context(".plotMsiSlice")

test_that(".colorMatrix", {
  x <- matrix(c(NA, 1:8, NA), nrow=2)
  colRamp1 <- colorRamp(c("black", "green"))
  colRamp2 <- function(x)cbind(0, 0, 30*x)
  r1 <- matrix(c(NA, rgb(colRamp1(1:8/8), maxColorValue=255), NA), nrow=2)
  r2 <- matrix(c(NA, rgb(colRamp2(1:8), maxColorValue=255), NA), nrow=2)

  expect_equal(MALDIquant:::.colorMatrix(x, colRamp1), r1)
  expect_equal(MALDIquant:::.colorMatrix(x, colRamp2, scale=FALSE), r2)
})

test_that(".combineColorMatrices", {
  x <- array(c(1:8, 8:1), dim=c(2, 4, 2))
  col <- array(rep(1:2, each=8), dim=c(2, 4, 2))
  r <- matrix(rep(2:1, each=4), nrow=2, ncol=4)

  expect_equal(MALDIquant:::.combineColorMatrices(x, col), r)
})

test_that(".rgb", {
  r <- cbind(1:255, 1:255, 1:255)
  expect_equal(MALDIquant:::.rgb(r), rgb(r, maxColorValue=255))
})
