context(".plotMsiSlice")

test_that(".array2matrix", {
  x1 <- array(1:12, dim=c(x=2, y=3, z=2))
  x2 <- array(1:12, dim=c(1, 12, 1))
  r1 <- matrix(1:6, nrow=2, ncol=3)
  r2 <- matrix(7:12, nrow=2, ncol=3)
  r3 <- matrix(1:12, nrow=1, ncol=12)

  expect_identical(MALDIquant:::.array2matrix(x1), r1)
  expect_identical(MALDIquant:::.array2matrix(x1, z=2), r2)
  expect_identical(MALDIquant:::.array2matrix(x2), r3)
})

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
