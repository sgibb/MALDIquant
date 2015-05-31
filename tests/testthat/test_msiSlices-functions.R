context("msiSlices")

test_that("msiSlices", {
  p <- list(createMassPeaks(mass=1:5, intensity=1:5),
            createMassPeaks(mass=1:5, intensity=2:6),
            createMassPeaks(mass=1:5, intensity=3:7))
  coordinates(p) <- cbind(x=c(2, 2, 3), y=c(2, 3, 2))

  r <- array(c(3, 5, 4, NA), dim=c(x=2, y=2, z=1))
  attr(r, "center") <- 3
  attr(r, "tolerance") <- 0.5
  attr(r, "method") <- "sum"

  expect_equal(msiSlices(p, center=3, tolerance=0.5), r)

  r <- array(c(NA, NA, NA, NA, 3, 5, NA, 4, NA), dim=c(x=3, y=3, z=1))
  attr(r, "center") <- 3
  attr(r, "tolerance") <- 0.5
  attr(r, "method") <- "sum"
  expect_equal(msiSlices(p, center=3, tolerance=0.5, adjust=FALSE), r)
})

test_that(".msiSlices", {
  m <- matrix(c(1:5, 2:6, 3:7), byrow=TRUE, nrow=3)
  attr(m, "mass") <- 1:5
  coord <- cbind(x=c(1, 1, 2), y=c(1, 2, 1))

  r <- array(c(3, 5, 4, NA), dim=c(x=2, y=2, z=1))
  attr(r, "center") <- 3
  attr(r, "tolerance") <- 0.5
  attr(r, "method") <- "sum"
  expect_equal(MALDIquant:::.msiSlices(m, coord, center=3, tolerance=0.5), r)

  r[,,1] <- c(9, 15, 12, NA)
  attr(r, "tolerance") <- 1
  expect_equal(MALDIquant:::.msiSlices(m, coord, center=3, tolerance=1), r)

  r[,,1] <- c(3, 5, 4, NA)
  attr(r, "tolerance") <- 1
  attr(r, "method") <- "mean"
  expect_equal(MALDIquant:::.msiSlices(m, coord, center=3, tolerance=1,
                                       method="mean"), r)

  r[,,1] <- c(3, 5, 4, NA)
  attr(r, "tolerance") <- 1
  attr(r, "method") <- "median"
  expect_equal(MALDIquant:::.msiSlices(m, coord, center=3, tolerance=1,
                                       method="median"), r)

  r <- array(c(6, 12, 9, NA, 9, 15, 12, NA), dim=c(x=2, y=2, z=2))
  attr(r, "center") <- 2:3
  attr(r, "tolerance") <- 1
  attr(r, "method") <- "sum"
  expect_equal(MALDIquant:::.msiSlices(m, coord, center=2:3, tolerance=1), r)

  r[,,2] <- c(15, 25, 20, NA)
  attr(r, "tolerance") <- 1:2
  expect_equal(MALDIquant:::.msiSlices(m, coord, center=2:3, tolerance=1:2), r)
})
