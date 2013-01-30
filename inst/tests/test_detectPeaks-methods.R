context("detectPeaks")

s <- createMassSpectrum(mass=1:5, intensity=c(1, 2, 1, 2, 1))

zeroNoise <- function(x, y, ...)cbind(x, 0)

test_that("detectPeaks shows warnings", {
  expect_warning(detectPeaks(createMassSpectrum(mass=double(),
                                                intensity=double()), "empty"))
})

test_that("detectPeaks handles user defined noise estimators", {
  staticNoise <- function(x, y, ...)cbind(x, 1)

  expect_equal(detectPeaks(s, halfWindowSize=1, fun=zeroNoise),
               createMassPeaks(c(2, 4), c(2, 2), c(Inf, Inf)))
  expect_equal(detectPeaks(s, halfWindowSize=1, fun=staticNoise, SNR=1),
               createMassPeaks(c(2, 4), c(2, 2), c(2, 2)))

  wrongNoise <- function(x, y, ...) { return(0) }
  wrongNoise2 <- function(x, y, ...) {
    return(do.call(cbind, list(x, rep(1, length(x)), rep(2, length(x)))))
  }
  expect_error(detectPeaks(s, fun=wrongNoise), "noise argument is not valid")
  expect_error(detectPeaks(s, fun=wrongNoise2), "noise argument is not valid")
})

test_that("detectPeaks works with list of MassSpectrum objects", {
  expect_error(detectPeaks(list(x=1, y=1)),
               "no list of MALDIquant::MassSpectrum objects")
  expect_error(detectPeaks(list(s, createMassPeaks(1, 1)),
               "no list of MALDIquant::MassSpectrum objects"))
  p <- createMassPeaks(c(2, 4), c(2, 2), c(Inf, Inf))
  expect_equal(detectPeaks(list(s, s), halfWindowSize=1, fun=zeroNoise),
               list(p, p))
})
