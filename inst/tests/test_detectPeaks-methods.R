context("detectPeaks")

s <- createMassSpectrum(mass=1:5, intensity=c(1, 2, 1, 2, 1))

test_that("detectPeaks shows warnings", {
  expect_warning(detectPeaks(createMassSpectrum(mass=double(),
                                                intensity=double()), "empty"))
})

test_that("detectPeaks works with list of MassSpectrum objects", {
  expect_error(detectPeaks(list(x=1, y=1)),
               "no list of MALDIquant::MassSpectrum objects")
  expect_error(detectPeaks(list(s, createMassPeaks(1, 1)),
               "no list of MALDIquant::MassSpectrum objects"))
  p <- createMassPeaks(c(2, 4), c(2, 2), c(Inf, Inf))
})

