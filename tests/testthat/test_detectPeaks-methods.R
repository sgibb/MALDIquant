context("detectPeaks")

s <- createMassSpectrum(mass=1:5, intensity=c(1, 2, 1, 2, 1))
p <- createMassPeaks(c(2, 4), c(2, 2), c(Inf, Inf))

test_that("detectPeaks", {
  expect_equal(detectPeaks(s, halfWindowSize=1), p)
})

test_that("detectPeaks shows warnings", {
  expect_warning(detectPeaks(createMassSpectrum(mass=double(),
                                                intensity=double()), "empty"))
})

test_that("detectPeaks works with list of MassSpectrum objects", {
  expect_error(detectPeaks(list(x=1, y=1)),
               "no list of MALDIquant::MassSpectrum objects")
  expect_error(detectPeaks(list(s, createMassPeaks(1, 1)),
               "no list of MALDIquant::MassSpectrum objects"))
  expect_equal(detectPeaks(list(s, s), halfWindowSize=1), list(p, p))
})
