context("smoothIntensity")

s <- createMassSpectrum(mass=1:10, intensity=(1:10)^2)

test_that("smoothIntensity shows warnings", {
  expect_warning(smoothIntensity(
                   createMassSpectrum(mass=double(), intensity=double())),
                 "empty")
})

test_that("smoothIntensity", {
  expect_equal(length(smoothIntensity(s, method="MovingAverage",
                                      halfWindowSize=2)), 10)
  expect_equal(intensity(smoothIntensity(s, method="MovingAverage",
                                         halfWindowSize=2)),
               MALDIquant:::.movingAverage((1:10)^2, halfWindowSize=2))
})

test_that("smoothIntensity works with list of MassSpectrum objects", {
  expect_error(smoothIntensity(list(x=1, y=1)),
               "no list of MALDIquant::MassSpectrum objects")
  expect_error(smoothIntensity(list(s, createMassPeaks(1, 1)),
               "no list of MALDIquant::MassSpectrum objects"))
  r <- createMassSpectrum(mass=1:10, intensity=
                          MALDIquant:::.movingAverage((1:10)^2))
  expect_equal(smoothIntensity(list(s, s), method="MovingAverage"), list(r, r))
})

