context("smoothIntensity")

i <- c(1:20, 20:1)^2
s <- createMassSpectrum(mass=1:40, intensity=i)

test_that("smoothIntensity shows warnings", {
  expect_warning(smoothIntensity(
                   createMassSpectrum(mass=double(), intensity=double())),
                 "empty")
})

test_that("smoothIntensity", {
  expect_equal(intensity(smoothIntensity(s, method="SavitzkyGolay",
                                         halfWindowSize=4)),
               MALDIquant:::.savitzkyGolay(i, halfWindowSize=4))
  expect_equal(intensity(smoothIntensity(s, method="SavitzkyGolay")),
               MALDIquant:::.savitzkyGolay(i, halfWindowSize=10))
  expect_equal(length(smoothIntensity(s, method="MovingAverage",
                                      halfWindowSize=2)), 40)
  expect_equal(intensity(smoothIntensity(s, method="MovingAverage",
                                         halfWindowSize=2)),
               MALDIquant:::.movingAverage(i, halfWindowSize=2))
})

test_that("smoothIntensity works with list of MassSpectrum objects", {
  expect_error(smoothIntensity(list(x=1, y=1)),
               "no list of MALDIquant::MassSpectrum objects")
  expect_error(smoothIntensity(list(s, createMassPeaks(1, 1)),
               "no list of MALDIquant::MassSpectrum objects"))
  r <- createMassSpectrum(mass=1:40, intensity=
                          MALDIquant:::.movingAverage(i))
  expect_equal(smoothIntensity(list(s, s), method="MovingAverage"), list(r, r))
})
