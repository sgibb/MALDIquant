context("calibrateIntensity")

s <- createMassSpectrum(mass=1:10, intensity=1:10)

test_that("calibrateIntensity throws errors", {
  expect_error(calibrateIntensity(s, method="foobar"),
               ".*arg.* should be one of .*TIC.*, .*Median.*")
})

test_that("calibrateIntensity TIC works", {
  expect_equal(totalIonCurrent(calibrateIntensity(s)), 1)
  expect_equal(totalIonCurrent(calibrateIntensity(s, method="TIC")), 1)
})

test_that("calibrateIntensity Median works", {
  expect_equal(median(intensity(calibrateIntensity(s, method="Median"))), 1)
})

test_that("calibrateIntensity works with list of MassSpectrum objects", {
  expect_error(calibrateIntensity(list(x=1, y=1)),
               "no list of MALDIquant::MassSpectrum objects")
  expect_error(calibrateIntensity(list(s, createMassPeaks(1, 1)),
               "no list of MALDIquant::MassSpectrum objects"))
  expect_equal(lapply(calibrateIntensity(list(s, s), method="TIC"),
                      totalIonCurrent),
               list(1, 1))
})

