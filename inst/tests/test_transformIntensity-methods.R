context("transformIntensity")

s <- createMassSpectrum(mass=1:10, intensity=(1:10)^2)

test_that("transformIntensity throws errors", {
  expect_error(transformIntensity(s))
})

test_that("transformIntensity shows warnings", {
  expect_warning(transformIntensity(
                   createMassSpectrum(mass=double(), intensity=double()), sqrt),
                 "empty")
})

test_that("transformIntensity", {
  expect_equal(intensity(transformIntensity(s, sqrt)), 1:10)
  expect_equal(length(transformIntensity(s, movingAverage, halfWindowSize=2)),
               10)
  expect_equal(length(transformIntensity(s,
                        function(x)filter(x, rep(1, 5)/5, sides=2))), 6)
})

test_that("transformIntensity works with list of MassSpectrum objects", {
  expect_error(transformIntensity(list(x=1, y=1)),
               "no list of MALDIquant::AbstractMassObject objects")
  expect_error(transformIntensity(list(s, createMassPeaks(1, 1)),
               "no list of MALDIquant::AbstractMassObject objects"))
  r <- createMassSpectrum(mass=1:10, intensity=1:10)
  expect_equal(transformIntensity(list(s, s), sqrt), list(r, r))
})

