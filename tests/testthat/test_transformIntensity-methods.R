context("transformIntensity")

s <- createMassSpectrum(mass=1:10, intensity=(1:10)^2)

test_that("transformIntensity shows warnings", {
  expect_warning(transformIntensity(
                   createMassSpectrum(mass=double(), intensity=double()),
                   method="sqrt"), "empty")
})

test_that("transformIntensity", {
  expect_equal(intensity(transformIntensity(s, method="sqrt")), 1:10)
  expect_equal(intensity(transformIntensity(s, method="log")), log((1:10)^2))
  expect_equal(intensity(transformIntensity(s, method="log2")), log2((1:10)^2))
  expect_equal(intensity(transformIntensity(s, method="log10")),
               log10((1:10)^2))
})

test_that("transformIntensity works with list of MassSpectrum objects", {
  expect_error(transformIntensity(list(x=1, y=1)),
               "no list of MALDIquant::AbstractMassObject objects")
  expect_error(transformIntensity(list(s, createMassPeaks(1, 1)),
               "no list of MALDIquant::AbstractMassObject objects"))
  r <- createMassSpectrum(mass=1:10, intensity=1:10)
  expect_equal(transformIntensity(list(s, s), method="sqrt"), list(r, r))
})

test_that(".transformIntensity throws errors", {
  expect_error(.transformIntensity(s), "is missing")
})

test_that(".transformIntensity shows warnings", {
  expect_warning(.transformIntensity(s, fun=function(x)return(-x)),
                 "Negative intensity values are replaced by zeros.")
})

test_that(".transformIntensity", {
  expect_equal(intensity(.transformIntensity(s, sqrt)), 1:10)
  expect_equal(length(.transformIntensity(s, MALDIquant:::.movingAverage,
                                          halfWindowSize=2)), 10)
  expect_equal(length(.transformIntensity(s,
                        function(x)as.double(filter(x, rep(1, 5)/5, sides=2)))),
               6)
  expect_equal(intensity(suppressWarnings(
                 .transformIntensity(s, fun=function(x)return(-x)))),
               rep(0, 10))
})

test_that(".transformIntensity works with list of AbstractMassObject objects", {
  expect_error(.transformIntensity(list(x=1, y=1)),
               "no list of MALDIquant::AbstractMassObject objects")
  expect_error(.transformIntensity(list(s, createMassPeaks(1, 1)),
               "no list of MALDIquant::AbstractMassObject objects"))
  r <- createMassSpectrum(mass=1:10, intensity=1:10)
  expect_equal(.transformIntensity(list(s, s), fun=sqrt), list(r, r))
})
