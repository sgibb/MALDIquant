context("constructors")

meta <- list(name="test", file="test_constructor-functions.R")

test_that("a MassSpectrum object is constructed", {
  expect_error(createMassSpectrum(mass=1:5, intensity=1:10),
               "have to be equal")
  expect_error(createMassSpectrum(mass=1:10, intensity=1:5),
               "have to be equal")
  expect_error(createMassSpectrum(mass=1:10, intensity=1:10, metaData=1:10),
               "invalid object for slot \"metaData\"")
  expect_error(createMassSpectrum(mass=LETTERS[1:5], intensity=1:5),
               "invalid object for slot \"mass\"")
  expect_error(createMassSpectrum(mass=1:5, intensity=LETTERS[1:5]),
               "invalid object for slot \"intensity\"")

  expect_warning(createMassSpectrum(mass=-(1:5), intensity=1:5),
                 "Negative mass values found.")
  expect_warning(createMassSpectrum(mass=1:5, intensity=-(1:5)),
                 "Negative intensity values found.")

  expect_equal(createMassSpectrum(mass=1:10, intensity=1:10),
               new("MassSpectrum", mass=1:10, intensity=1:10, metaData=list()))
  expect_equal(createMassSpectrum(1:10, 1:10, meta),
               new("MassSpectrum", mass=1:10, intensity=1:10, metaData=meta))
})

test_that("a MassPeaks object is constructed", {
  expect_error(createMassPeaks(mass=1:5, intensity=1:10),
               "have to be equal")
  expect_error(createMassPeaks(mass=1:10, intensity=1:5),
               "have to be equal")
  expect_error(createMassPeaks(mass=1:5, intensity=1:5, snr=1:2),
               "have to be equal")
  expect_error(createMassPeaks(mass=1:10, intensity=1:10, metaData=1:10),
               "invalid object for slot \"metaData\"")

  expect_equal(createMassPeaks(mass=1:10, intensity=1:10),
               new("MassPeaks", mass=1:10, intensity=1:10,
                   snr=as.double(rep(NA, 10)), metaData=list()))
  expect_equal(createMassPeaks(1:10, 1:10, snr=1:10, meta),
               new("MassPeaks", mass=1:10, intensity=1:10, snr=1:10,
                   metaData=meta))
})
