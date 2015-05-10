context("warp")

s <- createMassSpectrum(mass=1:10, intensity=1:10)
p <- createMassPeaks(mass=1:10, intensity=1:10, snr=1:10)

test_that("warpMassSpectra throws errors", {
  expect_error(warpMassSpectra(p, function(x)1),
               "no list of MALDIquant::MassSpectrum objects")
  expect_error(warpMassSpectra(s, function(x)1),
               "no list of MALDIquant::MassSpectrum objects")
  expect_error(warpMassSpectra(list(s), 1),
               "no list of functions")
})

test_that("warpMassSpectra", {
  expect_equal(warpMassSpectra(list(s, s), list(function(x)1, function(x)x+1)),
               list(createMassSpectrum(mass=2:11, intensity=1:10),
                    createMassSpectrum(mass=seq(3, 21, by=2), intensity=1:10)))
})

test_that("warpMassPeaks throws errors", {
  expect_error(warpMassPeaks(p, function(x)1),
               "no list of MALDIquant::MassPeaks objects")
  expect_error(warpMassPeaks(s, function(x)1),
               "no list of MALDIquant::MassPeaks objects")
  expect_error(warpMassPeaks(list(p), 1),
               "no list of functions")
})

test_that("warpMassPeaks", {
  expect_equal(warpMassPeaks(list(p, p), list(function(x)1, function(x)x+1)),
               list(createMassPeaks(mass=2:11, intensity=1:10, snr=1:10),
                    createMassPeaks(mass=seq(3, 21, by=2), intensity=1:10,
                                    snr=1:10)))
})

test_that(".warp", {
  expect_equal(MALDIquant:::.warp(list(s, s),
                                  list(function(x)1, function(x)x+1)),
               list(createMassSpectrum(mass=2:11, intensity=1:10),
                    createMassSpectrum(mass=seq(3, 21, by=2), intensity=1:10)))
})
