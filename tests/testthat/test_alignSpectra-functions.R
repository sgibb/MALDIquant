context("alignSpectra")

test_that("alignSpectra throws errors", {
  expect_error(alignSpectra(list()),
               ".*spectra.* is no list of MALDIquant::MassSpectrum objects")
})

test_that("alignSpectra", {
  data(fiedler2009subset)
  spectra <- fiedler2009subset[1:8]

  peaks <- detectPeaks(spectra, halfWindowSize=20, method="MAD", SNR=2)
  wf <- determineWarpingFunctions(peaks, tolerance=0.002, method="lowess")
  expect_equal(alignSpectra(spectra), warpMassSpectra(spectra, wf))
})
