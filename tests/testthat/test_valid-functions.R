context("valid")

test_that(".validAbstractMassObject", {
  s <- createMassSpectrum(mass=1:10, intensity=1:10)
  expect_true(MALDIquant:::.validAbstractMassObject(s))
  s@intensity <- 1
  expect_identical(MALDIquant:::.validAbstractMassObject(s),
                   "Lengths of mass (10) and intensity (1) have to be equal.")
  s@mass <- -1
  expect_warning(MALDIquant:::.validAbstractMassObject(s),
                 "Negative mass values found.")
  s@mass <- 1
  s@intensity <- -1
  expect_warning(MALDIquant:::.validAbstractMassObject(s),
                 "Negative intensity values found.")
})

test_that(".validMassPeaks", {
  p <- createMassPeaks(mass=1:10, intensity=1:10, snr=1:10)

  expect_true(MALDIquant:::.validMassPeaks(p))
  p@intensity <- 1
  expect_identical(MALDIquant:::.validMassPeaks(p),
                   "Lengths of intensity (1) and snr (10) have to be equal.")
})
