context("isMassObject")

s <- createMassSpectrum(mass=1:10, intensity=1:10)
p <- createMassPeaks(mass=1:10, intensity=1:10)

test_that(".isMassObject", {
  expect_false(MALDIquant:::.isMassObject(double()))
  expect_false(MALDIquant:::.isMassObject(list()))

  expect_true(MALDIquant:::.isMassObject(s))
  expect_true(MALDIquant:::.isMassObject(p))
})

test_that("isMassSpectrum", {
  expect_false(isMassSpectrum(double()))
  expect_false(isMassSpectrum(list()))
  expect_false(isMassSpectrum(p))

  expect_true(isMassSpectrum(s))
})

test_that("isMassPeaks", {
  expect_false(isMassPeaks(double()))
  expect_false(isMassPeaks(list()))
  expect_false(isMassPeaks(s))

  expect_true(isMassPeaks(p))
})
