context("isMassObjectList")

s <- createMassSpectrum(mass=1:10, intensity=1:10)
p <- createMassPeaks(mass=1:10, intensity=1:10)

test_that(".isMassObjectList", {
  expect_false(MALDIquant:::.isMassObjectList(double(2)))
  expect_false(MALDIquant:::.isMassObjectList(list()))
  expect_false(MALDIquant:::.isMassObjectList(s))
  expect_false(MALDIquant:::.isMassObjectList(p))

  expect_true(MALDIquant:::.isMassObjectList(list(s, s)))
  expect_true(MALDIquant:::.isMassObjectList(list(s, p)))
  expect_true(MALDIquant:::.isMassObjectList(list(p, p)))
})

test_that("isMassSpectrumList", {
  expect_false(isMassSpectrumList(double(2)))
  expect_false(isMassSpectrumList(list()))
  expect_false(isMassSpectrumList(s))
  expect_false(isMassSpectrumList(p))
  expect_false(isMassSpectrumList(list(s, p)))
  expect_false(isMassSpectrumList(list(p, p)))

  expect_true(isMassSpectrumList(list(s, s)))
})

test_that("isMassPeaksList", {
  expect_false(isMassPeaksList(double(2)))
  expect_false(isMassPeaksList(list()))
  expect_false(isMassPeaksList(s))
  expect_false(isMassPeaksList(p))
  expect_false(isMassPeaksList(list(s, s)))
  expect_false(isMassPeaksList(list(s, p)))

  expect_true(isMassPeaksList(list(p, p)))
})
