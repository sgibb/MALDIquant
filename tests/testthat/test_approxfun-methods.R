context("approxfun")

test_that("approxfun", {
  s <- createMassSpectrum(mass=1:10, intensity=11:20)
  e <- createMassSpectrum(mass=double(), intensity=double())
  p <- createMassPeaks(mass=1:10, intensity=11:20)

  expect_error(MALDIquant:::approxfun(p))

  expect_equal(approxfun(s)(seq(0, 11, by=0.5)),
               c(NA, NA, seq(11, 20, by=0.5), NA, NA))
  expect_equal(approxfun(e)(1:20), rep(NA, 20))
})
