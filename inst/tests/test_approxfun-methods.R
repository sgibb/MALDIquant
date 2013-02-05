context("approxfun")

test_that("approxfun", {
  s <- createMassSpectrum(mass=1:10, intensity=11:20)
  p <- createMassPeaks(mass=1:10, intensity=11:20)

  expect_error(MALDIquant:::approxfun(p))

  a <- approxfun(s)
  expect_equal(a(seq(0, 11, by=0.5)), c(NA, NA, seq(11, 20, by=0.5), NA, NA))
})

