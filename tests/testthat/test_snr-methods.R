context("snr")

s <- createMassPeaks(mass=1:10, intensity=11:20, snr=rep(2, 10))

test_that("snr", {
  expect_identical(snr(s), rep(2, 10))
})
