context("as")

test_that("as", {
  p <- createMassPeaks(mass=1:10, intensity=11:20, snr=rep(1, 10),
                       metaData=list(file="foo"))
  s <- createMassSpectrum(mass=1:10, intensity=11:20, metaData=list(file="foo"))
  expect_equal(as(p, "MassSpectrum"), s)
})
