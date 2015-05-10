context("subset")

s <- createMassSpectrum(mass=1:10, intensity=11:20)
p <- createMassPeaks(mass=1:10, intensity=11:20, snr=21:30)

test_that("numerical index based subsetting works on MassSpectrum", {
  expect_equal(s[2:9], createMassSpectrum(mass=2:9, intensity=12:19))
  expect_equal(s[-1], createMassSpectrum(mass=2:10, intensity=12:20))
})

test_that("logical index based subsetting works on MassSpectrum", {
  expect_equal(s[mass(s) <= 9],
               createMassSpectrum(mass=1:9, intensity=11:19))
  expect_equal(s[intensity(s) <= 15],
               createMassSpectrum(mass=1:5, intensity=11:15))
})

test_that("numerical index based subsetting works on MassPeaks", {
  expect_equal(p[2:9], createMassPeaks(mass=2:9, intensity=12:19, snr=22:29))
  expect_equal(p[-1], createMassPeaks(mass=2:10, intensity=12:20, snr=22:30))
})

test_that("logical index based subsetting works on MassPeaks", {
  expect_equal(p[mass(p) <= 9],
               createMassPeaks(mass=1:9, intensity=11:19, snr=21:29))
  expect_equal(p[intensity(p) <= 15],
               createMassPeaks(mass=1:5, intensity=11:15, snr=21:25))
})
