context("totalIonCurrent")

s <- createMassSpectrum(mass=1:10, intensity=1:10)
i <- createMassSpectrum(mass=1:2, intensity=rep.int(.Machine$integer.max, 2))
e <- createMassSpectrum(mass=1:10, intensity=rep(0, 10))

test_that("totalIonCurrent", {
  expect_equal(totalIonCurrent(s), 49.5)
  expect_equal(totalIonCurrent(i), as.double(.Machine$integer.max))
})
