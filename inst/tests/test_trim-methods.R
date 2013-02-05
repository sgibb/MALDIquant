context("trim")

s <- createMassSpectrum(mass=1:10, intensity=11:20)

test_that("trim", {
  expect_equal(trim(s, 2, 9),
         createMassSpectrum(mass=2:9, intensity=12:19))
  expect_equal(trim(s, minMass=2),
         createMassSpectrum(mass=2:10, intensity=12:20))
  expect_equal(trim(s, maxMass=9),
         createMassSpectrum(mass=1:9, intensity=11:19))
})

test_that("ltrim", {
  expect_equal(ltrim(s, 2), createMassSpectrum(mass=2:10, intensity=12:20))
})

test_that("rtrim", {
  expect_equal(rtrim(s, 9), createMassSpectrum(mass=1:9, intensity=11:19))
})

test_that("trim works with list of AbstractMassObject objects", {
  r <- createMassSpectrum(mass=2:9, intensity=12:19)
  expect_equal(trim(list(s, s), 2, 9), list(r, r))
})

