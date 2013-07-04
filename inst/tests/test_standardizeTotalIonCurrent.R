context("standardizeTotalIonCurrent")

s <- list(createMassSpectrum(mass=1:10, intensity=1:10),
          createMassSpectrum(mass=1:10, intensity=2:11))

test_that("standardizeTotalIonCurrent throws errors", {
  expect_error(standardizeTotalIonCurrent(list(a=1, b=2)))
})

test_that("standardizeTotalIonCurrent", {
  sTIC <- standardizeTotalIonCurrent(s)
  expect_equal(totalIonCurrent(sTIC[[1]]), 1)
  expect_equal(totalIonCurrent(sTIC[[2]]), 1)
  sTIC <- standardizeTotalIonCurrent(s, 2)
  expect_equal(totalIonCurrent(sTIC[[1]]), 2)
  expect_equal(totalIonCurrent(sTIC[[2]]), 2)
})

