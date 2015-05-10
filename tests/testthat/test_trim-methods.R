context("trim")

s <- createMassSpectrum(mass=1:10, intensity=11:20)

test_that("trim throws errors", {
  expect_error(trim(s, range=1:10), "has to be a vector of length 2")
  expect_error(trim(s, range=1), "has to be a vector of length 2")
  expect_error(trim(c(s, createMassSpectrum(mass=21:30, intensity=1:10))),
               "No overlap")
})

test_that("trim throws warnings", {
  expect_warning(trim(s, range=c(20, 30)), "No data points left")
})

test_that("trim", {
  expect_equal(trim(s, c(2, 9)),
         createMassSpectrum(mass=2:9, intensity=12:19))
})

test_that("trim works with list of AbstractMassObject objects", {
  r <- createMassSpectrum(mass=2:9, intensity=12:19)
  expect_equal(trim(list(s, s), c(2, 9)), list(r, r))
  expect_equal(trim(list(s, r)), list(r, r))
})
