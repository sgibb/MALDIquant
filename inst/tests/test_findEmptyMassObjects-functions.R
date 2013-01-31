context("findEmptyMassObjects")

s <- createMassSpectrum(mass=1:10, intensity=1:10)
e <- createMassSpectrum(double(), double())

test_that("findEmptyMassObjects throws errors", {
  expect_error(findEmptyMassObjects(e),
               "no list of MALDIquant::AbstractMassObject objects")
  expect_error(findEmptyMassObjects(list()),
               "no list of MALDIquant::AbstractMassObject objects")
})

test_that("findEmptyMassObjects", {
  expect_identical(findEmptyMassObjects(list(e, e)), 1L:2L)
  expect_identical(findEmptyMassObjects(list(s, s)), integer())
  expect_identical(findEmptyMassObjects(list(s, e)), 2L)
  expect_identical(findEmptyMassObjects(list(e, s)), 1L)
})
