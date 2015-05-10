context("removeEmptyMassObjects")

s <- createMassSpectrum(mass=1:10, intensity=1:10)
e <- createMassSpectrum(double(), double())

test_that("removeEmptyMassObjects throws errors", {
  expect_error(removeEmptyMassObjects(e),
               "no list of MALDIquant::AbstractMassObject objects")
  expect_error(removeEmptyMassObjects(list()),
               "no list of MALDIquant::AbstractMassObject objects")
})

test_that("removeEmptyMassObjects", {
  expect_identical(removeEmptyMassObjects(list(e, e)), list())
  expect_identical(removeEmptyMassObjects(list(s, s)), list(s, s))
  expect_identical(removeEmptyMassObjects(list(s, e)), list(s))
  expect_identical(removeEmptyMassObjects(list(e, s)), list(s))
})
