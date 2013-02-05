context("isEmpty")

s <- createMassSpectrum(mass=1:10, intensity=11:20)
e <- createMassSpectrum(mass=double(), intensity=double())

test_that("isEmpty", {
  expect_true(isEmpty(e))
  expect_false(isEmpty(s))
})

test_that("isEmptyWarning", {
  expect_warning(MALDIquant:::.isEmptyWarning(e), "empty")
  expect_true(suppressWarnings(MALDIquant:::.isEmptyWarning(e)))
  expect_false(MALDIquant:::.isEmptyWarning(s))
})

