context("isEmpty")

s <- createMassSpectrum(mass=1:10, intensity=11:20)
e <- c(createMassSpectrum(mass=double(), intensity=double()),
       createMassSpectrum(mass=1:10, intensity=rep(0, 10)))

test_that("isEmpty", {
  for (i in seq(along=e)) {
    expect_true(isEmpty(e[[i]]))
  }
  expect_false(isEmpty(s))
})

test_that("isEmptyWarning", {
  expect_warning(MALDIquant:::.isEmptyWarning(e[[1]]), "empty")
  expect_true(suppressWarnings(MALDIquant:::.isEmptyWarning(e[[1]])))
  expect_false(MALDIquant:::.isEmptyWarning(s))
})

