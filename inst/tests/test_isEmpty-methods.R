context("isEmpty")

s <- c(createMassSpectrum(mass=1:10, intensity=11:20),
       createMassSpectrum(mass=1:2, intensity=rep.int(.Machine$integer.max, 2)))
e <- c(createMassSpectrum(mass=double(), intensity=double()),
       createMassSpectrum(mass=1:10, intensity=rep(0, 10)),
       createMassSpectrum(mass=1:10, intensity=as.double(rep(NA, 10))))

test_that("isEmpty", {
  for (i in seq(along=e)) {
    expect_true(isEmpty(e[[i]]))
  }
  for (i in seq(along=s)) {
    expect_false(isEmpty(s[[i]]))
  }
})

test_that("isEmptyWarning", {
  expect_warning(MALDIquant:::.isEmptyWarning(e[[1]]), "empty")
  expect_true(suppressWarnings(MALDIquant:::.isEmptyWarning(e[[1]])))
  expect_false(MALDIquant:::.isEmptyWarning(s[[1]]))
})

