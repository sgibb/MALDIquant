context("imputeMass")

m <- list(createMassSpectrum(mass=(1:10)[-8], intensity=rep(1, 9)),
          createMassSpectrum(mass=c(1, 2, 4, 7, 11, 16, 22, 37, 46),
                             intensity=rep(1, 9)))

s <- list(createMassSpectrum(mass=1:10, intensity=c(rep(1, 7), 0, 1, 1)),
          createMassSpectrum(mass=c(1, 2, 4, 7, 11, 16, 22, 29, 37, 46),
                             intensity=c(rep(1, 7), 0, 1, 1)))

test_that("imputeMass shows warnings", {
  expect_warning(MALDIquant:::imputeMass(
                   createMassSpectrum(mass=double(), intensity=double()),
                 "empty"))
})

test_that("imputeMass", {
  expect_equal(MALDIquant:::imputeMass(m[[1]]), s[[1]])
  expect_equal(MALDIquant:::imputeMass(m[[2]]), s[[2]], tolerance=0.02)
})

