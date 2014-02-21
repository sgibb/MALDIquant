context("estimateNoise-methods")

i <- rep(10:1, 2)
s <- createMassSpectrum(mass=1:20, intensity=i)
m <- matrix(c(1:20, rep(NA, 20)), ncol=2, byrow=FALSE,
            dimnames=list(list(), list("mass", "intensity")))

test_that("estimateNoise throws errors", {
  expect_error(estimateNoise(s, method="foobar"),
               ".*arg.* should be one of .*MAD.*, .*SuperSmoother.*")
})

test_that("estimateNoise shows warnings", {
  expect_warning(estimateNoise(
                      createMassSpectrum(mass=double(), intensity=double())),
                 "empty")
  expect_identical(suppressWarnings(estimateNoise(
                      createMassSpectrum(mass=double(), intensity=double()))),
                   0L)
})

test_that(".estimateNoiseMad", {
  m[, 2] <- stats::mad(i)
  expect_identical(unname(MALDIquant:::.estimateNoiseMad(1:20, i)),
                   unname(m))
  expect_identical(estimateNoise(s), m)
  expect_identical(estimateNoise(s, method="MAD"), m)
})

test_that(".estimateNoiseSuperSmoother", {
  m[, 2] <- stats::supsmu(x=1:20, y=i)$y
  expect_identical(unname(MALDIquant:::.estimateNoiseSuperSmoother(1:20, i)),
                   unname(m))
  expect_identical(estimateNoise(s, method="SuperSmoother"), m)
})
