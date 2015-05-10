context("estimateNoise-methods")

s <- createMassSpectrum(mass=1:20, intensity=rep(10:1, 2))

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
  n <- rep(stats::mad(intensity(s)), length(s))
  m <- cbind(mass=mass(s), intensity=n)
  expect_identical(MALDIquant:::.estimateNoiseMad(1:20, intensity(s)), n)
  expect_identical(MALDIquant:::.estimateNoise(mass(s), intensity(s)), n)
  expect_identical(MALDIquant:::.estimateNoise(mass(s), intensity(s),
                                               method="MAD"), n)
  expect_identical(estimateNoise(s), m)
  expect_identical(estimateNoise(s, method="MAD"), m)
})

test_that(".estimateNoiseSuperSmoother", {
  n <- stats::supsmu(x=1:20, y=intensity(s))$y
  m <- cbind(mass=mass(s), intensity=n)
  expect_identical(MALDIquant:::.estimateNoiseSuperSmoother(mass(s),
                                                            intensity(s)), n)
  expect_identical(MALDIquant:::.estimateNoise(mass(s), intensity(s),
                                               method="SuperSmoother"), n)
  expect_identical(estimateNoise(s, method="SuperSmoother"), m)
})
