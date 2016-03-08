context("estimateNoise-methods")

s <- createMassSpectrum(mass=1:20, intensity=rep(10:1, 2))

test_that("estimateNoise throws errors", {
  expect_error(estimateNoise(s, method="foobar"),
               ".*arg.* should be one of .*MAD.*, .*MovingMAD.*, .*SuperSmoother.*")
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
  expect_identical(MALDIquant:::.estimateNoise(mass(s), intensity(s)), n)
  expect_identical(MALDIquant:::.estimateNoise(mass(s), intensity(s),
                                               method="MAD"), n)
  expect_identical(estimateNoise(s), m)
  expect_identical(estimateNoise(s, method="MAD"), m)
})

test_that(".estimateNoiseMovingMad", {
  n <- .colMads(t(embed(intensity(s), 5)))
  n <- c(rep(n[1], 2), n, rep(n[length(n)], 2))
  m <- cbind(mass=mass(s), intensity=n)
  expect_identical(MALDIquant:::.estimateNoiseMovingMad(mass(s), intensity(s), halfWindowSize=2), n)
  expect_identical(MALDIquant:::.estimateNoise(mass(s), intensity(s),
                                               halfWindowSize=2,
                                               method="MovingMAD"), n)
  expect_identical(estimateNoise(s, method="MovingMAD", halfWindowSize=2), m)
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
