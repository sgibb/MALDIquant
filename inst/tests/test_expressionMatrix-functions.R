context("intensityMatrix")

p <- list(createMassPeaks(mass=1:4, intensity=11:14),
          createMassPeaks(mass=2:5, intensity=22:25))
s <- list(createMassSpectrum(mass=1:5, intensity=11:15),
          createMassSpectrum(mass=1:5, intensity=21:25))

b <- matrix(c(rep(1L, 4L), rep(0L, 2L), rep(1L, 4L)), byrow=T, ncol=5, nrow=2,
            dimnames=list(NULL, 1:5))
m <- matrix(c(11:15, 21:25), byrow=T, ncol=5, nrow=2,
            dimnames=list(NULL, 1:5))

test_that("expressionMatrix throws errors", {
  expect_error(expressionMatrix(p[[1]]),
               "no list of MALDIquant::MassPeaks objects")
  expect_error(expressionMatrix(list()),
               "no list of MALDIquant::MassPeaks objects")
  expect_error(expressionMatrix(p, list()),
               "no list of MALDIquant::MassSpectrum objects")
  expect_error(expressionMatrix(p, s[1]),
               "Incompatible number of spectra")
})

test_that("expressionMatrix", {
    expect_identical(expressionMatrix(p), b)
    expect_equal(expressionMatrix(p, s), m)
})

