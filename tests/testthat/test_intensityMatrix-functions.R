context("intensityMatrix")

p <- list(createMassPeaks(mass=1:4, intensity=11:14),
          createMassPeaks(mass=2:5, intensity=22:25))
s <- list(createMassSpectrum(mass=1:5, intensity=11:15),
          createMassSpectrum(mass=1:5, intensity=21:25))

m <- matrix(c(11:14, NA_real_, NA_real_, 22:25), byrow=TRUE, ncol=5, nrow=2,
            dimnames=list(NULL, 1:5))
e <- matrix(c(11:15, 21:25), byrow=TRUE, ncol=5, nrow=2,
            dimnames=list(NULL, 1:5))
attr(m, "mass") <- attr(e, "mass") <- 1:5

test_that("intensityMatrix throws errors", {
  expect_error(intensityMatrix(p[[1]]),
               "no list of MALDIquant::MassPeaks objects")
  expect_error(intensityMatrix(list()),
               "no list of MALDIquant::MassPeaks objects")
  expect_error(intensityMatrix(p, list()),
               "no list of MALDIquant::MassSpectrum objects")
  expect_error(intensityMatrix(p, s[1]),
               "Incompatible number of spectra")
})

test_that("intensityMatrix", {
    expect_identical(intensityMatrix(p), m)
    expect_equal(intensityMatrix(p, s), e)
})
