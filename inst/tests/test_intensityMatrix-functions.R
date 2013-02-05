context("intensityMatrix")

p <- list(createMassPeaks(mass=1:4, intensity=11:14),
          createMassPeaks(mass=2:5, intensity=22:25))

m <- matrix(c(11:14, NA, NA, 22:25), byrow=T, ncol=5, nrow=2,
            dimnames=list(NULL, 1:5))

test_that("intensityMatrix throws errors", {
  expect_error(intensityMatrix(p[[1]]),
               "no list of MALDIquant::AbstractMassObject objects")
  expect_error(intensityMatrix(list()),
               "no list of MALDIquant::AbstractMassObject objects")
})

test_that("intensityMatrix", {
    expect_identical(intensityMatrix(p), m)
})

