context(".findLocalMaximaLogical")

s <- createMassSpectrum(mass=1:5, intensity=c(1, 2, 1, 2, 1))

test_that(".findLocalMaximaLogical throws errors", {
  expect_error(MALDIquant:::.findLocalMaximaLogical(s, halfWindowSize=0),
               "too small")
  expect_error(MALDIquant:::.findLocalMaximaLogical(s, halfWindowSize=3),
               "too large")
})

test_that(".findLocalMaxima(Logical) shows warnings", {
  expect_warning(MALDIquant:::.findLocalMaximaLogical(
                   createMassSpectrum(mass=double(), intensity=double()),
                 "empty"))
  expect_warning(MALDIquant:::.findLocalMaxima(
                   createMassSpectrum(mass=double(), intensity=double()),
                 "empty"))
})

test_that(".findLocalMaximaLogical works with different window sizes", {
  expect_identical(suppressWarnings(MALDIquant:::.findLocalMaximaLogical(
                      createMassSpectrum(mass=double(), intensity=double()),
                      halfWindowSize=1)), logical())
  expect_identical(MALDIquant:::.findLocalMaximaLogical(s, halfWindowSize=1),
                   c(FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_identical(MALDIquant:::.findLocalMaximaLogical(s, halfWindowSize=2),
                   c(FALSE, TRUE, FALSE, FALSE, FALSE))
})

test_that(".findLocalMaxima returns matrix", {
  m <- matrix(ncol=2, dimnames=list(list(), list("mass", "intensity")))
  expect_equal(suppressWarnings(MALDIquant:::.findLocalMaxima(
                  createMassSpectrum(mass=double(), intensity=double()),
                  halfWindowSize=1)), m)
  m <- matrix(c(2, 2, 4, 2), ncol=2, byrow=TRUE,
              dimnames=list(list(), list("mass", "intensity")))
  expect_identical(MALDIquant:::.findLocalMaxima(s, halfWindowSize=1), m)
})
