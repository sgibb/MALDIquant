context("calibrate")

m <- matrix(c(1, 2, 3,
              3, 6, 9), nrow=2, byrow=TRUE,
            dimnames=list(paste("samples", 1:2, sep=""),
                          paste("peaks", LETTERS[1:3])))

test_that("calibrate throws errors", {
  expect_error(calibrate(list()))
  expect_error(calibrate(createMassPeaks(mass=1:10, intensity=1:10)))
})

test_that("calibrate throws errors", {
  expect_true(all(as.numeric(calibrate(m))==rep(c(2, 4, 6), each=2)))
  expect_true(all(attr(calibrate(m), "scale")==c(0.5, 1.5)))
})

