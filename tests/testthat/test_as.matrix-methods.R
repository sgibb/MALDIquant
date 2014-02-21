context("as.matrix")

m <- matrix(c(1:10, 11:20), ncol=2, dimnames=list(c(), c("mass", "intensity")))
s <- createMassSpectrum(mass=1:10, intensity=11:20)
p <- createMassPeaks(mass=1:10, intensity=11:20)

test_that("as.matrix works without indices", {
  expect_equal(as.matrix(s), m)
  expect_equal(as.matrix(p), m)
})

test_that("as.matrix works with indices", {
  expect_equal(as.matrix(s)[3:4,], m[3:4,])
  expect_equal(as.matrix(s)[7, 1], m[7, 1])
  expect_equal(as.matrix(s)[mass(s) > 5, 2], 16:20)
})
