context("estimateBaseline")

i <- rep(10:1, 2)
s <- createMassSpectrum(mass=1:20, intensity=i)
m <- matrix(c(1:20, rep(NA, 20)), ncol=2, byrow=FALSE,
            dimnames=list(list(), list("x", "y")))

test_that("estimateBaseline throws errors", {
  expect_error(estimateBaseline(s, method="foobar"),
               ".*arg.* should be one of .*SNIP.*, .*ConvexHull.*, .*Median.*")
})

test_that("estimateBaseline shows warnings", {
  expect_warning(estimateBaseline(
                   createMassSpectrum(mass=double(), intensity=double()),
                 "empty"))
})

test_that("estimateBaselineConvexHull", {
  m[, 2] <- c(10:1, rep(1, 10))
  ## C implementation
  expect_identical(MALDIquant:::.estimateBaselineConvexHull(1:20, i), m)
  ## obsolete (slow) R implementation
  expect_identical(MALDIquant:::.lowerConvexHullR(1:20, i), m)
  ## user method
  colnames(m) <- c("mass", "intensity")
  expect_identical(estimateBaseline(s, method="ConvexHull"), m)
})

test_that("estimateBaselineMedian", {
  m[, 2] <- c(10:4, rep(c(3, 8), each=3), 7:1)
  m1 <- m
  m1[, 2] <- c(10:3, rep(c(2, 9), each=2), 8:1)
  expect_identical(MALDIquant:::.estimateBaselineMedian(1:20, i, 1), m1)
  expect_identical(MALDIquant:::.estimateBaselineMedian(1:20, i, 2), m)
  ## user method
  colnames(m) <- c("mass", "intensity")
  expect_identical(estimateBaseline(s, method="Median", halfWindowSize=2), m)

  ## halfWindowSize
  expect_error(MALDIquant:::.estimateBaselineMedian(1:20, i, 0),
               "too small")
  expect_error(MALDIquant:::.estimateBaselineMedian(1:20, i, 20),
               "too large")
})

test_that("estimateBaselineSnip", {
  m[, 2] <- c(10:1, 5:4, 5.5, 7:1)
  m100 <- m
  m100[, 2] <- c(10:1, 3.75, 3.375, 3:1, 2.5, 4:1)
  ## C implementation
  expect_identical(MALDIquant:::.estimateBaselineSnip(1:20, i, 2), m)
  expect_equal(MALDIquant:::.estimateBaselineSnip(1:20, i), m100)
  ## obsolete (slow) R implementation
  expect_identical(MALDIquant:::.snipR(1:20, i, 2), m)

  ## user method
  colnames(m100) <- colnames(m) <- c("mass", "intensity")
  expect_identical(estimateBaseline(s, method="SNIP", iterations=2), m)
  expect_identical(estimateBaseline(s, iterations=2), m)
  expect_equal(estimateBaseline(s), m100)
})

test_that("estimateBaselineTopHat", {
  m[, 2] <- c(rep(8, 3), 7:1, rep(6, 5), 5:1)
  m1 <- m
  m1[, 2] <- c(rep(9, 2), 8:1, rep(8, 3), 7:1)
  ## C implementation
  expect_identical(MALDIquant:::.estimateBaselineTopHat(1:20, i, 1), m1)
  expect_identical(MALDIquant:::.estimateBaselineTopHat(1:20, i, 2), m)
  ## obsolete (slow) R implementation
  expect_equal(MALDIquant:::.topHatR(1:20, i, 2), m)

  ## user method
  colnames(m) <- c("mass", "intensity")
  expect_identical(estimateBaseline(s, method="TopHat", halfWindowSize=2), m)

  ## halfWindowSize
  expect_error(MALDIquant:::.estimateBaselineTopHat(1:20, i, 0),
               "too small")
  expect_error(MALDIquant:::.estimateBaselineTopHat(1:20, i, 20),
               "too large")

})

