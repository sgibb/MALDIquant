context("estimateBaseline")

i <- rep(10:1, 2)
s <- createMassSpectrum(mass=1:20, intensity=i)
m <- matrix(c(1:20, rep(NA, 20)), ncol=2, byrow=FALSE,
            dimnames=list(list(), list("x", "y")))

test_that("estimateBaseline throws errors", {
  expect_error(estimateBaseline(s, method="foobar"),
               ".*arg.* should be one of .*SNIP.*, .*ConvexHull.*, .*median.*")
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
  expect_identical(estimateBaseline(s, method="median", halfWindowSize=2), m)

  ## halfWindowSize
  expect_error(MALDIquant:::.estimateBaselineMedian(1:20, i, 0),
               "too small")
  expect_error(MALDIquant:::.estimateBaselineMedian(1:20, i, 20),
               "too large")
})

test_that("estimateBaselineSnip", {
  mlist <- list(decreasing=list(m=m, m100=m), increasing=list(m=m, m100=m))
  mlist$decreasing$m[, 2] <- c(10:1, c(2.5, 4.0, 5.5), 7:1)
  mlist$decreasing$m100[, 2] <- c(10:1, rep(c(1.25, 1.5, 1.75, 1.375, 1), times=2))
  mlist$increasing$m[, 2] <- c(10:1, 5:4, 5.5, 7:1)
  mlist$increasing$m100[, 2] <- c(10:1, 3.75, 3.375, 3:1, 2.5, 4:1)

  ## test default decreasing argument
  expect_identical(MALDIquant:::.estimateBaselineSnip(1:20, i, 2),
                   mlist$decreasing$m)

  for (j in seq(along=mlist)) {
    d <- names(mlist)[j] == "decreasing"

    ## C implementation
    expect_identical(MALDIquant:::.estimateBaselineSnip(1:20, i, 2,
                                                        decreasing=d),
                     mlist[[j]]$m)
    expect_equal(MALDIquant:::.estimateBaselineSnip(1:20, i, decreasing=d),
                 mlist[[j]]$m100)
    ## obsolete (slow) R implementation
    expect_identical(MALDIquant:::.snipR(1:20, i, 2, decreasing=d),
                     mlist[[j]]$m)

    ## user method
    colnames(mlist[[j]]$m100) <- colnames(mlist[[j]]$m) <- c("mass", "intensity")
    expect_identical(estimateBaseline(s, method="SNIP", iterations=2,
                                      decreasing=d), mlist[[j]]$m)
    expect_identical(estimateBaseline(s, iterations=2,
                                      decreasing=d), mlist[[j]]$m)
    expect_equal(estimateBaseline(s, decreasing=d), mlist[[j]]$m100)
  }
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

