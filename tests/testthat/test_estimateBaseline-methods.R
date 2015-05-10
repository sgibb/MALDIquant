context("estimateBaseline")

s <- createMassSpectrum(mass=1:20, intensity=rep(10:1, 2))

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
  b <- c(10:1, rep(1, 10))
  m <- cbind(mass=mass(s), intensity=b)
  expect_identical(MALDIquant:::.estimateBaselineConvexHull(mass(s),
                                                            intensity(s)), b)
  ## user method
  expect_identical(estimateBaseline(s, method="ConvexHull"), m)
})

test_that("estimateBaselineMedian", {
  b <- c(10:4, rep(c(3, 8), each=3), 7:1)
  b1 <- c(10:3, rep(c(2, 9), each=2), 8:1)
  m <- cbind(mass=mass(s), intensity=b)
  expect_identical(MALDIquant:::.estimateBaselineMedian(mass(s), intensity(s),
                                                        1), b1)
  expect_identical(MALDIquant:::.estimateBaselineMedian(mass(s), intensity(s),
                                                        2), b)
  ## user method
  expect_identical(estimateBaseline(s, method="median", halfWindowSize=2), m)

  ## halfWindowSize
  expect_error(MALDIquant:::.estimateBaselineMedian(mass(s), intensity(s), 0),
               "too small")
  expect_error(MALDIquant:::.estimateBaselineMedian(mass(s), intensity(s), 20),
               "too large")
})

test_that("estimateBaselineSnip", {
  b <- list(decreasing=list(b=c(10:1, c(2.5, 4.0, 5.5), 7:1),
                            b100=c(10:1, rep(c(1.25, 1.5, 1.75, 1.375, 1),
                                             times=2))),
            increasing=list(b=c(10:1, 5:4, 5.5, 7:1),
                            b100=c(10:1, 3.75, 3.375, 3:1, 2.5, 4:1)))

  ## test default decreasing argument
  expect_identical(MALDIquant:::.estimateBaselineSnip(mass(s), intensity(s), 2),
                   b$decreasing$b)

  for (j in seq(along=b)) {
    d <- names(b)[j] == "decreasing"

    expect_identical(MALDIquant:::.estimateBaselineSnip(mass(s), intensity(s),
                                                        2, decreasing=d),
                     b[[j]]$b)
    expect_equal(MALDIquant:::.estimateBaselineSnip(mass(s), intensity(s),
                                                    decreasing=d),
                 b[[j]]$b100)

    ## user method
    m <- cbind(mass=mass(s), intensity=b[[j]]$b)
    m100 <- cbind(mass=mass(s), intensity=b[[j]]$b100)
    expect_identical(estimateBaseline(s, method="SNIP", iterations=2,
                                      decreasing=d), m)
    expect_identical(estimateBaseline(s, iterations=2, decreasing=d), m)
    expect_equal(estimateBaseline(s, decreasing=d), m100)
  }
})

test_that("estimateBaselineTopHat", {
  b <- c(rep(8, 3), 7:1, rep(6, 5), 5:1)
  b1 <- c(rep(9, 2), 8:1, rep(8, 3), 7:1)
  m <- cbind(mass=mass(s), intensity=b)
  expect_identical(MALDIquant:::.estimateBaselineTopHat(mass(s), intensity(s),
                                                        1), b1)
  expect_identical(MALDIquant:::.estimateBaselineTopHat(mass(s), intensity(s),
                                                        2), b)

  ## user method
  expect_identical(estimateBaseline(s, method="TopHat", halfWindowSize=2), m)

  ## halfWindowSize
  expect_error(MALDIquant:::.estimateBaselineTopHat(mass(s), intensity(s), 0),
               "too small")
  expect_error(MALDIquant:::.estimateBaselineTopHat(mass(s), intensity(s), 20),
               "too large")

})
