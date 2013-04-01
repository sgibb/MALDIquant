context("removeBaseline")

s <- createMassSpectrum(mass=1:20, intensity=rep(10:1, 2))

test_that("removeBaseline throws errors", {
  expect_error(removeBaseline(s, method="foobar"),
               ".*arg.* should be one of .*SNIP.*, .*ConvexHull.*, .*Median.*")

  wrongBaseline <- function(x, y, ...) { return(0) }
  expect_error(removeBaseline(s, fun=wrongBaseline),
               "The baseline is not a valid matrix!")

  wrongBaseline <- function(x, y, ...) {
    return(do.call(cbind, list(x, rep(1, length(x)), rep(2, length(x)))))
  }
  expect_error(removeBaseline(s, fun=wrongBaseline),
               "The baseline is not a valid matrix!")

})

test_that("removeBaseline shows warnings", {
  expect_warning(estimateBaseline(
                   createMassSpectrum(mass=double(), intensity=double()),
                 "empty"))
})

test_that("removeBaseline works with ConvexHull", {
  expect_equal(removeBaseline(s, method="ConvexHull"),
               createMassSpectrum(mass=1:20, intensity=c(rep(0, 10), 9:0)))
})

test_that("removeBaseline works with Median", {
  ## halfWindowSize
  expect_error(removeBaseline(s, method="Median", halfWindowSize=0),
               "too small")
  expect_error(removeBaseline(s, method="Median"),
               "too large")
  expect_equal(removeBaseline(s, method="Median", halfWindowSize=2),
               createMassSpectrum(mass=1:20, intensity=c(rep(0, 8), -1:-2, 2:1,
                                                         rep(0, 8))))
})

test_that("removeBaseline works with SNIP", {
  expect_equal(removeBaseline(s, iterations=2),
               createMassSpectrum(mass=1:20, intensity=c(rep(0, 10), 7.5, 5, 2.5,
                                                         rep(0, 7))))
  expect_equal(removeBaseline(s, method="SNIP", iterations=2),
               createMassSpectrum(mass=1:20, intensity=c(rep(0, 10), 7.5, 5, 2.5,
                                                         rep(0, 7))))
})

test_that("removeBaseline works with list of MassSpectrum objects", {
  expect_error(removeBaseline(list(x=1, y=1)),
               "no list of MALDIquant::MassSpectrum objects")
  expect_error(removeBaseline(list(s, createMassPeaks(1, 1)),
               "no list of MALDIquant::MassSpectrum objects"))
  r <- createMassSpectrum(mass=1:20,
                          intensity=c(rep(0, 10), 7.5, 5, 2.5, rep(0, 7)))
  expect_equal(removeBaseline(list(s, s), iterations=2), list(r, r))
})

