context("removeBaseline")

s <- createMassSpectrum(mass=1:20, intensity=rep(10:1, 2))
e <- createMassSpectrum(double(), double())

test_that("removeBaseline throws errors", {
  expect_error(removeBaseline(s, method="foobar"),
               ".*arg.* should be one of .*SNIP.*, .*ConvexHull.*, .*median.*")
})

test_that("removeBaseline shows warnings", {
  expect_warning(estimateBaseline(e), "empty")
})

test_that("removeBaseline returns an empty spectrum if input is empty", {
  expect_identical(suppressWarnings(removeBaseline(e)), e)
})

test_that("removeBaseline works with ConvexHull", {
  expect_equal(removeBaseline(s, method="ConvexHull"),
               createMassSpectrum(mass=1:20, intensity=c(rep(0, 10), 9:0)))
})

test_that("removeBaseline works with median", {
  ## halfWindowSize
  expect_error(removeBaseline(s, method="median", halfWindowSize=0),
               "too small")
  expect_error(removeBaseline(s, method="median"),
               "too large")
  expect_equal(removeBaseline(s, method="median", halfWindowSize=2),
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
