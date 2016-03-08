context("topN methods")

p <- createMassPeaks(mass=1:10, intensity=11:20, snr=rep(1, 10),
                     metaData=list(file="foo"))

test_that("topN throws errors", {
  expect_error(topN(p), ".*n.* is missing, with no default")
  expect_error(topN(p, "foo"), ".*n.* has to be an integer of length 1")
  expect_error(topN(p, 1:10), ".*n.* has to be an integer of length 1")
})

test_that("topN", {
  expect_equal(topN(p, 2), p[9:10])
  expect_equal(topN(p, 10), p)
})

test_that("topN,list throws errors", {
  expect_error(topN(list(x=1, y=1)), "no list of MALDIquant::MassPeaks objects")
})

test_that("topN,list", {
  expect_equal(topN(list(p, p), 2), list(p[9:10], p[9:10]))
  expect_equal(topN(list(p, p), 10), list(p, p))
})
