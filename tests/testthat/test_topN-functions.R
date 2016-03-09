context(".topN")

test_that(".topNIndices", {
  x <- c(100, 3, 4, 1, 100, 99, 99, 99, 98, 98, 4, 3)
  ## note it is expected that n == 1 returns the highest element in x
  ## if it occures multiple times it should returned multiple times
  expect_equal(MALDIquant:::.topNIndices(x, 1), c(1, 5))
  expect_equal(MALDIquant:::.topNIndices(x, 2), c(1, 5))
  expect_equal(MALDIquant:::.topNIndices(x, 3), c(1, 5:8))
  expect_equal(MALDIquant:::.topNIndices(x, 5), c(1, 5:8))
  expect_equal(MALDIquant:::.topNIndices(x, 7), c(1, 5:10))
  expect_equal(MALDIquant:::.topNIndices(x, 100), seq_along(x))
})
