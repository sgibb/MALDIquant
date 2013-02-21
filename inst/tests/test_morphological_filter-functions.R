context("morphological filters")

x <- c(15, 8, 4, 11, 10, 1, 13, 12, 6, 9, 5, 3, 2, 7, 14)

test_that(".erosion", {
  expect_identical(.erosion(x, halfWindowSize=2),
                   c(rep(4, 3), rep(1, 5), 5, 3, rep(2, 5)))
})

test_that(".dilation", {
  expect_identical(.dilation(x, halfWindowSize=2),
                   c(rep(15, 3), 11, rep(13, 5), 12, rep(9, 2), rep(14, 3)))
})

