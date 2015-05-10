context("morphological filters")

x <- list(c(8, 6, 2, 4, 5, 7, 1, 10),
          c(15, 8, 4, 11, 10, 1, 13, 12, 6, 9, 5, 3, 2, 7, 14))

e <- list(c(rep(2:1, each=4)),
          c(rep(4, 3), rep(1, 5), 5, 3, rep(2, 5)))

d <- list(c(rep(8, 3), rep(7, 2), rep(10, 3)),
          c(rep(15, 3), 11, rep(13, 5), 12, rep(9, 2), rep(14, 3)))

test_that(".erosion", {
  for (i in seq(along=x)) {
    expect_equal(.erosion(x[[i]], halfWindowSize=2), e[[i]])
  }
})

test_that(".dilation", {
  for (i in seq(along=x)) {
    expect_equal(.dilation(x[[i]], halfWindowSize=2), d[[i]])
  }
})
