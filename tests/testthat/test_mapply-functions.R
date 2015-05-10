context(".mapply")

test_that(".mapply", {
  a <- setNames(1L:10L, LETTERS[1L:10L])
  b <- 1L:2L
  f <- function(x, y)x+y

  expect_identical(MALDIquant:::.mapply(f, a, b),
                   mapply(f, a, b, USE.NAMES=FALSE, SIMPLIFY=FALSE))
})
