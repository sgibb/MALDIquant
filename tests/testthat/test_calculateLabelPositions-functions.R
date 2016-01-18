context("calculateLabelPositions")

test_that(".textLabelRects", {
  pdf(NULL)
  on.exit(dev.off())
  plot.new()

  r <- structure(c(0.97851023195729, 0.956043656276274, 0.994765602915447,
0.994765602915447, 1.02148976804271, 1.04395634372373, 1.03140638250732,
1.03140638250732, 1, 1, 1, 1, 0.0429795360854208, 0.0879126874474516,
0.0261719854227628, 0.0261719854227628), .Dim = c(2L, 8L), .Dimnames = list(
    NULL, c("x0", "y0", "x1", "y1", "x", "y", "w", "h")))

  expect_equal(MALDIquant:::.textLabelRects(c(1, 1), c(1, 1),
                                            c("foo", "foobar")), r,
               tolerance = 0.02)
})

test_that(".overlaps", {
  target <- c(2, 2, 4, 3)         # x1,y1 == bottom left
  dest <- list(a=target,          # identical
               b=c(1, 1, 3, 2.5), # overlap
               c=c(3, 1, 6, 4),   # overlap
               d=c(2, 4, 3, 5),   # no overlap
               e=c(6, 0, 8, 1))   # no overlap
  dest2 <- list(rbind(dest$a, dest$b), # overlap
                rbind(dest$b, dest$e), # overlap
                rbind(dest$d, dest$e)) # no overlap

  r <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
  r2 <- c(TRUE, TRUE, FALSE)

  for (i in seq(along=dest)) {
    expect_identical(MALDIquant:::.overlaps(target, dest[[i]]), r[i])
  }
  for (i in seq(along=dest2)) {
    expect_identical(MALDIquant:::.overlaps(target, dest2[[i]]), r2[i])
  }

})
