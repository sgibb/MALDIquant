context("calculateLabelPositions")

test_that(".textLabelRects", {
  pdf(NULL)
  on.exit(dev.off())
  plot.new()

  words <- c("foo", "foobar")
  cex <- 1

  r <- matrix(c(1:4, strwidth(words, cex=cex), strheight(words, cex=cex)),
              nrow = 2, dimnames = list(c(), c("x", "y", "w", "h")))
  r <- cbind(x0=r[, "x"] - r[, "w"]/2, y0=r[, "y"],
             x1=r[, "x"] + r[, "w"]/2, y1=r[, "y"] + r[, "h"],
             r)
  s <- MALDIquant:::.scaleFactor()
  r90 <- r
  r90[, "x1"] <- r90[, "x"]
  r90[, "x0"] <- r90[, "x"] - r[, "h"] / s
  r90[, "y0"] <- r90[, "y"] - r[, "w"]/2 * s
  r90[, "y1"] <- r90[, "y"] + r[, "w"]/2 * s
  r90[, c("w", "h")] <- r90[, c("x1", "y1")] - r90[, c("x0", "y0")]

  expect_equal(MALDIquant:::.textLabelRects(1:2, 3:4, c("foo", "foobar"),
                                            offset = c(0, 0), cex=1), r)
  expect_equal(MALDIquant:::.textLabelRects(1:2, 3:4, c("foo", "foobar"),
                                            offset = c(0, 0), cex=1, srt=90),
               r90)
})

test_that(".overlaps", {
  target <- c(2, 2, 4, 3)              # x1,y1 == bottom left
  dest <- list(a=target,               # identical
               b=c(1, 1, 3, 2.5),      # overlap
               c=c(3, 1, 6, 4),        # overlap
               d=c(2, 4, 3, 5),        # no overlap
               e=c(6, 0, 8, 1))        # no overlap
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

test_that(".scaleFactor", {
  pdf(NULL)
  on.exit(dev.off())
  plot.new()

  expect_equal(MALDIquant:::.scaleFactor(), 1.116279, tolerance = 1e-5)
})

test_that(".strWH", {
  pdf(NULL)
  on.exit(dev.off())
  plot.new()

  expect_equal(as.vector(MALDIquant:::.strWH("foo", srt=0, cex=1,
                                             scale=0.5)),
               c(strwidth("foo", cex=1), strheight("foo", cex=1)))
  expect_equal(as.vector(MALDIquant:::.strWH("foo", srt=90, cex=1,
                                             scale=0.5)),
               c(strwidth("foo", cex=1)/2, strheight("foo", cex=1)*2))
})

test_that(".rotate", {
  .m <- function(x)matrix(x, ncol=2, byrow=TRUE)
  p <- .m(c(1, 2,
            4, 4,
            3, 2))
  center <- p - 1
  srt <- c(0, 90, 180, 270, 360)
  r <- list("0" = p,
            "90" = .m(c(-1, 2,
                        2, 4,
                        1, 2)),
            "180" = .m(c(-1, 0,
                         2, 2,
                         1, 0)),
            "270" = .m(c(1, 0,
                         4, 2,
                         3, 0)),
            "360" = p)

  for (i in seq(along=srt)) {
    expect_equal(MALDIquant:::.rotate(p, center, srt=srt[i]),
                 r[[i]])
  }
})
