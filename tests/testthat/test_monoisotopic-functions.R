context(".monoisotopic")

test_that(".pseudoCluster", {
  x <- c(1, 2, 3, 5, 8, 9, 10, 12, 15)
  m1s2 <- matrix(c(1, 2, 2, 3, 5, 6, 6, 7), nrow=2)
  m1s3 <- matrix(c(1, 2, 3, 5, 6, 7), nrow=3)
  m3s3 <- matrix(c(2, 4, 5, 6, 8, 9), nrow=3)
  m5s4 <- matrix(NA_real_, nrow=4, ncol=0)
  m12s3 <- matrix(c(1:3, 5:7, 1, 3, 4, 5, 7, 8), nrow=3)

  expect_error(MALDIquant:::.pseudoCluster(x, size=1),
               "The .*size.* of a cluster has to be at least 2")
  expect_equal(MALDIquant:::.pseudoCluster(x, size=2, distance=1), m1s2)
  expect_equal(MALDIquant:::.pseudoCluster(x, size=3, distance=1), m1s3)
  expect_equal(MALDIquant:::.pseudoCluster(x, size=3, distance=3), m3s3)
  expect_equal(MALDIquant:::.pseudoCluster(x, size=4, distance=5), m5s4)
  expect_equal(MALDIquant:::.pseudoCluster(x, size=3, distance=1:2), m12s3)
})

test_that(".F", {
  x <- seq(1000, 5000, by=10)
  expect_equal(MALDIquant:::.F(x), 0.000594 * x + 0.03091)
})

test_that(".P", {
  x <- seq(1000, 5000, by=10)
  expect_equal(MALDIquant:::.P(x, 0:3), dpois(0:3, MALDIquant:::.F(x)))
})

test_that(".Psum", {
  x <- c(1000, 2000)
  isotopes <- 0:3
  p <- sapply(x, function(xx) {
              pp <- MALDIquant:::.P(xx, isotopes=isotopes)
              pp/sum(pp)
  })
  expect_equal(MALDIquant:::.Psum(x, isotopes), p)
})

test_that(".monoisotopicPattern", {
  x <- c(1, 2, 3, 5, 8, 9, 10, 12, 15)
  y <- c(96, 3, 1, 5, 78, 20, 2, 12, 15)

  expect_equal(MALDIquant:::.monoisotopicPattern(1:10, 1:10),
               matrix(NA_real_, nrow=3, ncol=0))
  expect_equal(MALDIquant:::.monoisotopicPattern(x, y, distance=1, size=3),
               cbind(1:3, 5:7))
  expect_equal(MALDIquant:::.monoisotopicPattern(x, y, distance=1:2, size=3),
               cbind(1:3, 5:7))
  expect_equal(MALDIquant:::.monoisotopicPattern(x, y, distance=2:1, size=3),
               cbind(c(1, 3, 4), c(5, 7, 8)))
  expect_equal(MALDIquant:::.monoisotopicPattern(x, y, distance=1, size=2),
               cbind(1:2, 5:6))
  expect_equal(MALDIquant:::.monoisotopicPattern(x, y, distance=1, minCor=0.99), as.matrix(1:3))
})

test_that(".monoisotopic", {
  x <- c(1, 2, 3, 5, 8, 9, 10, 12, 15)
  y <- c(96, 3, 1, 5, 78, 20, 2, 12, 15)

  expect_equal(MALDIquant:::.monoisotopic(double(), double()), numeric())
  expect_equal(MALDIquant:::.monoisotopic(1:10, 1:5), numeric())
  expect_equal(MALDIquant:::.monoisotopic(1:10, 1:10), numeric())
  expect_equal(MALDIquant:::.monoisotopic(x, y, distance=1, size=2:5), c(1, 5))
  expect_equal(MALDIquant:::.monoisotopic(x, y, distance=1, minCor=0.99), 1)
})
