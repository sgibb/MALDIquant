context(".colMedians")

test_that(".colMedians throws errors", {
  expect_error(MALDIquant:::.colMedians(list()))
  expect_error(MALDIquant:::.colMedians(matrix(1:10), na.rm="foobar"))
})

test_that(".colMedians", {
  colMedians <- function(x, na.rm=FALSE)apply(x, 2, median, na.rm=na.rm)
  set.seed(1234)
  ## even nrow
  m <- matrix(rnorm(1e5), ncol=1e2)
  expect_equal(MALDIquant:::.colMedians(m), colMedians(m))
  ## odd nrow
  nr <- nrow(m)
  m <- m[1:(nr-1), ]
  expect_equal(MALDIquant:::.colMedians(m), colMedians(m))
  ## NA
  na <- sample(1:length(m), size=1e2)
  m[na] <- NA
  expect_equal(MALDIquant:::.colMedians(m), colMedians(m))
  expect_equal(MALDIquant:::.colMedians(m, TRUE), colMedians(m, TRUE))
})

test_that(".colMaxs", {
  colMaxs <- function(x)apply(x, 2, max)
  set.seed(1234)
  ## even nrow
  m <- matrix(rnorm(1e5), ncol=1e2)
  expect_equal(MALDIquant:::.colMaxs(m), colMaxs(m))
})

test_that(".colCors", {
  colCors <- function(x, y, use="everything") {
    z <- double(ncol(x))
    for (i in 1:ncol(x)) {
      z[i] <- stats::cor(x[,i], y[,i], use=use)
    }
    z
  }
  set.seed(1234)
  m <- matrix(rnorm(1e5), ncol=1e2)
  n <- matrix(rnorm(1e5), ncol=1e2)
  mna <- m
  nna <- n
  mna[sample(1e5, 1e3)] <- NA
  nna[sample(1e5, 1e3)] <- NA
  expect_equal(MALDIquant:::.colCors(m, n), colCors(m, n))
  expect_equal(MALDIquant:::.colCors(mna, nna), colCors(mna, nna))
  expect_equal(MALDIquant:::.colCors(mna, nna, na.rm=TRUE),
               colCors(mna, nna, use="na.or.complete"))
})

