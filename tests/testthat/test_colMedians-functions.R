context(".colMedians")

test_that(".colMedians throws errors", {
  expect_error(MALDIquant:::.colMedians(list()))
  expect_error(MALDIquant:::.colMedians(matrix(1:10), na.rm="foobar"))
})

test_that(".colMedians", {
  colMedians <- function(x, na.rm=FALSE)apply(m, 2, median, na.rm=na.rm)
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
