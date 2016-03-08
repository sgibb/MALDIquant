context("mz")

s <- createMassSpectrum(mass=1:10, intensity=11:20)

test_that("mz", {
  expect_identical(mz(s), 1:10)
  expect_identical(mz(s)[1:3], 1:3)
})

test_that("mz<- throws errors", {
  expect_error(mz(s) <- LETTERS[1:10])
  expect_error(mz(s) <- 1, "have to be equal")
})

test_that("mz<-", {
  mz(s) <- 11:20
  expect_equal(mz(s), 11:20)
  mz(s)[5:10] <- 5:10
  expect_equal(mz(s), c(11:14, 5:10))
})
