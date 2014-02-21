context("intensity")

s <- createMassSpectrum(mass=1:10, intensity=11:20)

test_that("intensity", {
  expect_identical(intensity(s), 11:20)
  expect_identical(intensity(s)[1:3], 11:13)
})

test_that("intensity<- throws errors", {
  expect_error(intensity(s) <- LETTERS[1:10])
  expect_error(intensity(s) <- 1, "have to be equal")
})

test_that("intensity<-", {
  intensity(s) <- 1:10
  expect_equal(intensity(s), 1:10)
  intensity(s)[5:10] <- 15:20
  expect_equal(intensity(s), c(1:4, 15:20))
})
