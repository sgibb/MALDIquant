context("mass")

s <- createMassSpectrum(mass=1:10, intensity=11:20)

test_that("mass", {
  expect_identical(mass(s), 1:10)
  expect_identical(mass(s)[1:3], 1:3)
})

test_that("mass<- throws errors", {
  expect_error(mass(s) <- LETTERS[1:10])
  expect_error(mass(s) <- 1, "have to be equal")
})

test_that("mass<-", {
  mass(s) <- 11:20
  expect_equal(mass(s), 11:20)
  mass(s)[5:10] <- 5:10
  expect_equal(mass(s), c(11:14, 5:10))
})
