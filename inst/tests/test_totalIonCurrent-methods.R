context("totalIonCurrent")

s <- createMassSpectrum(mass=1:10, intensity=1:10)
e <- createMassSpectrum(mass=1:10, intensity=rep(0, 10))

test_that("totalIonCurrent", {
  expect_equal(totalIonCurrent(s), 55)
})

test_that("totalIonCurrent<- throws errors", {
  expect_error(totalIonCurrent(s) <- 1:10,
               "Length of .*value.* has to be one")
  expect_error(totalIonCurrent(s) <- "A",
               "unable to find an inherited method .*character.*")
})

test_that("totalIonCurrent<- throws warnings", {
  expect_warning(totalIonCurrent(e) <- 1,
                 "Total Ion Current is zero! Is spectrum empty?")
})

test_that("totalIonCurrent<-", {
  totalIonCurrent(s) <- 1
  expect_equal(sum(s@intensity), 1)
  suppressWarnings(totalIonCurrent(e) <- 1)
  expect_equal(sum(e@intensity), 0)
  expect_equal(length(e), 10)
})
