context("metaData")

s <- createMassSpectrum(mass=1:10, intensity=11:20, metaData=list(name="test spectrum"))

test_that("metaData", {
  expect_identical(metaData(s)$name, "test spectrum")
})

test_that("metaData<-", {
  l <- list(a=1:3, b=letters[1:3])
  metaData(s) <- l
  expect_identical(metaData(s), l)
})
