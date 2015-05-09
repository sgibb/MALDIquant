context("coordinates")

s <- list(createMassSpectrum(mass=1:5, intensity=6:10,
                             metaData=list(imaging=list(pos=c(5, 2)))),
          createMassSpectrum(mass=1:5, intensity=6:10,
                             metaData=list(imaging=list(pos=c(6, 2)))))

test_that("coordinates", {
  expect_identical(coordinates(s[[1]]), c(5, 2))
  expect_identical(coordinates(s), cbind(x=5:6, y=c(2, 2)))
  expect_identical(coordinates(s, adjust=TRUE), cbind(x=1:2, y=c(1, 1)))
})

test_that("coordinates<- throws errors", {
  expect_error(coordinates(s[[1]]) <- LETTERS[1:10])
  expect_error(coordinates(s[[1]]) <- 1, "2 or 3 coordinates are needed!")
  expect_error(coordinates(s[[1]]) <- 4, "2 or 3 coordinates are needed!")
  expect_error(coordinates(s[[1]]) <- cbind(x=1, y=1, z=1, zz=1),
               "2 or 3 coordinates are needed!")
  expect_error(coordinates(s) <- cbind(x=1:4), "2 or 3 coordinates are needed!")
  l <- list(1:10)
  expect_error(coordinates(l) <- cbind(x=2:3, y=3:4),
               "is no list of MALDIquant::AbstractMassObject objects!")
})

test_that("coordinates<- throws warnings", {
  expect_warning(coordinates(s[[1]]) <- cbind(x=1:2, y=1:2),
                 "all rows but the first are ignored")
})

test_that("coordinates<-", {
  coordinates(s[[1]]) <- 2:3
  expect_equal(coordinates(s[[1]]), 2:3)
  coordinates(s[[1]]) <- cbind(x=2, y=3)
  expect_equal(coordinates(s[[1]]), c(x=2, y=3))
  coordinates(s[[1]])[2] <- 5
  expect_equal(coordinates(s[[1]]), c(x=2, y=5))
  coordinates(s) <- cbind(x=2:3, y=3:4)
  expect_equal(coordinates(s), cbind(x=2:3, y=3:4))
})
