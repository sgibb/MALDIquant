context("deprecated")

## deprecated since MALDIquant 1.7.4

test_that("totalIonCurrent", {
  p <- createMassPeaks(mass=1:5, intensity=1:5)
  expect_equal(suppressWarnings(totalIonCurrent(p)), 15)
  expect_warning(totalIonCurrent(p), "is deprecated.")
})

## deprecated since MALDIquant 1.7.6

test_that("calibrate", {
  m <- matrix(c(1, 2, 3,
              3, 6, 9), nrow=2, byrow=TRUE,
              dimnames=list(paste0("samples", 1:2),
                            paste("peaks", LETTERS[1:3])))

  expect_warning(calibrate(m), "is deprecated.")
  expect_error(suppressWarnings(calibrate(list())))
  expect_error(suppressWarnings(calibrate(createMassPeaks(mass=1:10,
                                                          intensity=1:10))))
  expect_true(all(as.numeric(suppressWarnings(calibrate(m))) ==
                  rep(c(2, 4, 6), each=2)))
  expect_true(all(attr(suppressWarnings(calibrate(m)), "scale") == c(0.5, 1.5)))
})

test_that("standardizeTotalIonCurrent", {
  s <- list(createMassSpectrum(mass=1:10, intensity=1:10),
            createMassSpectrum(mass=1:10, intensity=2:11))

  expect_warning(standardizeTotalIonCurrent(s), "is deprecated.")

  expect_error(suppressWarnings(standardizeTotalIonCurrent(list(a=1, b=2))))

  sTIC <- suppressWarnings(standardizeTotalIonCurrent(s))
  expect_equal(totalIonCurrent(sTIC[[1]]), 1)
  expect_equal(totalIonCurrent(sTIC[[2]]), 1)
  sTIC <- suppressWarnings(standardizeTotalIonCurrent(s, 2))
  expect_equal(totalIonCurrent(sTIC[[1]]), 2)
  expect_equal(totalIonCurrent(sTIC[[2]]), 2)
})

test_that("totalIonCurrent<-", {
  s <- createMassSpectrum(mass=1:10, intensity=1:10)
  i <- createMassSpectrum(mass=1:2, intensity=rep.int(.Machine$integer.max, 2))
  e <- createMassSpectrum(mass=1:10, intensity=rep(0, 10))

  expect_warning(totalIonCurrent(s) <- 1, "is deprecated.")
  expect_warning(totalIonCurrent(e) <- 1,
                  "Total Ion Current is zero! Is spectrum empty?")

  expect_error(suppressWarnings(totalIonCurrent(s) <- 1:10,
               "Length of .*value.* has to be one"))
  expect_error(suppressWarnings(totalIonCurrent(s) <- "A",
               "unable to find an inherited method .*character.*"))
  suppressWarnings(totalIonCurrent(s) <- 1)
  expect_equal(totalIonCurrent(s), 1)
  suppressWarnings(totalIonCurrent(e) <- 1)
  expect_equal(sum(e@intensity), 0)
  expect_equal(length(e), 10)
})

test_that("{l,r}trim", {
  s <- createMassSpectrum(mass=1:10, intensity=11:20)
  expect_warning(ltrim(s, 2), "is deprecated.")
  expect_equal(suppressWarnings(ltrim(s, 2)),
               createMassSpectrum(mass=2:10, intensity=12:20))
  expect_warning(rtrim(s, 9), "is deprecated.")
  expect_equal(suppressWarnings(rtrim(s, 9)),
               createMassSpectrum(mass=1:9, intensity=11:19))
})

