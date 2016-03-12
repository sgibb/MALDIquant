context("plotMsiSlice")

m <- c(createMassSpectrum(mass=1:10, intensity=1:10),
       createMassSpectrum(mass=2:11, intensity=2:11))
coordinates(m) <- cbind(1:2, c(1, 1))

test_that("plotMsiSlice throws errors", {
  expect_error(plotMsiSlice(m, center=c(3, 5), tolerance=0.5,
                            colRamp=list(A=1, B=2, C=3)),
               paste0(".*number of centers.* has to be the same ",
                      "as the length of the list .*colRamp.*"))
})

test_that("plotMsiSlice throws warnings", {
  expect_error(tryCatch(plotMsiSlice(m, center=c(3, 5), tolerance=0.5),
                        warning=function(w)stop(conditionMessage(w))),
               paste0(".*plotMsiSlice.* was called for multiple slices on an ",
                      "interactive device. Only the first slice is plotted."))
})
