context("determineWarpingFunctions")

r <- createMassPeaks(mass=1:10, intensity=1:10)
p <- createMassPeaks(mass=(1:10)+0.01, intensity=1:10)

test_that("determineWarpingFunctions throws errors", {
  expect_error(determineWarpingFunctions(1:10),
               "no list of MALDIquant::MassPeaks objects")
  expect_error(determineWarpingFunctions(p, reference=createMassPeaks(double(),
                                                                      double())),
               "Reference MassPeaks object contains no peaks")
  expect_error(determineWarpingFunctions(createMassPeaks(mass=20, intensity=20),
                                         reference=r),
               "Could not match any peak in spectrum 1 to a reference peak")
})

test_that("determineWarpingFunctions throws warnings", {
  expect_warning(determineWarpingFunctions(p, reference=r[6:10]),
                 "Reference MassPeaks object contains very few peaks")
  expect_error(tryCatch(determineWarpingFunctions(list(p, p), plot=TRUE),
                        warning=function(w)stop(conditionMessage(w))),
               paste0(".*plot.* is .*TRUE.* but no non-interactive device ",
                      "is available. Using pdf.* to create a default one."))
  expect_error(tryCatch(determineWarpingFunctions(list(p, p), plot=TRUE,
                                                  plotInteractive=TRUE),
                        warning=function(w)stop(conditionMessage(w))),
               paste0(".*plot.* is .*TRUE.* but no interactive device is ",
                      "available. Using dev.new.* to create a default one."))
})

test_that("determineWarpingFunctions works with single MassPeaks object", {
  w <- determineWarpingFunctions(p, reference=r, method="linear")
  wp <- warpMassPeaks(list(p), w)[[1]]
  expect_equal(r, wp)
})

test_that("determineWarpingFunctions works with list of MassPeaks objects", {
  w <- determineWarpingFunctions(list(p, p), reference=r, method="linear")
  wp <- warpMassPeaks(list(p, p), w)
  expect_equal(list(r, r), wp)
})
