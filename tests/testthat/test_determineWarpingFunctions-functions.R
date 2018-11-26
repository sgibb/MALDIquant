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
  expect_warning(determineWarpingFunctions(list(p, createMassPeaks(11, 11)),
                                           reference=r, allowNoMatches=TRUE),
                 "Could not match any peak in spectrum 2 to a reference peak")
})

test_that("determineWarpingFunctions works with single MassPeaks object", {
  w <- determineWarpingFunctions(p, reference=r, method="linear")
  expect_equal(attr(w, "nmatch"), 6)
  wp <- warpMassPeaks(list(p), w)[[1]]
  expect_equal(r, wp)
})

test_that("determineWarpingFunctions works with list of MassPeaks objects", {
  w <- determineWarpingFunctions(list(a=p, b=p), reference=r, method="linear")
  expect_equal(attr(w, "nmatch"), c(a=6, b=6))
  wp <- warpMassPeaks(list(p, p), w)
  expect_equal(list(r, r), wp)
})

test_that("determineWarpingFunctions supports allowNoMatches argument", {
  suppressWarnings(
    w <- determineWarpingFunctions(
        list(p, createMassPeaks(11, 11), p), reference=r, allowNoMatches=TRUE)
  )
  expect_equal(sapply(w, is.function), c(TRUE, FALSE, TRUE))
  expect_equal(is.na(w), c(FALSE, TRUE, FALSE))
  expect_equal(attr(w, "nmatch"), c(6, 0, 6))
  wp <- warpMassPeaks(list(p, createMassPeaks(11, 11), p), w, emptyNoMatches=TRUE)
  expect_equal(list(r, createMassPeaks(11, 0), r), wp)
})
