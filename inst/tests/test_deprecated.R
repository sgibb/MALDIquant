context("deprecated")

test_that("totalIonCurrent", {
  p <- createMassPeaks(mass=1:5, intensity=1:5)
  expect_equal(suppressWarnings(totalIonCurrent(p)), 15)
  expect_warning(totalIonCurrent(p), "is deprecated.")
})

