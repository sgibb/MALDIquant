context(".replaceNegativeIntensityValues")

test_that(".replaceNegativeIntensityValues", {
  s <- suppressWarnings(createMassSpectrum(mass=1:5, intensity=-(1:5)))
  expect_warning(MALDIquant:::.replaceNegativeIntensityValues(s),
                 "Negative intensity values are replaced by zeros.")
  expect_equal(intensity(
                 MALDIquant:::.replaceNegativeIntensityValues(s, warn=FALSE)),
               rep(0L, 5L))
})
