context(".reorder")

test_that(".reorder", {
  s <- createMassSpectrum(mass=1:5, intensity=1:5)
  ## do nothing
  expect_equal(intensity(MALDIquant:::.reorder(s)), 1:5)

  ## reorder
  s@mass <- 5:1
  expect_warning(MALDIquant:::.reorder(s),
                 "Mass and intensity values are reordered.")
  expect_equal(intensity(MALDIquant:::.reorder(s, warn=FALSE)), 5:1)
})
