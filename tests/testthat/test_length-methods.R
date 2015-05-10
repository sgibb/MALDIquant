context("length")

test_that("length", {
  expect_identical(length(createMassSpectrum(mass=1:10, intensity=1:10)), 10L)
  expect_identical(length(createMassSpectrum(mass=double(),
                                             intensity=double())), 0L)
})
