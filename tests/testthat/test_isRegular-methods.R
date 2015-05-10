context("isRegular")

test_that("isRegular", {
  expect_false(isRegular(createMassSpectrum(double(), double())))
  expect_false(isRegular(createMassSpectrum(double(1), double(1))))
  expect_false(isRegular(createMassSpectrum(double(2), double(2))))
  expect_true(isRegular(createMassSpectrum(double(3), double(3))))
  expect_true(isRegular(createMassSpectrum(1:10, 1:10)))
  expect_false(isRegular(createMassSpectrum((1:10)[-8], 1:9)))
  expect_true(isRegular(createMassSpectrum((1:10)[-8], 1:9), threshold=0.15))
})
