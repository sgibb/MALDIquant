context("match.closest")

test_that("match.closest", {
  expect_equal(match.closest(1.001, 1:10), 1)
  expect_equal(match.closest(1.4, 1:10), 1)
  expect_equal(match.closest(9.8, 1:10), 10)
  expect_equal(match.closest(11.1, 1:10), 10)
  expect_equal(match.closest(c(0.5, 1.5, exp(1), pi), 1:10),
               c(1, 1, 3, 3))
})

test_that("match.closest, tolerance", {
  expect_equal(match.closest(1.001, 1:10, tolerance=0), NA_integer_)
  expect_equal(match.closest(1.001, 1:10, tolerance=0, nomatch=-1), -1)
  expect_equal(match.closest(1.4, 1:10, tolerance=0.4), 1)
})
