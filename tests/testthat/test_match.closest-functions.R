context("match.closest")

test_that("match.closest", {
  expect_error(match.closest(1, c(0, -1, 3)), "sorted non-decreasingly")
  expect_equal(match.closest(1.001, 1:10), 1)
  expect_equal(match.closest(1.4, 1:10), 1)
  expect_equal(match.closest(9.8, 1:10), 10)
  expect_equal(match.closest(11.1, 1:10), 10)
  expect_equal(match.closest(4:5, 4.8, tolerance=1), c(1, 1))
  expect_equal(match.closest(c(0.5, 1.5, exp(1), pi), 1:10),
               c(1, 2, 3, 3))
})

test_that("match.closest, length(table) == 1", {
  expect_equal(match.closest(1:3, 0, nomatch=0, tolerance=0), c(0, 0, 0))
  expect_equal(match.closest(1:3, 1, nomatch=0, tolerance=0), c(1, 0, 0))
  expect_equal(match.closest(1:3, 2, nomatch=0, tolerance=0), c(0, 1, 0))
  expect_equal(match.closest(1:3, 3, nomatch=0, tolerance=0), c(0, 0, 1))
  expect_equal(match.closest(1:3, 4, nomatch=0, tolerance=0), c(0, 0, 0))
})

test_that("match.closest, tolerance", {
  expect_error(match.closest(1, 1, tolerance=1, nomatch=1:2), "Length of .*nomatch.* has to be one")
  expect_warning(match.closest(1, 1, tolerance=-1), ".*tolerance.* < 0 is meaningless")
  expect_equal(match.closest(1.001, 1:10, tolerance=0), NA_integer_)
  expect_equal(suppressWarnings(match.closest(c(1,2,3.1), 1:3, tolerance=-1)), c(1, 2, NA_integer_))
  expect_equal(match.closest(1.001, 1:10, tolerance=0, nomatch=-1), -1)
  expect_equal(match.closest(1.4, 1:10, tolerance=0.4), 1)
})
