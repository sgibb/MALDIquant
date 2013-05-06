context(".equal")

test_that(".equal compares numeric vectors", {
  expect_true(MALDIquant:::.equal(1:3, 1:3))
  expect_true(MALDIquant:::.equal(diff((1:10)+0.2), rep(1, 9)))
  expect_true(MALDIquant:::.equal(double(), double()))
  ## unequal length
  expect_false(MALDIquant:::.equal(1:3, 1:4))
  ## different order
  expect_false(MALDIquant:::.equal(3:1, 1:3))
})

test_that(".equal returns NA if at least one vector contains a NA element", {
  expect_identical(MALDIquant:::.equal(c(1, NA, 3), 1:3), NA)
  expect_identical(MALDIquant:::.equal(c(1, NA, 3), c(1, NA, 3)), NA)
})

test_that(".equal returns NA for non-numeric vectors", {
  expect_identical(MALDIquant:::.equal(letters[1:3], letters[1:3]), NA)
  expect_identical(MALDIquant:::.equal(list(1:3), list(1:3)), NA)
})

