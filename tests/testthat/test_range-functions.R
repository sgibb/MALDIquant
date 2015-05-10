context("range")

test_that(".overlap", {
  l1 <- c(createMassSpectrum(1L:10L, 1L:10L),
          createMassSpectrum(5L:15L, 5L:15L))

  l2 <- c(createMassSpectrum(1L:10L, 1L:10L),
          createMassSpectrum(15L:25L, 15L:25L))

  expect_identical(MALDIquant:::.overlap(l1), c(5L, 10L))
  expect_identical(MALDIquant:::.overlap(l2), c(0L, 0L))
})

test_that(".reorderRange", {
  expect_identical(MALDIquant:::.reorderRange(c(5L, 10L)), c(5L, 10L))
  expect_identical(MALDIquant:::.reorderRange(c(10L, 5L)), c(5L, 10L))
})
