context("grouper")

test_that("grouperStrict", {
  expect_true(is.na(MALDIquant:::.grouperStrict(mass=c(1, 2),
                                                intensities=c(1, 2),
                                                samples=c(1, 1),
                                                tolerance=0.01)))
  expect_true(is.na(MALDIquant:::.grouperStrict(mass=c(1, 2, 2.1, 2.01),
                                                intensities=rep(1, 4),
                                                samples=1:4,
                                                tolerance=0.01)))
  expect_true(MALDIquant:::.grouperStrict(mass=c(2, 2.02, 2.01),
                                          intensities=rep(1, 3),
                                          samples=1:4,
                                          tolerance=0.01) == 2.01)
})

test_that("grouperRelaxed", {
  expect_true(is.na(MALDIquant:::.grouperRelaxed(mass=c(1, 2, 2.1, 2.01),
                                                 intensities=rep(1, 4),
                                                 samples=1:4,
                                                 tolerance=0.01)))
  expect_equal(MALDIquant:::.grouperRelaxed(mass=c(2.01, 2.03, 2.04, 2.01),
                                            intensities=c(2, 1, 1, 1),
                                            samples=c(1, 1, 2, 3),
                                            tolerance=0.1),
               c(2.02, 2.03, 2.02, 2.02))
  expect_equal(MALDIquant:::.grouperRelaxed(mass=c(1.009, 1.01, 1, 1.03),
                                            intensities=c(2, 1, 1, 1),
                                            samples=c(1, 1, 2, 3),
                                            tolerance=0.1),
               c(1.013, 1.01, 1.013, 1.013))
})

test_that("grouperRelaxedHighestAtReference", {
  expect_identical(MALDIquant:::.grouperRelaxedHighestAtReference(
                      mass=1:5, intensities=rep(1, 5), samples=2:6,
                      tolerance=0.01), 0L)
  expect_true(is.na(MALDIquant:::.grouperRelaxedHighestAtReference(
                      mass=1:5, intensities=rep(1, 5), samples=c(1, 1:4),
                      tolerance=0.01)))
  expect_equal(MALDIquant:::.grouperRelaxedHighestAtReference(
                  mass=c(2.01, 2.03, 2.04, 2.01), intensities=c(1, 2, 1, 1),
                  samples=c(1, 2, 2, 3), tolerance=0.1),
               c(2.01, 2.01, 0, 2.01))
})
