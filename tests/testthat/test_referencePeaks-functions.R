context("referencePeaks")

p <- c(createMassPeaks(mass=1:5, intensity=1:5),
       createMassPeaks(mass=1:4, intensity=1:4),
       createMassPeaks(mass=1:3, intensity=1:3),
       createMassPeaks(mass=1:2, intensity=1:2))

p2 <- c(createMassPeaks(mass=c(1, 1.001, 3), intensity=c(2, 1, 1)),
        createMassPeaks(mass=c(0.99, 3), intensity=rep(1, 2)),
        createMassPeaks(mass=c(1.02, 3), intensity=rep(1, 2)))

test_that("referencePeaks throws errors", {
  expect_error(referencePeaks(list(a="a", b="b"), minFrequency=0.5),
               "no list of MALDIquant::MassPeaks objects")
  expect_error(referencePeaks(p[[1]], minFrequency=0.5),
               "no list of MALDIquant::MassPeaks objects")
})

test_that("referencePeaks shows warnings", {
  expect_warning(referencePeaks(p, minFrequency=-1),
                 " < 0 does not make sense! Using 0 instead")
  expect_warning(referencePeaks(p, minFrequency=10),
                 "Empty peak whitelist for level")
})

test_that("referencePeaks works with different frequencies", {
  expect_equal(referencePeaks(p, minFrequency=1),
               createMassPeaks(mass=1:2, intensity=rep(1, 2)))

  expect_equal(referencePeaks(p, minFrequency=0.5),
               createMassPeaks(mass=1:4, intensity=c(1, 1, 3/4, 0.5)))
})

test_that("referencePeaks works with different methods", {
  expect_equal(referencePeaks(p2, minFrequency=1, tolerance=0.05),
               createMassPeaks(mass=3, intensity=1))
  expect_equal(referencePeaks(p2, method="strict", minFrequency=1,
                              tolerance=0.05),
               createMassPeaks(mass=3, intensity=1))
  expect_equal(referencePeaks(p2, method="relaxed", minFrequency=1,
                              tolerance=0.05),
               createMassPeaks(mass=c(1+1/300, 3), intensity=rep(1, 2)))
})
