context("filterPeaks")

p <- createMassPeaks(mass=1:5, intensity=1:5)
l <- list(p, p[1:4], p[1:3], p[1:2])

test_that("filterPeaks throws errors", {
  expect_error(filterPeaks(p), "no list of MALDIquant::MassPeaks")
  expect_error(filterPeaks(list()), "no list of MALDIquant::MassPeaks")
  expect_error(filterPeaks(l, minFrequency=NA, minNumber=NA),
                 " has to be a meaningful number")
})

test_that("filterPeaks shows warnings", {
  expect_warning(filterPeaks(l, minFrequency=2),
                 " > 1 does not make sense! Using 1 instead")
  expect_warning(filterPeaks(l, minFrequency=-1),
                 " < 0 does not make sense! Using 0 instead")
  expect_warning(filterPeaks(l, minNumber=10),
                 "does not make sense! Using 4 instead")
  expect_warning(filterPeaks(l, minNumber=-1),
                 " < 0 does not make sense! Using 0 instead")
  expect_warning(filterPeaks(l, minFrequency=2/3, minNumber=2),
                 " arguments are given. Choosing the higher one.")
  expect_warning(filterPeaks(l, minNumber=2, labels=c(1, 2, 2, 2)),
                 "does not make sense! Using 1 instead")
})

test_that("filterPeaks", {
  expect_identical(filterPeaks(l, minFrequency=1),
                   list(p[1:2], p[1:2], p[1:2], p[1:2]))

  expect_identical(filterPeaks(l, minFrequency=0.5),
                   list(p[1:4], p[1:4], p[1:3], p[1:2]))

  expect_identical(filterPeaks(l, minFrequency=0), l)

  expect_identical(filterPeaks(l, minFrequency=1,
                               labels=factor(rep(letters[1:2], each=2),
                                             levels=letters[1:2])),
                   list(p[1:4], p[1:4], p[1:2], p[1:2]))

  ## test unused levels
  expect_identical(filterPeaks(l, minFrequency=1,
                               labels=factor(rep(letters[1:2], each=2),
                                             levels=letters[1:5])),
                   list(p[1:4], p[1:4], p[1:2], p[1:2]))

  ## test numbers
  expect_identical(filterPeaks(l, minFrequency=1,
                               labels=rep(1:2, each=2)),
                   list(p[1:4], p[1:4], p[1:2], p[1:2]))
  ## test case for #22 (unexpected results for different number of technical
  ## replicates because using of floor(minFrequency))
  expect_identical(filterPeaks(list(p, p[1:4], p[1:3], p, p[1:4], p[1:3],
                                    p[1:2]), minFrequency=2/3,
                               labels=c(rep(1, 3), rep(2, 4))),
                   list(p[1:4], p[1:4], p[1:3], p[1:3], p[1:3], p[1:3], p[1:2]))

  ## use absolute threshold
  expect_identical(filterPeaks(list(p, p[1:4], p[1:3], p, p[1:4], p[1:3],
                                    p[1:2]), minNumber=2,
                               labels=c(rep(1, 3), rep(2, 4))),
                   list(p[1:4], p[1:4], p[1:3], p[1:4], p[1:4], p[1:3], p[1:2]))

  ## test case for #26 (minNumber > n removes all peaks)
  expect_identical(suppressWarnings(filterPeaks(list(p, p[1:4], p),
                                                minNumber=2,
                                                labels=c(1, 2, 2))),
                   list(p, p[1:4], p[1:4]))
})

test_that("filterPeaks mode argument works", {
  p2 <- list(
    createMassPeaks(1:5, 1:5),
    createMassPeaks(1:4, 1:4),
    createMassPeaks(4:9, 4:9),
    createMassPeaks(4:8, 4:8),
    createMassPeaks(1:5, 1:5),
    createMassPeaks(2:6, 2:6))

  ## test group mode (with recycling)
  expect_identical(filterPeaks(p2, minFrequency=1, labels=rep(1:3, each=2),
                               mode="group"),
                   c(p2[[2]], p2[[2]], p2[[4]], p2[[4]],
                     createMassPeaks(2:5, 2:5), createMassPeaks(2:5, 2:5)))
  ## test all mode (with recycling)
  expect_identical(filterPeaks(p2, minFrequency=1, labels=rep(1:3, each=2),
                               mode="all"),
                   c(p2[1:2], p2[[4]], p2[[4]], p2[5:6]))
  ## test none mode (with recycling)
  expect_equal(filterPeaks(p2, minFrequency=1, labels=rep(1:3, each=2),
                               mode="none"),
               rep(c(createMassPeaks(double(), double())), 6))
  ## test complex mode
  expect_identical(filterPeaks(p2, minFrequency=c(1, 1, 0),
                               labels=rep(1:3, each=2),
                               mode=c("group", "all", "none")),
                   c(p2[1:2], p2[[4]], p2[[4]],
                     createMassPeaks(4:5, 4:5), createMassPeaks(4:6, 4:6)))
  ## test complex mode with different numbers and frequencies
  expect_identical(filterPeaks(p2, minFrequency=c(1, NA, 0),
                               minNumber=c(NA, 2, NA),
                               labels=rep(1:3, each=2),
                               mode=c("group", "all", "none")),
                   c(p2[1:2], p2[[4]], p2[[4]],
                     createMassPeaks(4:5, 4:5), createMassPeaks(4:6, 4:6)))
})
