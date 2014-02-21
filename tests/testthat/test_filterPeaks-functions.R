context("filterPeaks")

p <- createMassPeaks(mass=1:5, intensity=1:5)
l <- list(p, p[1:4], p[1:3], p[1:2])

test_that("filterPeaks throws errors", {
  expect_error(filterPeaks(p), "no list of MALDIquant::MassPeaks")
  expect_error(filterPeaks(list()), "no list of MALDIquant::MassPeaks")
  expect_error(filterPeaks(l, minFrequency=NA, minNumber=NA),
                 " has to be a meaningful number")
  expect_error(filterPeaks(l=l, labels=as.factor("a"), minFrequency=1),
               "For each item in .*l.* there must be a label in .*labels.*")
})

test_that("filterPeaks shows warnings", {
  expect_warning(filterPeaks(l, minFrequency=2),
                 "Empty peak whitelist for level")
  expect_warning(filterPeaks(l, minFrequency=-1),
                 " < 0 does not make sense! Using 0 instead")
  expect_warning(filterPeaks(l, minNumber=10),
                 "Empty peak whitelist for level")
  expect_warning(filterPeaks(l, minNumber=-1),
                 " < 0 does not make sense! Using 0 instead")
  expect_warning(filterPeaks(l, minFrequency=2/3, minNumber=2),
                 " arguments are given. Choosing the higher one.")
  expect_warning(filterPeaks(l, minNumber=2, labels=c(1, 2, 2, 2)),
                 "Empty peak whitelist for level")
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
  ## since MALDIquant 1.8.16 min{Frequency,minNumber} could be vectors so this
  ## is not needed any more and we could use min{Frequency,Number} as feature to
  ## remove all peaks (or avoid the generation of a whitelist if
  ## mergeWhitelists=TRUE)
  expect_equal(suppressWarnings(filterPeaks(list(p, p[1:4], p),
                                                minNumber=2,
                                                labels=c(1, 2, 2))),
               list(createMassPeaks(double(), double()), p[1:4], p[1:4]))
})

test_that("filterPeaks mergeWhitelists argument works", {
  p2 <- list(
    createMassPeaks(1:5, 1:5),
    createMassPeaks(1:4, 1:4),
    createMassPeaks(4:9, 4:9),
    createMassPeaks(4:8, 4:8),
    createMassPeaks(1:5, 1:5),
    createMassPeaks(2:6, 2:6))

  expect_identical(filterPeaks(p2, minFrequency=1, labels=rep(1:3, each=2),
                               mergeWhitelists=FALSE),
                   c(p2[[2]], p2[[2]], p2[[4]], p2[[4]],
                     createMassPeaks(2:5, 2:5), createMassPeaks(2:5, 2:5)))
  expect_identical(filterPeaks(p2, minFrequency=1, labels=rep(1:3, each=2),
                               mergeWhitelists=TRUE),
                   c(p2[1:2], p2[[4]], p2[[4]], p2[5:6]))
  expect_identical(suppressWarnings(filterPeaks(p2, minFrequency=c(1, 1, 2),
                                                labels=rep(1:3, each=2),
                                                mergeWhitelists=TRUE)),
                   c(p2[1:2], p2[[4]], p2[[4]], p2[5:6]))
  ## test with different numbers and frequencies
  expect_identical(suppressWarnings(filterPeaks(p2, minFrequency=c(1, NA, NA),
                                                minNumber=c(NA, 3, 1),
                                                labels=rep(1:3, each=2),
                                                mergeWhitelists=TRUE)),
                   c(p2[1:2], createMassPeaks(4:6, 4:6),
                     createMassPeaks(4:6, 4:6), p2[5:6]))
})
