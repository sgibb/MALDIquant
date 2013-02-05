context("merge")

s <- c(createMassSpectrum(mass=1:3, intensity=1:3, metaData=list(a=1, b=2)),
       createMassSpectrum(mass=1:3, intensity=4:6, metaData=list(a=1, b=3)))

meanS <- createMassSpectrum(mass=1:3, intensity=(2:4)+0.5,
                            metaData=list(a=1, b=2:3))
sumS <- createMassSpectrum(mass=1:3, intensity=c(5, 7, 9),
                           metaData=list(a=1, b=2:3))

p <- c(createMassPeaks(mass=1:3, intensity=1:3, snr=1:3,
                       metaData=list(a=1, b=2)),
       createMassPeaks(mass=1:3, intensity=4:6, snr=4:6,
                       metaData=list(a=1, b=3)))

meanP <- createMassPeaks(mass=1:3, intensity=(2:4)+0.5, snr=(2:4)+0.5,
                         metaData=list(a=1, b=2:3))
sumP <- createMassPeaks(mass=1:3, intensity=c(5, 7, 9), snr=c(5, 7, 9),
                        metaData=list(a=1, b=2:3))

test_that("mergeMassPeaks", {
  expect_equal(unname(mergeMassPeaks(list(p[[1]], p[[1]], p[[2]], p[[2]]),
                                     labels=c(1:2, 1:2))),
               list(meanP, meanP))
  expect_equal(unname(mergeMassPeaks(list(p[[1]], p[[1]], p[[2]], p[[2]]),
                                     labels=c(1:2, 1:2), fun=sum)),
               list(sumP, sumP))
})

test_that("mergeMassPeaks throws errors", {
  expect_error(mergeMassPeaks(1:3),
               "no list of MALDIquant::MassPeaks objects")
  expect_error(mergeMassPeaks(list()),
               "no list of MALDIquant::MassPeaks objects")
  expect_error(mergeMassPeaks(s),
               "no list of MALDIquant::MassPeaks objects")
})

test_that("mergeMassSpectra", {
  expect_equal(unname(mergeMassSpectra(list(s[[1]], s[[1]], s[[2]], s[[2]]),
                                       labels=c(1:2, 1:2))),
               list(meanS, meanS))
  expect_equal(unname(mergeMassSpectra(list(s[[1]], s[[1]], s[[2]], s[[2]]),
                                       labels=c(1:2, 1:2), fun=sum)),
               list(sumS, sumS))
})

test_that("mergeMassSpectra throws errors", {
  expect_error(mergeMassSpectra(1:3),
               "no list of MALDIquant::MassSpectrum objects")
  expect_error(mergeMassSpectra(list()),
               "no list of MALDIquant::MassSpectrum objects")
  expect_error(mergeMassSpectra(p),
               "no list of MALDIquant::MassSpectrum objects")
})


test_that(".mergeMassPeaks", {
  expect_equal(MALDIquant:::.mergeMassPeaks(p), meanP)
  expect_equal(MALDIquant:::.mergeMassPeaks(p, fun=sum), sumP)
})

test_that(".mergeMassSpectra", {
  expect_equal(MALDIquant:::.mergeMassSpectra(s), meanS)
  expect_equal(MALDIquant:::.mergeMassSpectra(s, fun=sum), sumS)
})

test_that(".mergeMetaData", {
  l <- list(a=list(numbers=1:3, lists=list(a=1, b=2), characters=c("a", "b")),
            b=list(numbers=1:3, lists=list(a=1, b=2), characters=c("b", "c")))
  r <- list(numbers=1:3, lists=list(a=1, b=2),
            characters=c("a", "b", "b", "c"))
  expect_identical(MALDIquant:::.mergeMetaData(l), r)
})

test_that(".merge", {
  m <- matrix(c(1:6, NA, NA), nrow=2, byrow=TRUE)
  expect_equal(MALDIquant:::.merge(m, fun=mean), c(3:4, 3:4))
  expect_equal(MALDIquant:::.merge(m, fun=mean, na.rm=FALSE), c(3:4, NA, NA))
  expect_equal(MALDIquant:::.merge(m, fun=sum), c(6, 8, 3:4))
  expect_equal(MALDIquant:::.merge(m, fun=function(x)sum(x)+1), c(7, 9, 4:5))
  expect_equal(MALDIquant:::.merge(m, fun=function(x)sum(x)+1, na.rm=FALSE),
               c(7, 9, NA, NA))
})

