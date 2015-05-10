context("merge")

s <- c(createMassSpectrum(mass=1:3, intensity=1:3, metaData=list(a=1, b=2)),
       createMassSpectrum(mass=1:3, intensity=4:6, metaData=list(a=1, b=3)))

p <- c(createMassPeaks(mass=1:3, intensity=1:3, snr=1:3,
                       metaData=list(a=1, b=2)),
       createMassPeaks(mass=1:3, intensity=4:6, snr=4:6,
                       metaData=list(a=1, b=3)))

pNA <- c(createMassPeaks(mass=1:3, intensity=1:3, snr=1:3,
                         metaData=list(a=1, b=2)),
         createMassPeaks(mass=1:2, intensity=4:5, snr=4:5,
                         metaData=list(a=1, b=3)))

meanP <- createMassPeaks(mass=1:3, intensity=(2:4)+0.5, snr=(2:4)+0.5,
                         metaData=list(a=1, b=2:3))

meanPNA <- createMassPeaks(mass=1:3, intensity=c(2.5, 3.5, 1.5),
                           snr=c(2.5, 3.5, 1.5), metaData=list(a=1, b=2:3))
sumP <- createMassPeaks(mass=1:3, intensity=c(5, 7, 9), snr=c(5, 7, 9),
                        metaData=list(a=1, b=2:3))

test_that("mergeMassPeaks", {
  expect_equal(unname(mergeMassPeaks(list(p[[1]], p[[1]], p[[2]], p[[2]]),
                                     labels=c(1:2, 1:2))),
               list(meanP, meanP))
  expect_equal(unname(mergeMassPeaks(pNA[rep(1:2, each=2)],
                                     labels=rep(1:2, times=2), ignore.na=FALSE)),
               list(meanPNA, meanPNA))
  expect_equal(unname(mergeMassPeaks(list(p[[1]], p[[1]], p[[2]], p[[2]]),
                                     labels=c(1:2, 1:2), method="sum")),
               list(sumP, sumP))
  expect_equal(unname(mergeMassPeaks(pNA[rep(1:2, each=2)], method="median",
                      ignore.na=FALSE)),
               createMassPeaks(mass=1:3, intensity=c(2.5, 3.5, 1.5),
                               snr=c(2.5, 3.5, 1.5),
                               metaData=list(a=1, b=rep(2:3, each=2))))
})

test_that("mergeMassPeaks throws errors", {
  expect_error(mergeMassPeaks(1:3),
               "no list of MALDIquant::MassPeaks objects")
  expect_error(mergeMassPeaks(list()),
               "no list of MALDIquant::MassPeaks objects")
  expect_error(mergeMassPeaks(s),
               "no list of MALDIquant::MassPeaks objects")
})

test_that(".mergeMassPeaks", {
  expect_equal(MALDIquant:::.mergeMassPeaks(p), meanP)
  expect_equal(MALDIquant:::.mergeMassPeaks(p, fun=colSums), sumP)
})

test_that(".mergeMetaData", {
  l <- list(a=list(numbers=1:3, lists=list(a=1, b=2), characters=c("a", "b")),
            b=list(numbers=1:3, lists=list(a=1, b=2), characters=c("b", "c")))
  r <- list(numbers=1:3, lists=list(a=1, b=2),
            characters=c("a", "b", "b", "c"))
  expect_identical(MALDIquant:::.mergeMetaData(l), r)
  l <- list(a=list(numbers=1:2, lists=list(a=1, b=2, d=4),
                   characters=c("a", "b", "c")),
            b=list(numbers=1:3, lists=list(a=1, b=2), characters=c("b", "c")))
  r <- list(numbers=c(1:2, 1:3), lists=list(list(a=1, b=2, d=4),
                                            list(a=1, b=2)),
            characters=c("a", "b", "c", "b", "c"))
  expect_identical(MALDIquant:::.mergeMetaData(l), r)
})
