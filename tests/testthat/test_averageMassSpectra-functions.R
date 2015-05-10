context("averageMassSpectra")

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

test_that("averageMassSpectra", {
  expect_equal(unname(averageMassSpectra(list(s[[1]], s[[1]], s[[2]], s[[2]]),
                                         labels=c(1:2, 1:2))),
               list(meanS, meanS))
  expect_equal(unname(averageMassSpectra(list(s[[1]], s[[1]], s[[2]], s[[2]]),
                                         labels=c(1:2, 1:2), method="median")),
               list(meanS, meanS))
  expect_equal(unname(averageMassSpectra(list(s[[1]], s[[1]], s[[2]], s[[2]]),
                                         labels=c(1, 1, 2, 2), method="median")),
               s)
  expect_equal(unname(averageMassSpectra(list(s[[1]], s[[1]], s[[2]], s[[2]]),
                                         labels=c(1:2, 1:2), method="sum")),
               list(sumS, sumS))
})

test_that("averageMassSpectra throws errors", {
  expect_error(averageMassSpectra(1:3),
               "no list of MALDIquant::MassSpectrum objects")
  expect_error(averageMassSpectra(list()),
               "no list of MALDIquant::MassSpectrum objects")
  expect_error(averageMassSpectra(p),
               "no list of MALDIquant::MassSpectrum objects")
})

test_that("averageMassSpectra works with empty spectra", {
  e <- createMassSpectrum(mass=double(), intensity=double(),
                          metaData=list(a=1, b=2))
  expect_equal(unname(averageMassSpectra(list(s[[1]], e))), s[[1]])
  expect_equal(unname(averageMassSpectra(list(e, e))), e)
})

test_that(".averageMassSpectra", {
  expect_equal(MALDIquant:::.averageMassSpectra(s), meanS)
  expect_equal(MALDIquant:::.averageMassSpectra(s, fun=colSums), sumS)
})
