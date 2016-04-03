context("monoisotopicPeaks")

p <- createMassPeaks(mass=995:1005,
                     intensity=c(100, 10, 30, 10, 40, 550, 330, 110, 10, 5, 15))
m <- createMassPeaks(mass=1000, intensity=550)

test_that("monoisotopicPeaks", {
  expect_equal(monoisotopicPeaks(p), m)
})

test_that("detectPeaks works with list of MassPeaks objects", {
  expect_error(monoisotopicPeaks(list(x=1, y=1)),
               "no list of MALDIquant::MassPeaks objects")
  expect_error(monoisotopicPeaks(list(createMassSpectrum(1, 1),
                                      createMassSpectrum(1, 1)),
               "no list of MALDIquant::MassPeaks objects"))
  expect_equal(monoisotopicPeaks(list(p, p)), list(m, m))
})
