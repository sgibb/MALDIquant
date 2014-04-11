context(".as.matrix")

p <- list(createMassPeaks(mass=1:4, intensity=11:14),
          createMassPeaks(mass=2:5, intensity=22:25))
s <- list(createMassSpectrum(mass=1:5, intensity=11:15),
          createMassSpectrum(mass=1:5, intensity=21:25))

mp <- matrix(c(11:14, NA, NA, 22:25), byrow=T, ncol=5, nrow=2,
             dimnames=list(NULL, 1:5))
ms <- matrix(c(11:15, 21:25), byrow=T, ncol=5, nrow=2,
             dimnames=list(NULL, 1:5))

mb <- matrix(c(rep(1L, 4), 0L, 0L, rep(1L, 4)), byrow=T, ncol=5, nrow=2,
             dimnames=list(NULL, 1:5))

test_that(".as.matrix.MassObjectsList throws errors", {
  expect_error(MALDIquant:::.as.matrix.MassObjectList(p[[1]]),
               "no list of MALDIquant::AbstractMassObject objects")
  expect_error(MALDIquant:::.as.matrix.MassObjectList(list()),
               "no list of MALDIquant::AbstractMassObject objects")
  expect_error(MALDIquant:::.as.matrix.MassObjectList(list(p, mp)),
               "no list of MALDIquant::AbstractMassObject objects")
})

test_that(".as.binary.matrix", {
  expect_error(MALDIquant:::.as.binary.matrix(p[[1]]))
})

test_that(".as.matrix.MassObjectsList", {
    expect_identical(MALDIquant:::.as.matrix.MassObjectList(p), mp)
    expect_identical(MALDIquant:::.as.matrix.MassObjectList(s), ms)
})

test_that(".as.binary.matrix", {
    expect_identical(MALDIquant:::.as.binary.matrix(mp), mb)
})