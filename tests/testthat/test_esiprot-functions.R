context(".esiprot functions")

test_that(".esiprot throws errors", {
  expect_error(MALDIquant:::.esiprot(LETTERS[1:10]),
               ".*x.* has to be a numeric vector with more than 1 element.")
  expect_error(MALDIquant:::.esiprot(1),
               ".*x.* has to be a numeric vector with more than 1 element.")
  expect_error(MALDIquant:::.esiprot(1:10, range=c("A", "B")),
               ".*range.* has to be a numeric vector of length 2")
  expect_error(MALDIquant:::.esiprot(1:10, range=1),
               ".*range.* has to be a numeric vector of length 2")
  expect_error(MALDIquant:::.esiprot(1:10, range=1:2),
               ".*range.* is too small for this many peaks")
})

test_that(".esiprot", {
  # R. Winkler. 2010.
  # ESIprot: a universal tool for charge state determination and molecular
  # weight calculation of proteins from electrospray ionization mass
  # spectrometry data.
  # Rapid Communications in Mass Spectrometry 24(3): 285-294
  proteins <- list(
    "Cytochrome c"    = c(651.6, 687.7, 728.1, 773.5, 825.0, 883.9, 951.7,
                          1030.9, 1124.5),
    "Ribonuclease A"  = c(1053.5, 1141.2, 1245.0, 1369.2, 1521.2),
    "a-Lactalbumin"   = c(1091.7, 1182.5, 1290.0, 1418.8, 1576.3, 1773.3),
    "Lysozyme"        = c(1101.5, 1193.1, 1301.5, 1431.5, 1590.5),
    "a-Chymotrypsin"  = c(1019.0, 1061.4, 1107.5, 1157.8, 1212.9, 1273.5,
                          1340.4, 1414.8, 1498.0),
    "BSA"             = c(1236.1, 1261.4, 1285.6, 1309.3, 1335.6, 1362.5,
                          1391.7, 1420.3, 1451.4))

  z <- c(19, rep(13, 3), 25, 54)
  z <- mapply(function(from, to)from:to,
              from=z, to=(z-lengths(proteins)+1))

  pub <- matrix(c(12359.8, 13682.5, 14178.3, 14305.5, 25449.2, 66737.6,
                  1.0, 0.9, 0.6, 0.6, 0.6, 38.7), ncol = 2,
                dimnames = list(names(proteins), c("mw", "sd")))
  mz <- lapply(seq_along(proteins), function(i)cbind(mz=proteins[[i]], z=z[[i]]))
  pub <- cbind(pub, mz=mz)


  res <- lapply(proteins, MALDIquant:::.esiprot)
  res <- do.call(rbind, res)

  expect_equal(res, pub, tolerance=0.6)
})

test_that(".tembed", {
  expect_error(MALDIquant:::.tembed(1:10, 0), "wrong embedding dimension")
  expect_error(MALDIquant:::.tembed(1:10, 20), "wrong embedding dimension")
  expect_equal(MALDIquant:::.tembed(1:5, 3), t(embed(1:5, 3)))
  expect_equal(MALDIquant:::.tembed(1:10, 5), t(embed(1:10, 5)))
})

test_that(".consecutiveIndices", {
  m <- seq(1000, 1100, by=10)
  expect_equal(MALDIquant:::.consecutiveIndices(m, center=2, n=6), 1:6)
  expect_equal(MALDIquant:::.consecutiveIndices(m, center=4, n=4), 2:5)
  expect_equal(MALDIquant:::.consecutiveIndices(m, center=8, n=8), 4:11)
  expect_equal(MALDIquant:::.consecutiveIndices(m, center=4, n=4,
                                                method="right"), 3:6)
  expect_equal(MALDIquant:::.consecutiveIndices(m, center=4, n=5), 2:6)
  expect_equal(MALDIquant:::.consecutiveIndices(m, center=4, n=10),
               1:10)
  expect_equal(MALDIquant:::.consecutiveIndices(m, center=4, n=10,
                                                method="right"), 1:10)
  expect_equal(MALDIquant:::.consecutiveIndices(m, center=4, n=11),
               seq_along(m))
  expect_equal(MALDIquant:::.consecutiveIndices(m, center=4, n=11,
                                                method="right"),
               seq_along(m))
  expect_equal(MALDIquant:::.consecutiveIndices(m, center=8, n=8, method="right"), 4:11)
  expect_equal(MALDIquant:::.consecutiveIndices(m, center=2, n=100),
               seq_along(m))
  expect_equal(MALDIquant:::.consecutiveIndices(1:7, center=5, n=7), 1:7)
})

test_that(".esiprotMonoisotopic", {
  expect_equal(MALDIquant:::.esiprotMonoisotopic(c(1:5, 10:15)), 6)
  expect_equal(MALDIquant:::.esiprotMonoisotopic(c(1:5, 10:15), p=0.99),
               integer())
})

test_that(".esiprotAuto", {
  p <- createMassPeaks(mass=c(1:5, 10:15, seq(20, 25, by=0.5)),
                       intensity=c(rep(1, 5), 2, rep(1, 5), 3, rep(1, 10)))
  res <- list(mw=32.48, sd=7.78, mz=cbind(mz=c(9, 19), z=3:2))
  expect_equal(MALDIquant:::.esiprotAuto(p), res, tolerance=1e-3)
})
