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
  pub <- matrix(c(12359.8, 13682.5, 14178.3, 14305.5, 25449.2, 66737.6,
                  1.0, 0.9, 0.6, 0.6, 0.6, 38.7,
                  11, 9, 8, 9, 17, 46), ncol = 3,
                dimnames = list(names(proteins), c("mw", "sd", "z")))

  res <- lapply(proteins, MALDIquant:::.esiprot)
  res <- do.call(rbind, res)

  expect_equal(round(res - 1e-3, 1), pub)
})

test_that(".tembed", {
  expect_equal(MALDIquant:::.tembed(1:5, 3), t(embed(1:5, 3)))
  expect_equal(MALDIquant:::.tembed(1:10, 5), t(embed(1:10, 5)))
})
