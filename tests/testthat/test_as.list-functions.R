p <- list(createMassPeaks(mass=1:4, intensity=11:14),
          createMassPeaks(mass=2:5, intensity=22:25))

l <- list(
    sample = rep(1:2, each = 4),
    i = c(1:4, 2:5),
    mass = 1:5
)

test_that(".as.occurrence.list.MassObjectList throws errors", {
  expect_error(.as.occurrence.list.MassObjectList(p[[1]]),
               "no list of MALDIquant::AbstractMassObject objects")
  expect_error(.as.occurrence.list.MassObjectList(list()),
               "no list of MALDIquant::AbstractMassObject objects")
  expect_error(.as.occurrence.list.MassObjectList(list(p, l)),
               "no list of MALDIquant::AbstractMassObject objects")
})

test_that(".as.occurrence.list.MassObjectList", {
  expect_identical(.as.occurrence.list.MassObjectList(p), l)
})
