context(".replaceNonEmptyMassObjects")

s <- createMassSpectrum(mass=1:10, intensity=1:10)
r <- createMassSpectrum(mass=11:20, intensity=11:20)
e <- createMassSpectrum(double(), double())

test_that(".replaceNonEmptyMassObjects throws errors", {
  expect_error(MALDIquant:::.replaceNonEmptyMassObjects(e, e),
               "no list of MALDIquant::AbstractMassObject objects")
  expect_error(MALDIquant:::.replaceNonEmptyMassObjects(list(), list()),
               "no list of MALDIquant::AbstractMassObject objects")
  expect_error(MALDIquant:::.replaceNonEmptyMassObjects(list(s, s), e),
               "no list of MALDIquant::AbstractMassObject objects")
  expect_error(MALDIquant:::.replaceNonEmptyMassObjects(list(s, s, e, e),
                                                        list(r)),
               "Length of non-empty list elements .* have to be equal")
})

test_that(".replaceNonEmptyMassObjects", {
  expect_identical(MALDIquant:::.replaceNonEmptyMassObjects(list(s, e, e),
                                                            list(r)),
                   list(r, e, e))
  expect_identical(MALDIquant:::.replaceNonEmptyMassObjects(list(e, s, e),
                                                            list(r)),
                   list(e, r, e))
  expect_identical(MALDIquant:::.replaceNonEmptyMassObjects(list(e, e, s),
                                                            list(r)),
                   list(e, e, r))
  expect_identical(MALDIquant:::.replaceNonEmptyMassObjects(list(s, s),
                                                            list(r, r)),
                   list(r, r))
  expect_identical(MALDIquant:::.replaceNonEmptyMassObjects(list(s, e, s),
                                                            list(r, r)),
                   list(r, e, r))
})

