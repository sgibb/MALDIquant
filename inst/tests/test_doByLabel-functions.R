context(".doByLabels")

s <- createMassSpectrum(mass=1:5, intensity=1:5)

test_that(".doByLabels throws errors", {
  expect_error(MALDIquant:::.doByLabels(l=list(x=1, y=1),
                                        labels=as.factor(c("a", "b")),
                                        FUN=sum),
               "no list of MALDIquant::AbstractMassObject objects")
  expect_error(MALDIquant:::.doByLabels(l=list(s, s), labels=as.factor("a"),
                                        FUN=sum),
               "For each item in .*l.* there must be a label in .*labels.*")
  expect_error(MALDIquant:::.doByLabels(l=list(s, s),
                                        labels=as.factor(c("a", "b"))),
               "argument .*FUN.* is missing, with no default")
})

test_that(".doByLabels runs a function for group labels", {
  expect_identical(MALDIquant:::.doByLabels(l=list(s, s),
                                            FUN=function(x){ return(1) }), 1)
  expect_identical(unname(MALDIquant:::.doByLabels(l=list(s, s),
                            labels=as.factor(c("a", "b")),
                            FUN=function(x){ return(1) })),
                   c(1, 1))

  l <- list(s, s, s, s)
  l[[1]]@metaData$file <- 1
  l[[2]]@metaData$file <- 2
  l[[3]]@metaData$file <- 10
  l[[4]]@metaData$file <- 11

  ## preserve order
  m <- MALDIquant:::.doByLabels(l=l,
                  ## error because of wrong alphabetical order
                  ## always add levels
                  #labels=as.factor(paste("s", c(1, 2, 10, 11))),
                  labels=factor(paste("s", c(1, 2, 10, 11)),
                        levels=paste("s", c(1, 2, 10, 11))),
                  FUN=function(x){return(x)})

  expect_equal(unname(lapply(l, function(x)metaData(x)$file)),
               lapply(l, function(x)metaData(x)$file))

  ## see https://github.com/sgibb/MALDIquant/issues/1
  expect_equal(unname(MALDIquant:::.doByLabels(l, 1:4, function(x)x[[1]])), l)
})

