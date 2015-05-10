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
                                        labels=as.factor(c("a", "b"))))
})

test_that(".doByLabels runs a function for group labels", {
  expect_identical(MALDIquant:::.doByLabels(l=list(s, s),
                                            FUN=function(x){ return(1) }), 1)
  expect_identical(unname(MALDIquant:::.doByLabels(l=list(s, s),
                            labels=as.factor(c("a", "b")),
                            FUN=function(x)1)),
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
                  FUN=function(x)x)

  expect_equal(c(1:2, 10:11), unname(sapply(m, function(x)metaData(x)$file)))

  ## bug #19; order changes if length of output is smaller than input
  m <- MALDIquant:::.doByLabels(l=l,
                  ## results in factor(c(2, 2, 1, 1), levels=2:1)
                  labels=rep(2:1, each=2),
                  FUN=function(x)x[[1]])

  expect_equal(c(1, 10), unname(sapply(m, function(x)metaData(x)$file)))

  ## respect order of factor
  m <- MALDIquant:::.doByLabels(l=l,
                  ## results in factor(c(2, 2, 1, 1), levels=1:2)
                  labels=factor(rep(2:1, each=2)),
                  FUN=function(x)x[[1]])

  expect_equal(c(10, 1), unname(sapply(m, function(x)metaData(x)$file)))


  ## see https://github.com/sgibb/MALDIquant/issues/1
  expect_equal(unname(MALDIquant:::.doByLabels(l, 1:4, function(x)x[[1]])), l)
  expect_equal(unname(MALDIquant:::.doByLabels(l, rep(1, 4), function(x)x)), l)
})
