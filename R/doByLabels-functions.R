## .doByLabels
##  run a specific function labelwise
##
## params:
##  l: list of AbstractMassObject objects
##  labels: factor, labels for samples
##  fun: function
##
## returns:
##  list of modified AbstractMassObject objects
##
.doByLabels <- function(l, labels, FUN, ..., mc.cores=1L) {

  ## test parameters
  .stopIfNotIsMassObjectList(l)

  FUN <- match.fun(FUN)

  if (!missing(labels)) {
    ## drop unused levels and turn argument into factor
    if (is.factor(labels)) {
      labels <- droplevels(labels)
    } else {
      ## preserve order in labels
      labels <- factor(labels, levels=unique(labels))
    }

    if (length(labels) != length(l)) {
      stop("For each item in ", sQuote("l"), " there must be a label in ",
           sQuote("labels"), "!")
    }

    ## replace tapply by split to preserve order
    tmp <- .lapply(split(unlist(l), labels), FUN=FUN, ..., mc.cores=mc.cores)

    k <- unlist(tmp)

    if (length(k) != length(tmp)) {
      k <- unsplit(tmp, labels)
    }
  } else {
    k <- FUN(l, ...)
  }

  k
}
