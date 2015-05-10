## Copyright 2012-2015 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This file is part of MALDIquant for R and related languages.
##
## MALDIquant is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## MALDIquant is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with MALDIquant. If not, see <http://www.gnu.org/licenses/>

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
