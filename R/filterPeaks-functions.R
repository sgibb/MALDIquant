## Copyright 2012-2013 Sebastian Gibb
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

## filterPeaks
##  filter peaks which are not frequently represented in different samples
##
## params:
##  l: list of MassPeaks objects
##  minFrequency: double, minimal frequency of a peak to be not removed
##  minNumber: double, minimal (absolute) number of peaks to be not removed
##  labels: labelwise filtering
##
## returns:
##  a list of adjusted MassPeaks objects
##
filterPeaks <- function(l, minFrequency, minNumber, labels) {

  ## test parameters
  .stopIfNotIsMassPeaksList(l)

  if (missing(minFrequency)) {
    minFrequency <- NA
  } else {
    if (minFrequency > 1L) {
      minFrequency <- 1L
      warning(sQuote("minFrequency"),
              " > 1 does not make sense! Using 1 instead.")
    }

    if (minFrequency < 0L) {
      minFrequency <- 0L
      warning(sQuote("minFrequency"),
              " < 0 does not make sense! Using 0 instead.")
    }
  }

  if (missing(minNumber)) {
    minNumber<- NA
  } else {
    if (minNumber < 0L) {
      minNumber <- 0L
      warning(sQuote("minNumber"), " < 0 does not make sense! Using 0 instead.")
    }
  }

  if (!is.na(minFrequency) && !is.na(minNumber)) {
    warning(sQuote("minFrequency"), " and ", sQuote("minNumber"),
            " arguments are given. Choosing the higher one.")
  }

  return(.doByLabels(l, labels=labels, FUN=.filterPeaks,
                     minFrequency=minFrequency, minNumber=minNumber))
}

.filterPeaks <- function(l, minFrequency, minNumber=NA) {
  n <- length(l)

  ## minNumber have to be smaller than length(l)
  if (!is.na(minNumber) && minNumber > n) {
    minNumber <- n
    warning(sQuote("minNumber"), " > ", sQuote("length(l)"),
            " does not make sense! Using ", n, " instead.")
  }

  ## calculate minimal number of peaks
  minPeakNumber <- max(minFrequency*n, minNumber, na.rm=TRUE)

  ## fetch mass
  mass <- sort(unique(.unlist(lapply(l, function(x)x@mass))), method="quick")

  ## generate peak matrix
  pm <- intensityMatrix(l)
  exclude <- .unlist(apply(pm, 2L, function(x) {
    return(sum(!is.na(x)) < minPeakNumber)
  }))
  exclude <- mass[exclude]

  l <- lapply(l, function(x) {
    e <- x@mass %in% exclude
    x@mass <- x@mass[!e]
    x@intensity <- x@intensity[!e]
    x@snr <- x@snr[!e]
    return(x)
  })

  return(l)
}

