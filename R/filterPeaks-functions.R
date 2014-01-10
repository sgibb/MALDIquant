## Copyright 2012-2014 Sebastian Gibb
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
##  labels: factor, labelwise filtering
##  mergeWhitelists: logical, apply whitelists local (FALSE) or global (TRUE)
##
## returns:
##  a list of adjusted MassPeaks objects
##
filterPeaks <- function(l, minFrequency, minNumber, labels,
                        mergeWhitelists=FALSE) {

  ## test arguments
  .stopIfNotIsMassPeaksList(l)

  ## labels
  if (missing(labels)) {
    labels <- rep_len(0L, length(l))
  }

  ## drop unused levels and turn argument into factor
  if (is.factor(labels)) {
    labels <- droplevels(labels)
  } else {
    ## preserve order in labels
    labels <- factor(labels, levels=unique(labels))
  }

  if (missing(minFrequency)) {
    minFrequency <- NA
  }

  if (missing(minNumber)) {
    minNumber <- NA
  }

  ll <- levels(labels)
  nl <- length(ll)

  if (length(labels) != length(l)) {
    stop("For each item in ", sQuote("l"), " there must be a label in ",
         sQuote("labels"), "!")
  }

  ## recycle arguments if needed
  minFrequency <- rep_len(minFrequency, nl)
  minNumber <- rep_len(minNumber, nl)
  mergeWhitelists <- mergeWhitelists[1]

  ## binary peak matrix (mask)
  m <- .as.binary.matrix(.as.matrix.MassObjectList(l))

  ## whitelist
  w <- matrix(0L, nrow=nrow(m), ncol=ncol(m))

  ## group indices by labels
  idx <- lapply(ll, function(x)which(labels == x))

  ## collect whitelists
  for (i in seq(along=idx)) {
    wl <- .whitelist(m, idx[[i]],
                     minFrequency=minFrequency[i], minNumber=minNumber[i],
                     level=ll[i])
    if (mergeWhitelists) {
      ## R uses columnwise recycling
      w <- t(t(w) | wl)
    } else {
      ## R uses columnwise recycling
      w[idx[[i]], ] <- t(t(w[idx[[i]], ]) | wl)
    }
  }

  ## apply whitelist
  w <- w & m

  ## turn matrix back into MassPeaks objects
  for (i in seq(along=l)) {
    j <- which(as.logical(m[i, ]))
    include <- which(w[i, j])
    l[[i]]@mass <- l[[i]]@mass[include]
    l[[i]]@intensity <- l[[i]]@intensity[include]
    l[[i]]@snr <- l[[i]]@snr[include]
  }

  return(l)
}

## .whitelist
##  helper function to create whitelists for filtering
##
## params:
##  m: matrix
##  rows: index of rows which should filtered
##  minFrequency: double, minimal frequency of a peak to be not removed
##  minNumber: double, minimal (absolute) number of peaks to be not removed
##  level: current level (group) name
##
## returns:
##  a logical vector representing the whitelist
##
.whitelist <- function(m, rows, minFrequency, minNumber, level) {

  ## test arguments
  if (is.na(minFrequency) && is.na(minNumber)) {
    stop(sQuote(minFrequency), " or ", sQuote(minNumber),
         " has to be a meaningful number!")
  }

  ## minFrequency should be less or equal to 100%
  if (!is.na(minFrequency) && minFrequency > 1L) {
    warning(sQuote("minFrequency"), " > 1 for level ",
            sQuote(level), ". No whitelist is created. All peaks are filtered.")
    return(FALSE)
  }

  if (!is.na(minFrequency) && minFrequency < 0L) {
    minFrequency <- 0L
    warning(sQuote("minFrequency"),
            " < 0 does not make sense! Using 0 instead.")
  }

  if (!is.na(minNumber) && minNumber < 0L) {
    minNumber <- 0L
    warning(sQuote("minNumber"), " < 0 does not make sense! Using 0 instead.")
  }

  ## minNumber should be smaller than n
  n <- length(rows)
  if (!is.na(minNumber) && minNumber > n) {
    warning(sQuote("minNumber"), " > n for level ",
            sQuote(level), ". No whitelist is created. All peaks are filtered.")
    return(FALSE)
  }

  if (!is.na(minFrequency) && !is.na(minNumber)) {
    warning(sQuote("minFrequency"), " and ", sQuote("minNumber"),
            " arguments are given. Choosing the higher one.")
  }

  ## calculate minimal number of peaks
  minPeakNumber <- max(minFrequency*n, minNumber, na.rm=TRUE)

  if (length(rows) > 1) {
    return(colSums(m[rows, ]) >= minPeakNumber)
  } else {
    return(m)
  }
}

