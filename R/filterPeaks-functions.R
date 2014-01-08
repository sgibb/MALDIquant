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
##  mode: whitelist mode (group, all, none)
##
## returns:
##  a list of adjusted MassPeaks objects
##
filterPeaks <- function(l, minFrequency, minNumber, labels,
                        mode="group") {

  ## test arguments
  .stopIfNotIsMassPeaksList(l)

  ## labels
  if (missing(labels)) {
    labels <- rep(0L, length(l))
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

  mode <- match.arg(mode, choices=c("group", "all", "none"), several.ok=TRUE)

  ll <- levels(labels)
  nl <- length(ll)

  ## recycle arguments if needed
  minFrequency <- rep_len(minFrequency, nl)
  minNumber <- rep_len(minNumber, nl)
  mode <- rep_len(mode, nl)

  ## binary peak matrix (mask)
  m <- .as.binary.matrix(.as.matrix.MassObjectList(l))

  ## whitelist
  w <- matrix(0L, nrow=nrow(m), ncol=ncol(m))

  ## group indices by labels
  idx <- lapply(ll, function(x)which(labels == x))

  ## collect whitelists
  for (i in seq(along=idx)) {
    if (mode[i] != "none") {
      wl <- .whitelist(m, idx[[i]],
                       minFrequency=minFrequency[i], minNumber=minNumber[i])
      if (mode[i] == "all") {
        ## R uses columnwise recycling
        w <- t(t(w) | wl)
      } else if (mode[i] == "group") {
        ## R uses columnwise recycling
        w[idx[[i]], ] <- t(t(w[idx[[i]], ]) | wl)
      }
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

.whitelist <- function(m, rows, minFrequency, minNumber) {

  ## test arguments
  if (is.na(minFrequency) && is.na(minNumber)) {
    stop(sQuote(minFrequency), " or ", sQuote(minNumber),
         " has to be a meaningful number!")
  }

  if (!is.na(minFrequency) && minFrequency > 1L) {
    minFrequency <- 1L
    warning(sQuote("minFrequency"),
            " > 1 does not make sense! Using 1 instead.")
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

  ## minNumber have to be smaller than n
  n <- length(rows)
  if (!is.na(minNumber) && minNumber > n) {
    minNumber <- n
    warning(sQuote("minNumber"), " > ", sQuote("length(group)"),
            " does not make sense! Using ", n, " instead.")
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
    return(sum(m >= minPeakNumber))
  }
}
