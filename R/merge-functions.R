## Copyright 2011-2015 Sebastian Gibb
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

## mergeMassPeaks
##  merge MassPeaks objects
##
## params:
##  l: list of MassPeaks objects
##  labels: factor, labels for samples
##  fun: merge function
##
## returns:
##  a new MassPeaks object or a list of new MassPeaks objects
##
mergeMassPeaks <- function(l, labels, method=c("mean", "median", "sum"),
                           ignore.na=TRUE, ...) {

  ## test arguments
  .stopIfNotIsMassPeaksList(l)

  method <- match.arg(method)

  fun <- switch(method,
              "mean" = {
                colMeans
              },
              "median" = {
                .colMedians
              },
              "sum" = {
                colSums
              }
  )

  .doByLabels(l=l, labels=labels, FUN=.mergeMassPeaks, fun=fun,
              ignore.na=ignore.na, ...)
}

## .mergeMassPeaks
##  merge MassPeaks objects
##
## params:
##  l: list of MassPeaks objects
##  fun: merge function
##
## returns:
##  a new MassPeaks object
##
.mergeMassPeaks <- function(l, fun=colMeans, ignore.na=TRUE) {

  fun <- match.fun(fun)

  ## create a matrix which could merged
  m <- .as.matrix.MassObjectList(l)

  mass <- attr(m, "mass")

  ## avoid named intensity/snr slot
  colnames(m) <- NULL

  isNA <- is.na(m)
  if (!ignore.na) {
    m[isNA] <- 0L
  }

  ## merge intensities
  intensity <- fun(m, na.rm=TRUE)

  ## merge snr
  for (i in seq_along(l)) {
    m[i, !isNA[i, ]] <- l[[i]]@snr
  }
  snr <- fun(m, na.rm=TRUE)

  ## merge metaData
  metaData <- .mergeMetaData(lapply(l, function(x)x@metaData))

  createMassPeaks(mass=mass, intensity=intensity, snr=snr, metaData=metaData)
}

## merge different metaData by equal list names
##
## params
##  m: list of metaData
##
## returns:
##  merged list
##
.mergeMetaData <- function(m) {

  .flat <- function(x)unname(unlist(x))

  nm <- names(m[[1L]])
  names(nm) <- nm
  lapply(nm, function(n) {
    cur <- m[[1L]][[n]]
    all <- lapply(m, function(x)x[[n]])
    len <- lapply(all, function(x)length(x))

    if (!all(length(cur) == len) ||
        !all(.flat(cur) == .flat(all))) {
      if (!is.list(cur)) {
        all <- unlist(all)
      }
      return(unname(all))
    } else {
      return(cur)
    }
  })
}
