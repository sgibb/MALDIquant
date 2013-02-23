## Copyright 2011-2013 Sebastian Gibb
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
mergeMassPeaks <- function(l, labels, fun=mean, ...) {

  ## test parameters
  .stopIfNotIsMassPeaksList(l)

  return(.doByLabels(l=l, labels=labels, FUN=.mergeMassPeaks, fun=fun, ...))
}

## mergeMassSpectra
##  merge MassSpectrum objects
##
## params:
##  l: list of MassSpectrum objects
##  labels: factor, labels for samples
##  fun: merge function
##
## returns:
##  a new MassSpectrum object or a list of new MassSpectra objects
##
mergeMassSpectra <- function(l, labels, fun=mean, ...) {

  ## test parameters
  .stopIfNotIsMassSpectrumList(l)

  return(.doByLabels(l=l, labels=labels, FUN=.mergeMassSpectra, fun=fun, ...))
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
.mergeMassPeaks <- function(l, fun=mean, na.rm=TRUE, ...) {

  ## create a matrix which could merged
  m <- intensityMatrix(l)

  mass <- as.double(colnames(m))

  ## avoid named intensity/snr slot
  colnames(m) <- NULL

  ## merge intensities
  intensity <- .merge(m, fun=fun, ...)

  ## merge snr
  for (i in seq(along=l)) {
    m[i, !is.na(m[i, ]) ] <- l[[i]]@snr
  }
  snr <- .merge(m, fun=fun, ...)

  ## merge metaData
  metaData <- .mergeMetaData(lapply(l, function(x)x@metaData))

  return(createMassPeaks(mass=mass, intensity=intensity, snr=snr,
                         metaData=metaData))
}

## .mergeMassSpectra
##  merge MassSpectrum objects
##
## params:
##  l: list of MassSpectrum objects
##  fun: merge function
##
## returns:
##  a new MassSpectrum object
##
.mergeMassSpectra <- function(l, fun=mean, ...) {

  ## very simple score to find the "best" spectrum
  simpleScore <- function(x) {return(max(x@intensity)/mean(x@intensity))}

  ## use highest scored spectrum as reference
  maxScore <- which.max(vapply(l, simpleScore, double(1)))

  mass <- l[[maxScore]]@mass

  ## interpolate not existing masses
  approxSpectra <- lapply(l, approxfun)

  intensities <- lapply(approxSpectra, function(x)x(mass))

  ## create a matrix which could merged
  m <- do.call(rbind, intensities)

  ## merge intensities
  intensity <- .merge(m, fun=fun, ...)

  ## merge metaData
  metaData <- .mergeMetaData(lapply(l, function(x)x@metaData))

  return(createMassSpectrum(mass=mass, intensity=intensity, metaData=metaData))
}

## .mergeMetaData
## merge different metaData by equal list names
##
## params
##  m: list of metaData
##
## returns:
##  merged list
##
.mergeMetaData <- function(m) {

  .flat <- function(x) {return(unname(unlist(x)))}

  nm <- names(m[[1]])
  names(nm) <- nm
  m <- lapply(nm, function(n) {
    cur <- m[[1]][[n]]
    all <- lapply(m, function(x)x[[n]])

    if (!all(.flat(cur) == .flat(all))) {
      if (!is.list(cur)) {
        all <- unlist(all)
      }
      return(unname(all))
    } else {
      return(cur)
    }
  })
  return(m)
}

## .merge
##  merge matrix columns
##
## params:
##  m: matrix
##  fun: merge function
##  na.rm: logical, remove NA's?
##
## returns:
##  a vector (double)
##
.merge <- function(m, fun, na.rm=TRUE, ...) {

  fun <- match.fun(fun)

  if (identical(fun, mean)) {
    m <- colMeans(m, na.rm=na.rm)
  } else if (identical(fun, sum)) {
    m <- colSums(m, na.rm=na.rm)
  } else {
    ## mean, median, sum etc. have their own na.rm argument but maybe some other
    ## curious merge function lacks na.rm
    ## [would cause the error: unused argument(s) (na.rm = T)]
    ## thats why we have to remove NA's for our own
    if (na.rm) {
      m <- apply(X=m, MARGIN=2, FUN=function(x){
        return(fun(x[!is.na(x)], ...))
      })
    } else {
      m <- apply(X=m, MARGIN=2, FUN=fun, ...)
    }
  }
  return(m)
}

