## Copyright 2013-2015 Sebastian Gibb
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

## averageMassSpectra
##  averages MassSpectrum objects
##
## params:
##  l: list of MassSpectrum objects
##  labels: factor, labels for samples
##  method: aggregation method
##
## returns:
##  a new MassSpectrum object or a list of new MassSpectra objects
##
averageMassSpectra <- function(l, labels, method=c("mean", "median", "sum"),
                               ...) {

  ## test parameters
  .stopIfNotIsMassSpectrumList(l)

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

  .doByLabels(l=l, labels=labels, FUN=.averageMassSpectra, fun=fun, ...)
}

## .averageMassSpectra
##  average MassSpectrum objects
##
## params:
##  l: list of MassSpectrum objects
##  fun: aggregation function
##  ignore.na: ignore NA/set them to 0
##
## returns:
##  a new MassSpectrum object
##
.averageMassSpectra <- function(l, fun=colMeans, mergeMetaData=TRUE) {

  fun <- match.fun(fun)

  ## merge metaData
  if (mergeMetaData) {
    metaData <- .mergeMetaData(lapply(l, function(x)x@metaData))
  } else {
    metaData <- list()
  }

  ## use the first non empty spectrum as reference
  i <- which(!vapply(l, isEmpty, logical(1L)))[1L]
  if (!is.na(i)) {
    mass <- l[[i]]@mass
  } else {
    mass <- NA_real_
  }

  ## interpolate not existing masses
  approxSpectra <- lapply(l, approxfun)

  ## get interpolated intensities
  intensityList <- lapply(approxSpectra, function(x)x(mass))

  ## create a matrix which could merged
  m <- do.call(rbind, intensityList)

  ## merge intensities
  intensity <- fun(m, na.rm=TRUE)

  ## create an empty spectrum if all intensities are NaN
  if (is.nan(intensity[1L])) {
    intensity <- double()
    mass <- double()
  }

  createMassSpectrum(mass=mass, intensity=intensity, metaData=metaData)
}
