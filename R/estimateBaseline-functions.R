## Copyright 2011-2014 Sebastian Gibb
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

## estimateBaselineConvexHull
##  estimate baseline by creating a convex hull
##
##  A. M. Andrew, "Another Efficient Algorithm for Convex Hulls in Two
##  Dimensions", Info. Proc. Letters 9, 216-219 (1979).
##  only calculate lower hull (we don't need the upper one)
##
## params:
##  x: vector of x values
##  y: vector of y values
##
## returns:
##  a matrix of the estimate baseline (col1: mass; col2: intensity)
##

.estimateBaselineConvexHull <- function(x, y) {
  return(cbind(x=x, y=.Call("C_lowerConvexHull", x, y)))
}

## estimateBaselineMedian
##  estimate baseline by computing moving median
##
## params:
##  x: vector of x values
##  y: vector of y values
##  halfWindowSize: size of local window
##
## returns:
##  a matrix of the estimate baseline (col1: mass; col2: intensity)
##
.estimateBaselineMedian <- function(x, y, halfWindowSize=100L) {
  .stopIfNotIsValidHalfWindowSize(halfWindowSize=halfWindowSize, n=length(x))

  y <- runmed(y, k=2L*halfWindowSize+1L)

  return(cbind(x, y))
}

## estimateBaselineSnip
##  estimate baseline by SNIP algorithm
##
##  SNIP algorithm based on:
##  C.G. Ryan, E. Clayton, W.L. Griffin, S.H. Sie, and D.R. Cousens.
##  "Snip, a statistics-sensitive background treatment for the quantitative
##  analysis of pixe spectra in geoscience applications."
##  Nuclear Instruments and Methods in Physics Research Section B:
##  Beam Interactions with Materials and Atoms, 34(3):396-402, 1988.
##  ISSN 0168-583X. doi:10.1016/0168-583X(88)90063-8.
##  URL http://www.sciencedirect.com/science/article/B6TJN-46YSYTJ-30/2/e0d015ceb8ea8a7bc0702a857a19750b
##
##  decreasing clipping window adapted from:
##  M. Morhac. 2009.
##  "An algorithm for determination of peak regions and baseline elimination in
##   spectroscopic data."
##  Nuclear Instruments and Methods in Physics Research Section A:
##  Accelerators, Spectrometers, Detectors and Associated Equipment, 600(2), 478-487.
##  ISSN 0168-9002. doi:10.1016/S0168-9002(97)01023-1.
##  URL http://www.sciencedirect.com/science/article/pii/S0168900297010231
##
## params:
##  x: vector of x values (only needed for create a matrix as return value)
##  y: vector of y values
##  iterations: number of iterations
##  decreasing: use a decreasing clipping window
##
## returns:
##  a matrix of the estimate baseline (col1: mass; col2: intensity)

.estimateBaselineSnip <- function(x, y, iterations=100L, decreasing=TRUE) {
  return(cbind(x=x, y=.Call("C_snip", y, iterations, decreasing)))
}

## estimateBaselineTopHat
##  estimate baseline by TopHat filter (erosion + dilation)
##
## params:
##  x: vector of x values (only needed for create a matrix as return value)
##  y: vector of y values
##  halfWindowSize: size of local window
##
## returns:
##  a matrix of the estimate baseline (col1: mass; col2: intensity)
##
.estimateBaselineTopHat <- function(x, y, halfWindowSize=100L) {

  .stopIfNotIsValidHalfWindowSize(halfWindowSize=halfWindowSize, n=length(x))

  e <- .erosion(y, halfWindowSize=halfWindowSize)
  y <- .dilation(e, halfWindowSize=halfWindowSize)

  return(cbind(x, y))
}

