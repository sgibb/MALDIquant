## .estimateBaseline
##  estimating the baseline of a spectrum
##
## params:
##  x: vector of x values (mass)
##  y: vector of y values (intensity)
##  method: method to use
##  ...: further arguments passed to "method"
##
## returns:
##  numeric, estimated baseline (y)
##
.estimateBaseline <- function(x, y, method=c("SNIP", "TopHat", "ConvexHull",
                                             "median"), ...) {
  method <- match.arg(method)

  switch(method,
         "SNIP" = {
           .estimateBaselineSnip(x, y, ...)
         },
         "TopHat" = {
           .estimateBaselineTopHat(x, y, ...)
         },
         "ConvexHull" = {
           .estimateBaselineConvexHull(x, y, ...)
         },
         "median" = {
           .estimateBaselineMedian(x, y, ...)
         }
  )
}

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
##  numeric, estimated baseline (y)
##
.estimateBaselineConvexHull <- function(x, y) {
  .Call(C_lowerConvexHull, x, y)
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
##  numeric, estimated baseline (y)
##
.estimateBaselineMedian <- function(x, y, halfWindowSize=100L) {
  .stopIfNotIsValidHalfWindowSize(halfWindowSize=halfWindowSize, n=length(x))

  as.vector(runmed(y, k=2L * halfWindowSize + 1L))
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
##  numeric, estimated baseline (y)
##
.estimateBaselineSnip <- function(x, y, iterations=100L, decreasing=TRUE) {
  .Call(C_snip, y, iterations, decreasing)
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
##  numeric, estimated baseline (y)
##
.estimateBaselineTopHat <- function(x, y, halfWindowSize=100L) {
  .stopIfNotIsValidHalfWindowSize(halfWindowSize=halfWindowSize, n=length(x))

  .dilation(.erosion(y, halfWindowSize=halfWindowSize),
            halfWindowSize=halfWindowSize)
}
