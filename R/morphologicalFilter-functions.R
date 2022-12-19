## dilation
##  calculates moving max (dilation filter)
##
##  M. van Herk. "A Fast Algorithm for Local Minimum and Maximum Filters on
##  Rectangular and Octagonal Kernels."
##  Pattern Recognition Letters 13.7 (1992): 517-521.
##
##  J. Y. Gil and M. Werman. "Computing 2-Dimensional Min, Median and Max
##  Filters." IEEE Transactions (1996): 504-507.
##
## params:
##  x: vector of x values
##  halfWindowSize: size of local window
##
## returns:
##  moving max
##
.dilation <- function(x, halfWindowSize) {
  .Call(C_dilation, x, halfWindowSize)
}

## erosion
##  calculates moving min (erosion filter)
##
##  M. van Herk. "A Fast Algorithm for Local Minimum and Maximum Filters on
##  Rectangular and Octagonal Kernels."
##  Pattern Recognition Letters 13.7 (1992): 517-521.
##
##  J. Y. Gil and M. Werman. "Computing 2-Dimensional Min, Median and Max
##  Filters." IEEE Transactions (1996): 504-507.
##
## params:
##  x: vector of x values
##  halfWindowSize: size of local window
##
## returns:
##  moving min
##
.erosion <- function(x, halfWindowSize) {
  .Call(C_erosion, x, halfWindowSize)
}
