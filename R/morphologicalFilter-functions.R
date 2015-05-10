## Copyright 2013 Sebastian Gibb
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
  .Call("C_dilation", x, halfWindowSize)
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
  .Call("C_erosion", x, halfWindowSize)
}
