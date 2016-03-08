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
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with MALDIquant. If not, see <http://www.gnu.org/licenses/>

## .colMedians
##  column-wise median (similar to colSums/colMeans)
##
## params:
##  x: vector of x values
##  na.rm: should missing values excluded?
##
## returns:
##  numeric
##
.colMedians <- function(x, na.rm=FALSE) {
  stopifnot(is.matrix(x))
  stopifnot(is.logical(na.rm))
  .Call("C_colMedians", x, na.rm)
}

## .colMads
##  column-wise mad (similar to colSums/colMeans)
##
## params:
##  x: vector of x values
##  constant: scale factor
##  na.rm: should missing values excluded?
##
## returns:
##  numeric
##
.colMads <- function(x, constant=1.4826, na.rm=FALSE) {
  center <- .colMedians(x, na.rm=na.rm)
  constant * .colMedians(t(abs(t(x) - center)), na.rm=na.rm)
}
