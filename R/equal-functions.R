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

## .equal
##  test whether two numeric vectors are equal (not identical); avoids floating
##  point rounding problems.
##
## params:
##  x: numeric vector
##  y: numeric vector
##  tolerance: double, maximal allowed deviation to be treated as equal
##
## returns:
##  TRUE/FALSE
##
.equal <- function(x, y, tolerance=.Machine$double.eps^0.5) {
  nx <- length(x)
  ny <- length(y)

  if (nx != ny) {
    return(FALSE)
  }

  if (nx == 0) {
    return(TRUE)
  }

  if (!is.numeric(x) || !is.numeric(y)) {
    return(NA)
  }

  return(mean(abs(x-y)) <= tolerance)
}

