## Copyright 2012-2014 Sebastian Gibb
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

## local maxima function called by .findLocalMaxima
##
## This function looks for local maxima in a numeric vector.
##
## params:
##  y: double, intensity
##  halfWindowSize: numeric, half window size.
##
## returns:
##  logical vector of local maxima
##

.localMaxima <- function(y, halfWindowSize=1L) {
  y <- c(rep.int(0L, halfWindowSize), y, rep.int(0L, halfWindowSize))
  i <- (halfWindowSize + 1L):(length(y) - halfWindowSize)
  .Call("C_localMaxima", y, halfWindowSize)[i]
}
