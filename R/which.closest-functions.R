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

## .which.closest
##  a relaxed version of which (returns the nearest index)
##
## params:
##  x: numeric key value to look for
##  vec: numeric, has to be sorted
##  tolerance: accepted tolerance; Inf to choose the closest (old behaviour)
##  nomatch: if tolerance !Inf and the difference is larger than tolerance
##  return nomatch.
##
## returns:
##  a vector of indices
##
.which.closest <- function(x, vec, tolerance=Inf, nomatch=NA_integer_) {

  ## find left interval
  lIdx <- findInterval(x, vec, rightmost.closed=FALSE, all.inside=TRUE)
  rIdx <- lIdx + 1L

  ## calculate differences for left and right
  lDiff <- abs(vec[lIdx] - x)
  rDiff <- abs(vec[rIdx] - x)

  if (!is.infinite(tolerance)) {
    lIdx[lDiff > tolerance] <- nomatch
    rIdx[rDiff > tolerance] <- nomatch
  }

  d <- which(lDiff != pmin.int(lDiff, rDiff))
  lIdx[d] <- rIdx[d]
  lIdx
}
