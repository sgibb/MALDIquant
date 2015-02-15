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
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with MALDIquant. If not, see <http://www.gnu.org/licenses/>

## .overlap
## returns largest overlapping mass range of a list of AbstractMassObject
## objects.
##
## params:
## l: list of AbstractMassObject objects
##
## returns:
## double, minimal and maximal mass
##
.overlap <- function(l) {
  ## test argument
  .stopIfNotIsMassObjectList(l)

  ## mass values are already sorted
  leftMass <- .unlist(lapply(l, function(x)x@mass[1L]))
  rightMass <- .unlist(lapply(l, function(x)x@mass[length(x@mass)]))

  if (length(rightMass)) {
    r <- c(max(leftMass, na.rm=TRUE), min(rightMass, na.rm=TRUE))
    if (r[1L] < r[2L]) {
      return(r)
    }
  }

  ## no overlap
  c(0L, 0L)
}

## .reorderRange
## swap range values if needed
##
## params:
## x: range values
##
## returns:
## corrected range values
##
.reorderRange <- function(x) {
  ## sort range
  if (x[1L] > x[2L]) {
    x <- x[2L:1L]
  }

  x
}
