## Copyright 2016 Sebastian Gibb
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

## .topNIndices
##  find indices of highest values
##
## params:
##  x: numeric value
##  n: numeric, find the highest "n" values
##
## returns:
##  a vector of indices, length could differ from "n", e.g. the highest 2 values
##  of c(1, 2, 2, 3) are c(2, 2, 3) (indices: 2:4); see ?rank for details
##
.topNIndices <- function(x, n) {
  which(rank(x, ties.method = "max") > length(x) - n)
}
