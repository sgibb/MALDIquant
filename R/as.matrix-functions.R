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

## .as.matrix.MassObjectsList
##  internal function to convert a list of AbstractMassObjects into a matrix
##
## params:
##  l: list of AbstractMassObject objects
##
## returns:
##  a matrix
.as.matrix.MassObjectList <- function(l) {
  .stopIfNotIsMassObjectList(l)

  mass <- sort(x=.unlist(lapply(l, function(x)x@mass)), method="quick")
  uniqueMass <- unique(mass)

  ## build matrix
  m <- do.call(rbind, lapply(l, function(x) {
    return(x@intensity[match(x=uniqueMass, table=x@mass, nomatch=NA)])}))

  ## set column names
  dimnames(m) <- list(NULL, c(uniqueMass))

  return(m)
}

