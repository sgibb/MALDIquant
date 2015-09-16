## Copyright 2013-2014 Sebastian Gibb
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

## .as.matrix.MassObjectList
##  internal function to convert a list of AbstractMassObject objects into a
##  matrix
##
## params:
##  l: list of AbstractMassObject objects
##
## returns:
##  a matrix
.as.matrix.MassObjectList <- function(l) {
  .stopIfNotIsMassObjectList(l)

  mass <- .unlist(lapply(l, function(x)x@mass))
  intensity <- .unlist(lapply(l, function(x)x@intensity))
  uniqueMass <- sort.int(unique(mass), method="quick")
  n <- vapply(l, length, double(1L))
  r <- rep.int(seq_along(l), n)

  i <- findInterval(mass, uniqueMass)

  m <- matrix(NA_real_, nrow=length(l), ncol=length(uniqueMass),
              dimnames=list(NULL, uniqueMass))
  m[cbind(r, i)] <- intensity
  attr(m, "mass") <- uniqueMass
  m
}

## .as.binary.matrix
##  internal function to convert a matrix with NA to a binary one
##
## params:
##  m: matrix
##
## returns:
##  a binary matrix
.as.binary.matrix <- function(m) {
  stopifnot(is.matrix(m))
  isNA <- which(is.na(m))
  m[] <- 1L
  m[isNA] <- 0L
  mode(m) <- "integer"
  m
}
