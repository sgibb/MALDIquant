## Copyright 2013-2016 Sebastian Gibb
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

.colMedians <- function(x, na.rm=FALSE) {
  stopifnot(is.matrix(x), is.logical(na.rm))
  .Call("C_colMedians", x, na.rm)
}

#' .colMaxs
#'
#' Similar to apply(x, 2, max).
#'
#' @param x matrix/data.frame
#' @return double
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.colMaxs <- function(x) {
  x[max.col(t(x), ties.method="first") + 0L:(ncol(x) - 1L) * nrow(x)]
}

#' .colCors
#'
#' Calculate the correlation for two matrices columnwise.
#'
#' @param x matrix/data.frame
#' @return double
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.colCors <- function(x, y, na.rm=FALSE) {
  stopifnot(is.matrix(x) && is.matrix(y))
  stopifnot(all(dim(x) == dim(y)))

  if (na.rm) {
    isNA <- is.na(x) | is.na(y)
    x[isNA] <- NA_real_
    y[isNA] <- NA_real_
  }

  cmX <- colMeans(x, na.rm=na.rm)
  cmY <- colMeans(y, na.rm=na.rm)

  (colMeans(x * y, na.rm=na.rm) - (cmX * cmY)) /
    (sqrt(colMeans(x * x, na.rm=na.rm) - cmX * cmX) *
     sqrt(colMeans(y * y, na.rm=na.rm) - cmY * cmY))
}

#' .colSd
#'
#' Similar to colMeans but calculates the sd. Should be identical to
#' apply(x, 2, sd, na.rm).
#' based on: http://stackoverflow.com/questions/17549762/is-there-such-colsd-in-r/17551600#17551600
#'
#' @param x matrix/data.frame
#' @param na.rm logical. Should missing values (including \code{NaN})
#' be omitted from the calculations?
#' @return double
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.colSd <- function(x, na.rm=FALSE) {
  if (na.rm) {
    n <- colSums(!is.na(x))
  } else {
    n <- nrow(x)
  }
  colVar <- colMeans(x * x, na.rm=na.rm) - (colMeans(x, na.rm=na.rm))^2L
  sqrt(colVar * n/(n - 1L))
}
