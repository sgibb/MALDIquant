## Copyright 2012-2013 Sebastian Gibb
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


## movingAverage
##  runs a simple 2-side moving average.
##
## params:
##  y: double, intensity values
##  halfWindowSize integer, half window size.
##
## returns:
##  double
##
movingAverage <- function(y, halfWindowSize=2) {
  .stopIfNotIsValidHalfWindowSize(halfWindowSize, n=length(y))
  windowSize <- 2*halfWindowSize+1
  return(.filter(y, hws=halfWindowSize,
                 coef=matrix(1/windowSize, nrow=windowSize, ncol=windowSize)))
}

## savitzkyGolay
##  runs a savitzky golay filter
##
## Savitzky, A., & Golay, M. J. (1964). Smoothing and differentiation of data
## by simplified least squares procedures. Analytical chemistry, 36(8), 1627-1639.
##
## params:
##  y: double, intensity values
##  halfWindowSize: integer, half window size.
##  polynomialOrder: integer, polynomial order of sg-filter
##
## returns:
##  double
##
savitzkyGolay <- function(y, halfWindowSize=10, polynomialOrder=3) {
  .stopIfNotIsValidHalfWindowSize(halfWindowSize, n=length(y))

  windowSize <- 2*halfWindowSize+1

  if (windowSize < polynomialOrder) {
    stop("The window size has to be larger than the polynomial order.")
  }
  return(.filter(y, hws=halfWindowSize,
                 coef=.savitzkyGolayCoefficients(m=halfWindowSize,
                                                 k=polynomialOrder)))
}

## .savitzkyGolayCoefficients
##
## Savitzky, A., & Golay, M. J. (1964). Smoothing and differentiation of data
## by simplified least squares procedures. Analytical chemistry, 36(8), 1627-1639.
##
## Implementation based on:
## Steinier, J., Termonia, Y., & Deltour, J. (1972). Comments on Smoothing and
## differentiation of data by simplified least square procedure.
## Analytical Chemistry, 44(11), 1906-1909.
##
## Implemention of left/right extrema based on:
## sgolay in signal 0.7-3/R/sgolay.R by Paul Kienzle <pkienzle@users.sf.net>
## modificated by Sebastian Gibb <mail@sebastiangibb.de>
##
## params:
##  m: integer, half window size
##  k: integer, polynomial order (k == 0 = moving average)
.savitzkyGolayCoefficients <- function(m, k=3) {
  k <- 0:k
  nm <- 2*m+1
  nk <- length(k)
  K <- matrix(k, nrow=nm, ncol=nk, byrow=TRUE)

  ## filter is applied to -m:m around current data point
  ## to avoid removing (NA) of left/right extrema
  ## lhs: 0:2*m
  ## rhs: (n-2m):n

  ## filter matrix contains 2*m+1 rows
  ## row 1:m == lhs coef
  ## row m+1 == typical sg coef
  ## row (n-m-1):n == rhs coef
  F <- matrix(double(), nrow=nm, ncol=nm)
  for (i in 1:(m+1)) {
    M <- matrix((1:nm)-i, nrow=nm, ncol=nk, byrow=FALSE)
    X <- M^K
    T <- solve(t(X) %*% X) %*% t(X)
    F[i, ] <- T[1, ]
  }
  ## rhs (row (n-m):n) are equal to reversed lhs
  F[(m+2):nm, ] <- rev(F[1:m, ])

  return(F)
}

## .filter
##  remove time series attributes and NA at left/right extrema
.filter <- function(x, hws, coef) {
  n <- length(x)
  w <- 2*hws+1
  y <- stats::filter(x=x, filter=coef[hws+1, ], sides=2)
  attributes(y) <- NULL

  ## fix left/right extrema
  y[1:hws] <- head(coef, hws) %*% head(x, w)
  y[(n-hws+1):n] <- tail(coef, hws) %*% tail(x, w)
  return(y)
}

