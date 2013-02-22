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

## C version
.localMaxima <- function(y, halfWindowSize=1) {
  y <- c(rep(0, halfWindowSize), y, rep(0, halfWindowSize))
  n <- length(y)
  i <- (halfWindowSize+1):(n-halfWindowSize)
  return(.C("R_localMaxima",
            as.double(y),
            as.integer(n),
            as.integer(halfWindowSize),
            output=logical(n),
            DUP=FALSE, ## we don't change the input vector y
            PACKAGE="MALDIquant")$output[i])
}

## R only: obsolete because too slow and too much memory usage
.localMaximaR <- function(y, halfWindowSize=1) {
  ## based on a posting of Brian Ripley on r-help mailinglist
  ## https://stat.ethz.ch/pipermail/r-help/2001-January/010704.html
  windowSize <- 2*halfWindowSize+1

  windows <- embed(c(rep(0, halfWindowSize), y, rep(0, halfWindowSize)),
                   windowSize)
  localMaxima <- max.col(windows, "last") == halfWindowSize+1

  return(localMaxima)
}

