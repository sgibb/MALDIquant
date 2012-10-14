## Copyright 2011-2012 Sebastian Gibb
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

## estimateNoiseMad
##  estimate noise by calculating mad over intensity values 
##
## params:
##  x: vector of x values
##  y: vector of y values
##
## returns:
##  a matrix of the estimate noise (col1: mass; col2: intensity)
##
.estimateNoiseMad <- function(x, y) {
    return(cbind(x, rep(mad(y), times=length(x))));
}

## estimateNoiseSuperSmoother
##  estimate noise by using Friedman's super smoother
##
## params:
##  x: vector of x values
##  y: vector of y values
##  ...: further arguments to passed to supsmu
##
## returns:
##  a matrix of the estimate noise (col1: mass; col2: intensity)
##
.estimateNoiseSuperSmoother <- function(x, y, ...) {
    return(cbind(x, stats::supsmu(x=x, y=y, ...)$y));
}

