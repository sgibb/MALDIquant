## Copyright 2011-2015 Sebastian Gibb
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

## .estimateNoise
##  estimating the noise of a spectrum
##
## params:
##  x: vector of x values (mass)
##  y: vector of y values (intensity)
##  method: method to use
##  ...: further arguments passed to "method"
##
## returns:
##  numeric, estimated noise (y)
##
.estimateNoise <- function(x, y, method=c("MAD", "SuperSmoother"), ...) {

  method <- match.arg(method)

  switch(method,
         "MAD" = {
           .estimateNoiseMad(x, y)
         },
         "SuperSmoother" = {
           .estimateNoiseSuperSmoother(x, y, ...)
         }
  )
}

## estimateNoiseMad
##  estimate noise by calculating mad over intensity values
##
## params:
##  x: vector of x values
##  y: vector of y values
##
## returns:
##  numeric, estimated noise (y)
##
.estimateNoiseMad <- function(x, y) {
  rep.int(stats::mad(y), length(x))
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
##  numeric, estimated noise (y)
##
.estimateNoiseSuperSmoother <- function(x, y, ...) {
  stats::supsmu(x=x, y=y, ...)$y
}
