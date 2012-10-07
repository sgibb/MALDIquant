## Copyright 2012 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This file is part of MALDIquant for R and related languages.
##
## PACKAGE is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## PACKAGE is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with PACKAGE. If not, see <http://www.gnu.org/licenses/>

## warpingFunction functions called by determineWarpingFunction
##
## requirements:
##  A function with three arguments: x, difference, ... .
##  This function has to return a function with only an argument (x) which does
##  the warping.

## .warpingFunctionLowess
##  lowess based determination of warping function [default]
##
## params:
##  x: double, original mass
##  d: double, corresponding difference to reference
##  ...: additional arguments possible (see ?lowess for details)
##
## returns:
##  function(x)
##
.warpingFunctionLowess <- function(x, d, ...) {
    lo <- lowess(x=x, y=d, ...);
    return(approxfun(x=lo$x, y=lo$y, rule=2));
}

## .warpingFunctionCubic
##  3nd order polynomial based determination of warping function
##
## params:
##  x: double, original mass
##  d: double, corresponding difference to reference
##  ...: additional arguments possible (see ?lm for details)
##
## returns:
##  function(x)
##
.warpingFunctionCubic <- function(x, d, ...) {
    l <- lm(y ~ x1+x2+x3, data=list(x1=x, x2=x^2, x3=x^3, y=d), ...);
    co <- coef(l);
    return(function(x) { return (co[1]+x*co[2]+x^2*co[3]+x^3*co[4]) });
}

