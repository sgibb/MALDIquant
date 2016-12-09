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
  lo <- lowess(x=x, y=d, ...)
  approxfun(x=lo$x, y=lo$y, rule=2L)
}

## .warpingFunctionLinear
##  1st order polynomial based determination of warping function
##
## params:
##  x: double, original mass
##  d: double, corresponding difference to reference
##  ...: additional arguments possible (see ?lm for details)
##
## returns:
##  function(x)
##
.warpingFunctionLinear <- function(x, d, ...) {
  l <- lm(y ~ x1, data=list(x1=x, y=d), ...)
  co <- coef(l)
  function(x) { co[1L]+x*co[2L] }
}

## .warpingFunctionQuadratic
##  2nd order polynomial based determination of warping function
##
## params:
##  x: double, original mass
##  d: double, corresponding difference to reference
##  ...: additional arguments possible (see ?lm for details)
##
## returns:
##  function(x)
##
.warpingFunctionQuadratic <- function(x, d, ...) {
  l <- lm(y ~ x1+x2, data=list(x1=x, x2=x*x, y=d), ...)
  co <- coef(l)
  function(x) { co[1L]+x*co[2L]+x*x*co[3L] }
}

## .warpingFunctionCubic
##  3rd order polynomial based determination of warping function
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
  l <- lm(y ~ x1+x2+x3, data=list(x1=x, x2=x*x, x3=x*x*x, y=d), ...)
  co <- coef(l)
  function(x) { co[1L]+x*co[2L]+x*x*co[3L]+x*x*x*co[4L] }
}
