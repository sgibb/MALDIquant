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
