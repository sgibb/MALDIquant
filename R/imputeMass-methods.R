## Copyright 2011-2013 Sebastian Gibb
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

## MassSpectrum
setMethod(f="imputeMass",
          signature=signature(object="MassSpectrum"),
          definition=function(object, verbose=FALSE) {

  ## empty spectrum?
  if (.isEmptyWarning(object)) {
    return(object)
  }

  l <- length(object)

  ## 1. derivation
  d <- diff(object@mass)

  ## this easy approach fails on CompassXport's 32bit output
  ## 2. derivation
  dd <- diff(d)

  ## minimal slope
  #aIdx <- which.min(abs(dd))
  #a <- dd[aIdx]
  a <- min(abs(dd))

  ## look for a continous range of minimal difference
  r <- rle(dd)
  b <- r$length >= 2
  if (any(b)) {
    minDiffRangeIdx <- which(which.min(r$values[b]) & b)[1]-1
    aIdx <- sum(r$length[0:minDiffRangeIdx])+1
  } else {
    aIdx <- which.min(abs(dd))
  }

  if (a == 0) {
    ## equal step size (2. derivation==0)
    n <- ((object@mass[l]-object@mass[1])/median(d))+1
    indexFunction <- approxfun(c(object@mass[1], object@mass[l]), c(1, n))
    index <- indexFunction(object@mass)
  } else {
    ## min(mass) <- m[aIdx] + sum(i)*a - i*d[aIdx]
    ## min(mass) <- m[aIdx] + (i(i+1))/2*a - i*d[aIdx]
    ## 0 = a/2 * i*i + (a/2-d[aIdx]) * i + m[aIdx]-min(mass)
    b <- a/2-d[aIdx]
    c <- object@mass[aIdx]-object@mass[1]
    nDown <- ceiling( (-b-sqrt(b*b-(4*a/2*c)))/a )

    ## estimate linear model (1. derivation)
    missIdx <- nDown-aIdx+1
    firstDerivation <- lm("y ~ x",
      data=list(x=c(aIdx, aIdx+1)+missIdx, y=d[aIdx:(aIdx+1)]))
    co <- coef(firstDerivation)

    ## antiderivate
    ## F(i) <- 0.5*co[2] * (0:(n-1))^2 + co[1] * (0:(n-1)) + min(object@mass)
    a <- 0.5*co[2]
    b <- co[1]
    c <- object@mass[1]-object@mass

    index <- ( (-b+sqrt(b*b-4*a*c))/(2*a) )+1
  }

  ## round calculate (double) indices to integer values
  rIndex <- round(index)

  ## any rounding errors?
  dIndex <- round(diff(index))
  dRIndex <- diff(rIndex)

  ## fix rounding errors
  if (any(dIndex!=dRIndex)) {
    ddIndex <- dIndex-dRIndex
    for (i in seq(along=ddIndex)) {
      if (ddIndex[i] != 0) {
        rIndex[(i+1):l] <- rIndex[(i+1):l]+ddIndex[i]
      }
    }
  }

  ## n == max(rIndex); but mass are already sorted
  n <- rIndex[l]

  ## how many values have to be estimated
  imputed <- n-l

  if (imputed > 0) {
    ## fill gaps
    mass <- double(n)
    ## use approxfun to estimate missing mass values:
    ## in most cases mass ~ index^2 but we don't need these mass values
    ## the number of indicies is important for the following preprocessing
    ## steps (baseline correction, smoothing, peak detection)
    massFunction <- approxfun(rIndex, object@mass)
    missingMassIdx <- (1:n)[-rIndex]
    mass[missingMassIdx] <- massFunction(missingMassIdx)
    ## worse than approxfun:
    #mass[missingMassIdx] <-
    #   object@mass[1]+cumsum(co[2]*(0:(n-2))+co[1])[missingMassIdx-1]
    mass[rIndex] <- object@mass

    intensity <- double(n)
    intensity[rIndex] <- object@intensity

    ## fix MassSpectrum object
    object@mass <- mass
    object@intensity <- intensity
  }

  if (verbose) {
    message(imputed, " mass ", ifelse(imputed == 1, "value", "values"),
            " (", round(imputed/n*100, digits=2), "%) imputed.")
  }

  return(object)
})

