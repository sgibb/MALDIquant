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
setMethod(f=".findLocalMaxima",
          signature=signature(object="MassSpectrum"),
          definition=function(object, halfWindowSize=20L) {

  if (.isEmptyWarning(object)) {
    return(matrix(ncol=2L, dimnames=list(list(), list("mass", "intensity"))))
  }

  localMaxima <- .findLocalMaximaLogical(object, halfWindowSize=halfWindowSize)

  cbind(mass=object@mass, intensity=object@intensity)[localMaxima,]
})

setMethod(f=".findLocalMaximaLogical",
          signature=signature(object="MassSpectrum"),
          definition=function(object, halfWindowSize=20L) {

  if (.isEmptyWarning(object)) {
    return(logical())
  }

  .stopIfNotIsValidHalfWindowSize(halfWindowSize=halfWindowSize,
                                  n=length(object))

  .localMaxima(object@intensity, halfWindowSize=halfWindowSize)
})
