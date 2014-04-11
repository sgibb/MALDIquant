## Copyright 2011-2014 Sebastian Gibb
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

## AbstractMassObject
setMethod(f="transformIntensity",
          signature=signature(object="AbstractMassObject"),
          definition=function(object,
                              method=c("sqrt", "log", "log2", "log10")) {

  ## empty spectrum?
  if (.isEmptyWarning(object)) {
    return(object)
  }

  method <- match.arg(method)

  fun <- switch(method,
         "sqrt" = {
           sqrt
         },
         "log" = {
           log
         },
         "log2" = {
           log2
         },
         "log10" = {
           log10
         },
         {
           stop("Unknown ", sQuote("method"), ".")
         }
  )

  return(.transformIntensity(object, fun=fun))
})

## AbstractMassObject
setMethod(f=".transformIntensity",
          signature=signature(object="AbstractMassObject"),
          definition=function(object, fun, na.rm=TRUE, ...) {

  if (!isEmpty(object)) {
    fun <- match.fun(fun)

    object@intensity <- fun(object@intensity, ...)

    belowZeroIdx <- which(object@intensity < 0L)

    if (length(belowZeroIdx)) {
      warning("Negative intensities generated. Replaced by zeros.")
      object <- .replaceNegativeIntensityValues(object, warn=FALSE)
    }

    if (na.rm) {
      naIdx <- which(!is.na(object@intensity))
      object@intensity <- object@intensity[naIdx]
      object@mass <- object@mass[naIdx]
    }
  }

  return(object)
})

## list
setMethod(f="transformIntensity",
          signature=signature(object="list"),
          definition=function(object, ...) {


  ## test arguments
  .stopIfNotIsMassObjectList(object)

  return(lapply(object, transformIntensity, ...))
})

## list
setMethod(f=".transformIntensity",
          signature=signature(object="list"),
          definition=function(object, ...) {

  ## test arguments
  .stopIfNotIsMassObjectList(object)

  return(lapply(object, .transformIntensity, ...))
})

