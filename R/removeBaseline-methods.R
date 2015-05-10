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

## MassSpectrum
setMethod(f="removeBaseline",
          signature=signature(object="MassSpectrum"),
          definition=function(object,
                              method=c("SNIP", "TopHat", "ConvexHull",
                                       "median"),
                              ...) {
  ## empty spectrum?
  if (.isEmptyWarning(object)) {
    return(object)
  }

  ## estimate baseline
  baseline <- .estimateBaseline(x=object@mass, y=object@intensity,
                                method=method, ...)

  ## substract baseline
  object@intensity <- object@intensity - baseline

  object
})

## list
setMethod(f="removeBaseline",
          signature=signature(object="list"),
          definition=function(object, ...) {

  ## test arguments
  .stopIfNotIsMassSpectrumList(object)

  .mapply(removeBaseline, object, ...)
})
