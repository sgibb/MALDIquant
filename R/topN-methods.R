## Copyright 2016 Sebastian Gibb
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

## MassPeaks
setMethod("topN",
          signature=signature(object="MassPeaks"),
          definition=function(object, n, ...) {
  if (!is.numeric(n) || length(n) != 1L) {
    stop(sQuote("n"), " has to be an integer of length 1.")
  }
  object[.topNIndices(intensity(object), n=n)]
})

## list
setMethod("topN",
          signature=signature(object="list"),
          definition=function(object, n, ...) {
  ## test arguments
  .stopIfNotIsMassPeaksList(object)

  .lapply(X=object, FUN=topN, n=n, ...)
})
