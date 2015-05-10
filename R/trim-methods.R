## Copyright 2012-2015 Sebastian Gibb
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
setMethod("trim",
          signature=signature(object="AbstractMassObject", range="numeric"),
          definition=function(object, range) {
  if (length(range) != 2L) {
    stop(sQuote("range"), " has to be a vector of length 2.")
  }

  range <- .reorderRange(range)

  sel <- which(findInterval(object@mass, range, rightmost.closed=TRUE) == 1L)

  if (!length(sel)) {
    warning("The mass range (", paste0(range, collapse=":"),
            ") is outside of the stored mass values. No data points left.")
  }

  object[sel]
})

## list
setMethod("trim",
          signature=signature(object="list", range="numeric"),
          definition=function(object, range, ...) {
  .lapply(X=object, FUN=trim, range=range, ...)
})

setMethod("trim",
          signature=signature(object="list", range="missing"),
          definition=function(object, ...) {
  range <- .overlap(object)

  if (all(range == 0L)) {
    stop("No overlap found!")
  }

  .lapply(X=object, FUN=trim, range=range, ...)
})
