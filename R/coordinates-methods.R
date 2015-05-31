## Copyright 2015 Sebastian Gibb
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
setMethod(f="coordinates",
          signature=signature(object="AbstractMassObject"),
          definition=function(object, ...) {

  object@metaData$imaging$pos
})

## AbstractMassObject
setReplaceMethod(f="coordinates",
                 signature=signature(object="AbstractMassObject",
                                     value="numeric"),
                 definition=function(object, value) {

  if (!length(value) %in% c(2L, 3L)) {
    stop("2 or 3 coordinates are needed!")
  }
  object@metaData$imaging$pos <- value
  object
})

setReplaceMethod(f="coordinates",
                 signature=signature(object="AbstractMassObject",
                                     value="matrix"),
                 definition=function(object, value) {
  if (!ncol(value) %in% c(2L, 3L)) {
    stop("2 or 3 coordinates are needed!")
  }
  if (nrow(value) != 1L) {
    warning("all rows but the first are ignored!")
  }
  object@metaData$imaging$pos <- value[1L, ]
  object
})

## list
setMethod(f="coordinates",
          signature=signature(object="list"),
          definition=function(object, adjust=FALSE) {

  ## test arguments
  .stopIfNotIsMassObjectList(object)

  m <- do.call(rbind, lapply(object, coordinates))
  colnames(m) <- c("x", "y", "z")[seq_len(ncol(m))]

  if (adjust) {
    m <- apply(m, MARGIN=2L, function(x)x - min(x) + 1L)
  }
  m

})

## list
setReplaceMethod(f="coordinates",
                 signature=signature(object="list",
                                     value="matrix"),
                 definition=function(object, value) {
  ## test arguments
  .stopIfNotIsMassObjectList(object)

  .mapply("coordinates<-", object, split(value, seq_len(nrow(value))))
})
