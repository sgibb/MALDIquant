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

## numeric
setMethod("esiprot",
          signature=signature(object="numeric"),
          definition=function(object, ...) {
  .esiprot(object, ...)
})

## MassPeaks
setMethod("esiprot",
          signature=signature(object="MassPeaks"),
          definition=function(object, n=2L:10L, ...) {

  if (!is.integer(n) || any(n < 2L)) {
    stop(sQuote("n"), " has to be an integer >= 2.")
  }

  n <- unique(pmin(n, length(object)))

  center <- .topNIndices(intensity(object), n=1L)[1L]

  ## build indices to test
  i <- lapply(n, .consecutiveIndices,
              x=mass(object), center=center)
  ## test left and right preferation for n %% 2L == 0
  i <- c(i, lapply(n[!n %% 2L], .consecutiveIndices,
                   x=mass(object), center=center, method="right"))
  i <- unique(i)

  res <- lapply(i, function(ii).esiprot(mass(object)[ii], ...))
  res <- do.call(rbind, res)
  res
  m <- which.min(res[, "sd"])
  n <- length(i[[m]])
  c(res[m,], n=n, fpi=i[[m]][1L], lpi=i[[m]][n])
})

## list
setMethod("esiprot",
          signature=signature(object="list"),
          definition=function(object, ...) {
  do.call(rbind, .lapply(X=object, FUN=esiprot, ...))
})
