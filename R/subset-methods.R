## Copyright 2012-2013 Sebastian Gibb
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
setMethod(f="[",
          signature=signature(x="AbstractMassObject", i="numeric", j="missing"),
          definition=function(x, i, j, ..., drop=TRUE) {

  x@mass <- x@mass[i]
  x@intensity <- x@intensity[i]

  x
})

## AbstractMassObject
setMethod(f="[",
          signature=signature(x="AbstractMassObject", i="logical", j="missing"),
          definition=function(x, i, j, ..., drop=TRUE) {

  ## seems to be faster than evaluating the logical expression twice
  i <- which(i)
  x@mass <- x@mass[i]
  x@intensity <- x@intensity[i]

  x
})

setMethod(f="[",
          signature=signature(x="MassPeaks", i="numeric", j="missing"),
          definition=function(x, i, j, ..., drop=TRUE) {

  x@mass <- x@mass[i]
  x@intensity <- x@intensity[i]
  x@snr <- x@snr[i]

  x
})

## AbstractMassObject
setMethod(f="[",
          signature=signature(x="MassPeaks", i="logical", j="missing"),
          definition=function(x, i, j, ..., drop=TRUE) {

  ## seems to be faster than evaluating the logical expression twice
  i <- which(i)
  x@mass <- x@mass[i]
  x@intensity <- x@intensity[i]
  x@snr <- x@snr[i]

  x
})
