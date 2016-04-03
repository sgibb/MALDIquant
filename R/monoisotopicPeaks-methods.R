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
setMethod(f="monoisotopicPeaks",
          signature=signature(object="MassPeaks"),
          definition=function(object, minCor=0.95, tolerance=1e-4,
                              distance=1.00235, size=3L:10L) {
  object[.monoisotopic(x=mass(object), y=intensity(object),
                       minCor=minCor, tolerance=tolerance,
                       distance=distance, size=size)]
})

## list
setMethod(f="monoisotopicPeaks",
          signature=signature(object="list"),
          definition=function(object, ...) {

  ## test arguments
  .stopIfNotIsMassPeaksList(object)

  .lapply(object, monoisotopicPeaks, ...)
})
