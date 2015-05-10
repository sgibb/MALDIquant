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

## AbstractMassObject
.validAbstractMassObject <- function(object) {
  if (length(object@mass) != length(object@intensity)) {
    return(paste0("Lengths of mass (", length(object@mass),
                  ") and intensity (", length(object@intensity),
                  ") have to be equal."))
  }
  if (is.numeric(object@mass) &&
      length(object@mass) &&
      any(object@mass < 0L)) {
    warning("Negative mass values found.")
  }
  if (is.numeric(object@intensity) &&
      !isEmpty(object) &&
      any(object@intensity < 0L)) {
    warning("Negative intensity values found.")
  }
  TRUE
}

setValidity("AbstractMassObject", method=.validAbstractMassObject)

.validMassPeaks <- function(object) {
  if (length(object@intensity) != length(object@snr)) {
    return(paste0("Lengths of intensity (", length(object@intensity),
                  ") and snr (", length(object@snr),
                  ") have to be equal."))
  }
  .validAbstractMassObject(object)
}

setValidity("MassPeaks", method=.validMassPeaks)
