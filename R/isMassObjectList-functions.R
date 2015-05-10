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

.isMassObjectList <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }

  length(x) && all(unname(vapply(x, .isMassObject, logical(1L))))
}

.stopIfNotIsMassObjectList <- function(x) {
  if (!.isMassObjectList(x)) {
    parentCall <- deparse(sys.call(-1L))
    stop(parentCall, " : ", sQuote(deparse(substitute(x))),
         " is no list of MALDIquant::AbstractMassObject objects!", call.=FALSE)
  }
  TRUE
}

isMassSpectrumList <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }

  length(x) && all(unname(vapply(x, isMassSpectrum, logical(1L))))
}

.stopIfNotIsMassSpectrumList <- function(x) {
  if (!isMassSpectrumList(x)) {
    parentCall <- deparse(sys.call(-1L))
    stop(parentCall, " : ", sQuote(deparse(substitute(x))),
         " is no list of MALDIquant::MassSpectrum objects!", call.=FALSE)
  }
  TRUE
}

isMassPeaksList <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }

  length(x) && all(unname(vapply(x, isMassPeaks, logical(1L))))
}

.stopIfNotIsMassPeaksList <- function(x) {
  if (!isMassPeaksList(x)) {
    parentCall <- deparse(sys.call(-1L))
    stop(parentCall, " : ", sQuote(deparse(substitute(x))),
         " is no list of MALDIquant::MassPeaks objects!", call.=FALSE)
  }
  TRUE
}
