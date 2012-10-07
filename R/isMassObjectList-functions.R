## Copyright 2011-2012 Sebastian Gibb
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

isMassObjectList <- function(x) {
    if (!is.list(x)) {
        return(FALSE);
    } 

    areMassObjects <- length(x) > 0 &&
                      all(unname(vapply(x, function(e) {
                           return(isMassObject(e))}, logical(1))));

    return(areMassObjects);
}

.stopIfNotMassObjectList <- function(x) {
    if (!isMassObjectList(x)) {
        parentCall <- sys.call(-1);
        stop(paste("In ", parentCall, " : ", sQuote("x"), 
                   " is no list of MALDIquant::AbstractMassObject objects!",
                   sep=""), call.=FALSE);
        return(FALSE);
    }
    return(TRUE);
}

isMassSpectrumList <- function(x) {
    if (!is.list(x)) {
        return(FALSE);
    } 

    areMassSpectrumObjects <- length(x) > 0 &&
                              all(unname(vapply(x, function(e) {
                                   return(isMassSpectrum(e))}, logical(1))));
    return(areMassSpectrumObjects);
}

.stopIfNotMassSpectrumList <- function(x) {
    if (!isMassSpectrumList(x)) {
        parentCall <- sys.call(-1);
        stop(paste("In ", deparse(parentCall), " : ", sQuote("x"), 
                   " is no list of MALDIquant::MassSpectrum objects!", sep=""),
             call.=FALSE);
        return(FALSE);
    }
    return(TRUE);
}

isMassPeaksList <- function(x) {
    if (!is.list(x)) {
        return(FALSE);
    } 

    areMassPeaksObjects <- length(x) > 0 &&
                           all(unname(vapply(x, function(e) {
                                return(isMassPeaks(e))}, logical(1))));

    return(areMassPeaksObjects);
}

.stopIfNotMassPeaksList <- function(x) {
    if (!isMassPeaksList(x)) {
        parentCall <- sys.call(-1);
        stop(paste("In ", deparse(parentCall), " : ", sQuote("x"), 
                   " is no list of MALDIquant::MassPeaks objects!", sep=""),
             call.=FALSE);
        return(FALSE);
    }
    return(TRUE);
}

