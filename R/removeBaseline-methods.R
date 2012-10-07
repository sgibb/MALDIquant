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

## MassSpectrum 
setMethod(f="removeBaseline",
    signature=signature(object="MassSpectrum"),
    definition=function(object, fun,
                        ...) {

    ## empty spectrum?
    if (.isEmptyWarning(object)) {
        return(object);
    }

    ## try to use user-defined baseline estimation function
    if (!missing(fun)) {
        fun <- match.fun(fun);
        baseline <- fun(object@mass, object@intensity, ...);
    } else {
        baseline <- estimateBaseline(object=object, ...);
    }

    ## wrong baseline argument given?
    isBaselineMatrix <- is.matrix(baseline) &&
                        nrow(baseline) == length(object) &&
                        ncol(baseline) == 2;

    if (!isBaselineMatrix) {
        stop("The baseline is not a valid matrix!");
    }

    ## substract baseline
    object@intensity <- object@intensity - baseline[,2];
    
    return(object);
});

## list
setMethod(f="removeBaseline",
    signature=signature(object="list"),
    definition=function(object, ...) {

    ## test arguments
    .stopIfNotMassSpectrumList(object);

    return(lapply(object, removeBaseline, ...));
});

