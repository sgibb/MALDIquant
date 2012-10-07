## Copyright 2012 Sebastian Gibb
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
setMethod(f="totalIonCurrent",
    signature=signature(object="AbstractMassObject"),
    definition=function(object) {
    
    return(as.double(sum(object@intensity, na.rm=TRUE)));
});

## AbstractMassObject
setReplaceMethod(f="totalIonCurrent",
    signature=signature(object="AbstractMassObject",
                        value="numeric"),
    definition=function(object, value) {

    if (length(value) != 1) {
        stop("Length of value has to be one.");
    }

    tic <- totalIonCurrent(object);
    return(transformIntensity(object, function(x)x*value/tic));
});

