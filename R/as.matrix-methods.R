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

## AbstractMassObject 
setMethod(f="as.matrix",
    signature=signature(x="AbstractMassObject"),
    definition=function(x, index) {
    
    if (missing(index)) {
        mass <- x@mass;
        intensity <- x@intensity;
    } else { 
        mass <- x@mass[index];
        intensity <- x@intensity[index];
    }

    return(matrix(c(mass, intensity), ncol=2, byrow=FALSE,
                dimnames=list(NULL, c("mass", "intensity"))));
});

