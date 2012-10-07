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

## .which.nearby 
##  a relaxed version of which
##
## params:
##  x: vector
##  y: single
##
## returns:
##  a vector of indices refer to empty AbstractMassObject objects
##
.which.nearby <- function(x, y, tolerance=0.001) {
    
    ## test parameters
    if (!is.numeric(x)) {
        stop(sQuote("x"), " has to be numeric!");
    }

    if (!is.numeric(y)) {
        stop(sQuote("y"), " has to be numeric!");
    }

    if (length(y) != 1) {
        stop("length of ", sQuote("y"), " hast to be 1!");
    }

    diff <- abs(x-y);
    minDiffIdx <- which.min(diff);

    return(ifelse((diff[minDiffIdx]/y) <= tolerance, minDiffIdx, NA));    
}
