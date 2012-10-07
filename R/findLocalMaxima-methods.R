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
setMethod(f=".findLocalMaxima",
    signature=signature(object="MassSpectrum"),
    definition=function(object, 
                        halfWindowSize=20) {
        
    if (.isEmptyWarning(object)) {
        return(matrix(ncol=2, dimnames=list(list(), list("mass", "intensity"))));
    }

    if (halfWindowSize<1) {
        stop(sQuote("halfWindowSize"), "=", halfWindowSize, " is too small!");
    }

    ## based on a posting of Brian Ripley on r-help mailinglist
    ## https://stat.ethz.ch/pipermail/r-help/2001-January/010704.html
    windowSize <- 2*halfWindowSize+1;

    if (windowSize >= length(object)) {
        stop(sQuote("halfWindowSize"), " is too large! ",
             "(window size (", windowSize, ") >= number of ",
             " mass/intensity values (", length(object), "))");
    }

    windows <- embed(object@intensity, windowSize);

    localMaxima <- max.col(windows, "first") == halfWindowSize+1
    
    localMaxima <- c(rep(FALSE, halfWindowSize), localMaxima, 
                     rep(FALSE, halfWindowSize));

    m <- cbind(object@mass, object@intensity)[localMaxima,];
    
    if (!is.matrix(m)) {
        m <- t(as.matrix(m));
    }

    colnames(m) <- c("mass", "intensity");

    return(m);
});

