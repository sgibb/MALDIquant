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

## filterPeaks 
##  filter peaks which are not frequently represented in different samples 
##
## params:
##  l: list of MassPeaks objects
##  minFrequency: double, minimal frequency of a peak to be not removed 
##  labels: labelwise filtering
##
## returns:
##  a list of adjusted MassPeaks objects
##
filterPeaks <- function(l, minFrequency, labels) {

    ## test parameters
    .stopIfNotMassPeaksList(l);
    
    if (minFrequency > 1) {
        minFrequency <- 1;
        warning(sQuote("minFrequency"), 
                " > 1 does not make sense! Using 1 instead.");
    }
    
    if (minFrequency < 0) {
        minFrequency <- 0;
        warning(sQuote("minFrequency"), 
                " < 0 does not make sense! Using 0 instead.");
    }
    
    return(.doByLabels(l, labels=labels, FUN=.filterPeaks,
                       minFrequency));
}

.filterPeaks <- function(l, minFrequency) {
  
    ## calculate minimal number of peaks
    minPeakNumber <- floor(minFrequency*length(l));

    ## fetch mass
    mass <- sort(unique(.unlist(lapply(l, function(x)x@mass))), method="quick");

    ## generate peak matrix
    pm <- intensityMatrix(l);
    exclude <- .unlist(apply(pm, 2, function(x)(sum(!is.na(x))<minPeakNumber)));
    exclude <- mass[exclude];

    l <- lapply(l, function(x) {
                       e <- x@mass %in% exclude;
                       x@mass <- x@mass[!e];
                       x@intensity <- x@intensity[!e];
                       return(x);
    });

    return(l);
}
