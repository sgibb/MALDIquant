### Copyright 2012 Sebastian Gibb
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

## referencePeaks
##  calculate reference peaks (a wrapper around filterPeaks and binPeaks)
##
## params:
##  l: list of MassPeaks objects
##  labels: factor, labels for samples
##  tolerance: double, maximal deviation of a peak position to be
##             considered as same peak
##
## returns:
##  a new MassPeaks object
##
referencePeaks <- function(l, minFrequency=0.9, tolerance=0.002) {

    .stopIfNotMassPeaksList(l);

    ## find reference peaks by binning and filtering
    referencePeaks <- filterPeaks(binPeaks(l, tolerance=tolerance),
                                  minFrequency=minFrequency);

    iM <- intensityMatrix(referencePeaks);
    iM[!is.na(iM)] <- 1;

    ## set peak intensity to number of occurrence
    intensity <- colSums(iM, na.rm=TRUE)/length(l);

    return(createMassPeaks(mass=as.double(colnames(iM)),
                           intensity=intensity));
}
