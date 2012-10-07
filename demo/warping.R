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


## warping demo


## load necessary libraries
library("MALDIquant");

## load example spectra
data("fiedler2009subset", package="MALDIquant");

## use only 4 spectra
spectra <- fiedler2009subset[seq(1, 16, by=4)];

## some preprocessing

## sqrt transform (for variance stabilization)
spectra <- transformIntensity(spectra, sqrt);

## simple 5 point moving average for smoothing spectra
movingAverage <- function(y) {return( filter(y, rep(1, 5)/5, sides=2) );}
spectra <- transformIntensity(spectra, movingAverage);

## remove baseline
spectra <- removeBaseline(spectra);

## calibrate intensity values by "total ion current"
spectra <- standardizeTotalIonCurrent(spectra);

## run peak detection
peaks <- detectPeaks(spectra);

## warping
par(mfrow=c(2, 2));
warpingFunctions <- determineWarpingFunctions(peaks, tolerance=0.001,
                                              plot=TRUE, plotInteractive=TRUE);

## warp spectra
warpedSpectra <- warpMassSpectra(spectra, warpingFunctions);
## warp peaks
warpedPeaks <- warpMassPeaks(peaks, warpingFunctions);

## compare some regions in a plot
colour <- rainbow(length(spectra));

par(mfrow=c(2, 2));

## helper function to avoid double coding
plotSpectra <- function(unwarped, warped, range) {
    plot(unwarped[[1]], main=paste("unwarped spectra (mass ", 
                                   paste(range, collapse=":"), " Da)", sep=""),
         xlim=range, ylim=c(0, 4e-4), type="n");

    for (i in seq(along=unwarped)) {
        lines(unwarped[[i]], col=colour[i]);
    }

    plot(unwarped[[1]], main=paste("warped spectra (mass ", 
                                   paste(range, collapse=":"), " Da)", sep=""),
     xlim=range, ylim=c(0, 4e-4), type="n");

    for (i in seq(along=warped)) {
        lines(warped[[i]], col=colour[i]);
    }
}

plotSpectra(spectra, warpedSpectra, c(4180, 4240));
plotSpectra(spectra, warpedSpectra, c(9200, 9400));

