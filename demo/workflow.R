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


## workflow demo


## load necessary libraries
library("MALDIquant");

## load example spectra
data("fiedler2009subset", package="MALDIquant");

## sqrt transform (for variance stabilization)
spectra <- transformIntensity(fiedler2009subset, sqrt);

## simple 5 point moving average for smoothing spectra
movingAverage <- function(y) {return( filter(y, rep(1, 5)/5, sides=2) );}
spectra <- transformIntensity(spectra, movingAverage);

## remove baseline
spectra <- removeBaseline(spectra);

## run peak detection
peaks <- detectPeaks(spectra);

## align spectra by warping
## 1. create reference peaks (could be done automatically by
##    determineWarpingFunctions)
## 2. calculate individual warping functions
## 3. warp each MassPeaks object
refPeaks <- referencePeaks(peaks);
warpingFunctions <- determineWarpingFunctions(peaks, reference=refPeaks);
peaks <- warpMassPeaks(peaks, warpingFunctions);

## bin peaks
peaks <- binPeaks(peaks);

## merge technical replicates
## 1. create factors for correct assignment
nTechRep <- 2;
nBiologicalSamples <- length(peaks)/nTechRep;
samples <- factor(rep(1:nBiologicalSamples, each=nTechRep),
                  levels=1:nBiologicalSamples);

## 2. filter peaks which occur only in one of the replicates
peaks <- filterPeaks(peaks, labels=samples, minFrequency=1);

## 3. merge technical replicates
peaks <- mergeMassPeaks(peaks, labels=samples);

## prepare for statistical analysis
## 1. get cancer/control indices
filenames <- sapply(peaks, function(x)metaData(x)$file[1]);
cancer <- grepl(pattern="/tumor/", x=filenames);
classes <- factor(ifelse(cancer, "cancer", "control"),
                  levels=c("cancer", "control"));

## 2. filter peaks which occur less across all samples
peaks <- filterPeaks(peaks, minFrequency=1);

## 3. export MassPeaks objects as matrix
training <- intensityMatrix(peaks);

## 'training' and 'classes' could now used by any statistical tool e.g. sda
