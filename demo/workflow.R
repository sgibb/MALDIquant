## This is a MALDIquant example file. It is released into public domain with the
## right to use it for any purpose but without any warranty.


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
