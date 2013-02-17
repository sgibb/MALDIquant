## This is a MALDIquant example file. It is released into public domain with the
## right to use it for any purpose but without any warranty.


## workflow demo


## load necessary libraries
library("MALDIquant")

## load example spectra
data("fiedler2009subset", package="MALDIquant")

## sqrt transform (for variance stabilization)
spectra <- transformIntensity(fiedler2009subset, sqrt)

## simple 5 point moving average for smoothing spectra
## (maybe you have to increase halfWindowSize if your data are very noisy)
spectra <- transformIntensity(spectra, movingAverage, halfWindowSize=2)

## remove baseline
## (maybe you have to adjust iterations to your spectra; high resolution
## spectra need a much lower iteration number (halfWindowSize, for some other
## baseline estimation algorithms)
## see ?removeBaseline, ?estimateBaseline
spectra <- removeBaseline(spectra, method="SNIP", iterations=100)

## run peak detection
## (maybe you need to adjust halfWindowSize [decreasing it for high resolution
## spectra] and SNR [a higher value increase the True-Positive-Rate but decrease
## sensitivity])
## see ?detectPeaks, ?estimateNoise
peaks <- detectPeaks(spectra, method="MAD", halfWindowSize=20, SNR=2)

## align spectra by warping
## 1. create reference peaks (could be done automatically by
##  determineWarpingFunctions)
## 2. calculate individual warping functions
## 3. warp each MassPeaks object
## (maybe you have to adjust the tolerance argument [increasing for low
## resolution spectra with a high mass error, decreasing for high resolution
## spectra with a small mass error])
## see ?referencePeaks,?determineWarpingFunctions
refPeaks <- referencePeaks(peaks)
warpingFunctions <- determineWarpingFunctions(peaks, reference=refPeaks,
                                              tolerance=0.002)
peaks <- warpMassPeaks(peaks, warpingFunctions)

## bin peaks
peaks <- binPeaks(peaks)

## merge technical replicates
## 1. create factors for correct assignment
nTechRep <- 2
nBiologicalSamples <- length(peaks)/nTechRep
samples <- factor(rep(1:nBiologicalSamples, each=nTechRep),
                  levels=1:nBiologicalSamples)

## 2. filter peaks which occur only in one of the replicates
peaks <- filterPeaks(peaks, labels=samples, minFrequency=1)

## 3. merge technical replicates
peaks <- mergeMassPeaks(peaks, labels=samples, fun=mean)

## prepare for statistical analysis
## 1. get cancer/control indices
filenames <- sapply(peaks, function(x)metaData(x)$file[1])
cancer <- grepl(pattern="/tumor/", x=filenames)
classes <- factor(ifelse(cancer, "cancer", "control"),
                  levels=c("cancer", "control"))

## 2. filter peaks which occur less across all samples
peaks <- filterPeaks(peaks, minFrequency=1)

## 3. export MassPeaks objects as matrix
training <- intensityMatrix(peaks)

## 'training' and 'classes' could now used by any statistical tool e.g. sda
