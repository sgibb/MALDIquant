## This is a MALDIquant example file. It is released into public domain with the
## right to use it for any purpose but without any warranty.


## workflow demo


## load necessary packages
## requires MALDIquant >= 1.9
library("MALDIquant")


## load example spectra
data("fiedler2009subset", package="MALDIquant")


## check raw data
## any empty spectra? (empty spectra are ignored in subsequent baseline
## correction/peak detection; you could find/remove them by calling
## findEmptyMassObjects/removeEmptyMassObjects)
## see ?isEmpty, ?findEmptyMassObjects, ?removeEmptyMassObjects
any(sapply(fiedler2009subset, isEmpty))
# FALSE

## any spectra with irregular mass values/intervals? (spectra with
## missing/filtered mass values/irregular mass intervals may compromise
## subsequent baseline correction and peak detection.)
any(!sapply(fiedler2009subset, isRegular))
# FALSE

## do length of spectra differ? (if they differ you have to adjust the
## corresponding halfWindowSize in subsequent baseline correction and peak
## detection.)
table(sapply(fiedler2009subset, length))
# 42388
#    16


## preprocessing
## sqrt transform (for variance stabilization)
spectra <- transformIntensity(fiedler2009subset, method="sqrt")


## 21 point Savitzky-Golay-Filter for smoothing spectra
## (maybe you have to adjust the halfWindowSize;
## you could use a simple moving average instead)
## see ?smoothIntensity
spectra <- smoothIntensity(spectra, method="SavitzkyGolay", halfWindowSize=10)


## remove baseline
## (maybe you have to adjust iterations to your spectra; high resolution
## spectra need a much lower iteration number (halfWindowSize, for some other
## baseline estimation algorithms)
## see ?removeBaseline, ?estimateBaseline
spectra <- removeBaseline(spectra, method="SNIP", iterations=100)


## calibrate (normalize) intensities (different calibration methods available)
## see ?calibrateIntensity
spectra <- calibrateIntensity(spectra, method="TIC")


## spectra alignment
## (the spectra alignment is peak based, maybe you need to adjust
## halfWindowSize, SNR, tolerance, warpingMethod)
## see ?alignSpectra
spectra <- alignSpectra(spectra,
                        halfWindowSize=20, SNR=2,
                        tolerance=0.002, warpingMethod="lowess")


## average technical replicates
## 1. create factors for correct assignment
## (e.g. sample name is stored in metaData(x)$sampleName; maybe you have to
## adjust the metaData or use your own ID table for your data here)
samples <- factor(sapply(spectra, function(x)metaData(x)$sampleName))

## 2. average technical replicates
## see ?averageMassSpectra
avgSpectra <- averageMassSpectra(spectra, labels=samples, method="mean")


## run peak detection
## (maybe you need to adjust halfWindowSize [decreasing it for high resolution
## spectra] and SNR [a higher value increase the True-Positive-Rate but decrease
## sensitivity])
## see ?detectPeaks, ?estimateNoise
peaks <- detectPeaks(avgSpectra, method="MAD", halfWindowSize=20, SNR=2)

## bin peaks
## (After alignment peak positions (mass) are similar but not identical. Binning
## is needed to make similar peak mass values identical.)
## see ?binPeaks
peaks <- binPeaks(peaks, tolerance=0.002)


## prepare for statistical analysis
## 1. get cancer/control indices
filenames <- sapply(peaks, function(x)metaData(x)$file[1])
cancer <- grepl(pattern="/tumor/", x=filenames)
classes <- factor(ifelse(cancer, "cancer", "control"),
                  levels=c("cancer", "control"))

## 2. export expression/training matrix
## (and fill missing peaks by interpolated values)
## see ?intensityMatrix
training <- intensityMatrix(peaks, avgSpectra)


## 'training' and 'classes' could now used by any statistical tool e.g. sda
