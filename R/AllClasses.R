## basic class for all spectra based information
setClass("AbstractMassObject",
         slots=list(mass="numeric", intensity="numeric",
                    metaData="list"),
         prototype=list(mass=numeric(), intensity=numeric(),
                        metaData=list()),
         contains="VIRTUAL")

## represent a spectrum
setClass("MassSpectrum",
         contains="AbstractMassObject")

## represent a peak list from a single spectrum
setClass("MassPeaks",
         slots=list(snr="numeric"),
         prototype=list(snr=numeric()),
         contains="AbstractMassObject")
