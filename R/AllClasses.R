## basic class for all spectra based information

## Set a class union to extend slots 
#  type to matter Objects
setClassUnion("NumericOrOnDisk", c("numeric", "matter_vec"))


setClass("AbstractMassObject",
         slots=list(mass="NumericOrOnDisk",  
                    intensity="NumericOrOnDisk", 
                    metaData="list"),
         prototype=list(mass=numeric(), 
                        intensity=numeric(),
                        metaData=list()),
         contains="VIRTUAL")

## represent a spectrum
setClass("MassSpectrum",
         contains="AbstractMassObject")

## represent an On-disk spectrum 
setClass("MassSpectrumOnDisk",
         slots = list(path = "character"),
         prototype = list(path = character()),
         contains="AbstractMassObject")

## represent a peak list from a single spectrum
setClass("MassPeaks",
         slots=list(snr="numeric"),
         prototype=list(snr=numeric()),
         contains="AbstractMassObject")
