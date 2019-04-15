## basic class for all spectra based information

# AbstractMassObject (VIRTUAL, mass/intensity = "numeric"/"matter_vec", metaData="list/matter")
# ├── AbstractMassSpectrum (VIRTUAL, just for method implementation)
# │   ├── MassSpectrum (mass/intensity = "numeric"/"double", metaData="list")
# │   ├── MassSpectrumOnDisk (mass/intensity = "OnDiskVector", metaData="list")
# └── MassPeaks (mass/intensity = "numeric"/"double", metaData="list")


setClass("OnDiskVector",
         slots=list(
                path="character",
                mpath="character",
                modification="integer",
                n="numeric",
                offset="numeric",
                size="integer"
         ),
         prototype=list(
                path=character(),
                mpath=character(),
                modification=0L,
                n=numeric(),
                offset=numeric(),
                size=integer()
         )
)


## Set a class union to extend slots 
#  type to matter Objects
setClassUnion("NumericOrOnDisk", c("numeric", "OnDiskVector"))

setClass("AbstractMassObject",
         slots=list(mass="NumericOrOnDisk",  
                    intensity="NumericOrOnDisk", 
                    metaData="list"),
         prototype=list(mass=numeric(), 
                        intensity=numeric(),
                        metaData=list()),
         contains="VIRTUAL")

## represnt abstract spectrum
setClass("AbstractMassSpectrum",
         contains="AbstractMassObject")

## represent a spectrum
setClass("MassSpectrum",
         contains="AbstractMassSpectrum")

## represent an On-disk spectrum 
setClass("MassSpectrumOnDisk",
         contains="AbstractMassSpectrum")

## represent a peak list from a single spectrum
setClass("MassPeaks",
         slots=list(snr="numeric"),
         prototype=list(snr=numeric()),
         contains="AbstractMassObject")




