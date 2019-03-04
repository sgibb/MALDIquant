## MassSpectrum

## createMassSpectrum
##  default constructor: MassSpectrum class
##
## params:
##  mass: vector, spectrum mass
##  intensity: vector, spectrum intensities
##  metaData: list, metadata
##
## returns:
##  a MassSpectrum object
##
createMassSpectrum <- function(mass, intensity, metaData=list()) {
  object <- new(Class="MassSpectrum", mass=mass, intensity=intensity, metaData=metaData)
  .reorder(object)
}


## MassSpectrumOnDisk

## createMassSpectrumOnDisk
##  default constructor: MassSpectrumOnDisk class
##
## params:
##  mass: matter_vec, spectrum mass
##  intensity: matter_vec, spectrum intensities
##  metaData: list, metadata
##
## returns:
##  a MassSpectrumOnDisk object
##
createMassSpectrumOnDisk <- function(mass, intensity, metaData=list()) {
       
       onDiskMass <- matter::matter_vec(data=mass, datamode="double", filemode="rb+", 
                                        paths=tempfile("spectrum", fileext=".mass")
       onDiskIntensity <- matter::matter_vec(data=intensity, datamode="double", filemode="rb+", 
                                             paths=tempfile("spectrum", fileext=".intensity")
       
       object <- new(Class="MassSpectrumOnDisk", 
                     mass=onDiskMass, 
                     intensity=onDiskIntensity, 
                     metaData=metaData,
                     path=setNames(
                            c(onDiskMass@paths, onDiskIntensity@paths), 
                            c("mass", "intensity")
                            ))
       
       
       .reorder(object)
       
}


## end of MassSpectrum

## MassPeaks

## createMassPeaks
##  default constructor: MassPeaks class
##
## params:
##  mass: vector, peaks mass
##  intensity: vector, peaks intensities
##  snr: vector, peaks snr
##  metaData: list, metadata
##
## returns:
##  a MassPeaks object
##
createMassPeaks <- function(mass, intensity,
                            snr=rep.int(NA_real_, length(intensity)),
                            metaData=list()) {
  object <- new(Class="MassPeaks", mass=mass, intensity=intensity, snr=snr,
                metaData=metaData)
  .reorder(object)
}

## end of MassPeaks
