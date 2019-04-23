## convert MassPeaks into MassSpectrum objects
##
## returns:
##  a MassSpectrum object

setAs(from="MassPeaks", to="MassSpectrum",
      function (from)createMassSpectrum(mass=from@mass,
                                        intensity=from@intensity,
                                        metaData=from@metaData))


## convert MassSpectrumOnDisk into MassSpectrum objects - from disk to memory       
##
## returns:
##  a MassSpectrum object

setAs(from="MassSpectrumOnDisk", to="MassSpectrum",
      function (from)createMassSpectrum(mass=mass(from),
                                        intensity=intensity(from),
                                        metaData=from@metaData))
