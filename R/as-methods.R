## convert MassPeaks into MassSpectrum objects
##
## returns:
##  a MassSpectrum object

setAs(from="MassPeaks", to="MassSpectrum",
      function (from)createMassSpectrum(mass=from@mass,
                                        intensity=from@intensity,
                                        metaData=from@metaData))
