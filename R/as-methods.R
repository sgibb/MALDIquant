## convert MassPeaks into MassSpectrum objects
##
## returns:
##  a MassSpectrum object

setAs(from="MassPeaks", to="MassSpectrum",
      function (from)createMassSpectrum(mass=mass(from),
                                        intensity=intensity(from),
                                        metaData=metaData(from)))
