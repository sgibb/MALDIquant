## AbstractMassObject
.isMassObject <- function(x) {
  inherits(x=x, what="AbstractMassObject")
}

## MassSpectrum or MassSpectrumOnDisk
isMassSpectrum <- function(x) {
  (is(object=x, class2="MassSpectrum") || is(object=x, class2="MassSpectrumOnDisk"))
}

## MassPeaks
isMassPeaks <- function(x) {
  is(object=x, class2="MassPeaks")
}
