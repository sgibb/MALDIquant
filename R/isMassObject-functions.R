.isMassObject <- function(x) {
  inherits(x=x, what="AbstractMassObject")
}

isMassSpectrum <- function(x) {
  is(object=x, class2="MassSpectrum")
}

isMassSpectrumOnDisk <- function(x) {
  is(object=x, class2="MassSpectrumOnDisk")
}

isMassPeaks <- function(x) {
  is(object=x, class2="MassPeaks")
}
