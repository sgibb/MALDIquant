## AbstractMassObject
setMethod(f="[",
          signature=signature(x="AbstractMassObject", i="numeric", j="missing"),
          definition=function(x, i, j, ..., drop=TRUE) {

  x@mass <- x@mass[i]
  x@intensity <- x@intensity[i]
  x
})

## AbstractMassObject
setMethod(f="[",
          signature=signature(x="AbstractMassObject", i="logical", j="missing"),
          definition=function(x, i, j, ..., drop=TRUE) {

  ## seems to be faster than evaluating the logical expression twice
  i <- which(i)
  x@mass <- x@mass[i]
  x@intensity <- x@intensity[i]
  x
})

## MassSpectrumOnDisk
setMethod(f="[",
          signature=signature(x="MassSpectrumOnDisk", i="numeric", j="missing"),
          definition=function(x, i, j, ..., drop=TRUE) {
  createMassSpectrum(mass=x@mass[i], intensity=x@intensity[i],
                     metaData=x@metaData)
})

## MassSpectrumOnDisk
setMethod(f="[",
          signature=signature(x="MassSpectrumOnDisk", i="logical", j="missing"),
          definition=function(x, i, j, ..., drop=TRUE) {

  ## seems to be faster than evaluating the logical expression twice
  i <- which(i)
  createMassSpectrum(mass=x@mass[i], intensity=x@intensity[i],
                     metaData=x@metaData)
})

setMethod(f="[",
          signature=signature(x="MassPeaks", i="numeric", j="missing"),
          definition=function(x, i, j, ..., drop=TRUE) {

  x@mass <- x@mass[i]
  x@intensity <- x@intensity[i]
  x@snr <- x@snr[i]

  x
})

## AbstractMassObject
setMethod(f="[",
          signature=signature(x="MassPeaks", i="logical", j="missing"),
          definition=function(x, i, j, ..., drop=TRUE) {

  ## seems to be faster than evaluating the logical expression twice
  i <- which(i)
  x@mass <- x@mass[i]
  x@intensity <- x@intensity[i]
  x@snr <- x@snr[i]

  x
})
