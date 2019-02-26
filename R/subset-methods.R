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

## In the case of MassSpectrumOnDisk objects subsetting the original on-disk data in not possible. In
## this situation a new subsetted MassSpectrum object is returned

setMethod(f="[",
          signature=signature(x="MassSpectrumOnDisk", i="numeric", j="missing"),
          definition=function(x, i, j, ..., drop=TRUE) {
                 
                 warning("For the supplied MassSpectrumOnDisk object, subsetting the original on-disk data ",
                         "is not possible. In this situation a new subsetted MassSpectrum object is returned.\n")
                 
                
                 
                 createMassSpectrum(mass=mass(x)[i], 
                                    intensity=intensity(x)[i], 
                                    metaData=x@metaData)
                 
                 
          })

## MassSpectrumOnDisk
setMethod(f="[",
          signature=signature(x="MassSpectrumOnDisk", i="logical", j="missing"),
          definition=function(x, i, j, ..., drop=TRUE) {
                 
                 warning("For the supplied MassSpectrumOnDisk object, subsetting the original on-disk data ",
                         "is not possible. In this situation a new subsetted MassSpectrum object is returned.\n")
                 
                 ## seems to be faster than evaluating the logical expression twice
                 i <- which(i)
                
                 createMassSpectrum(mass=mass(x)[i], 
                                    intensity=intensity(x)[i], 
                                    metaData=x@metaData)
                 
                 
          })

## MassPeaks
setMethod(f="[",
          signature=signature(x="MassPeaks", i="numeric", j="missing"),
          definition=function(x, i, j, ..., drop=TRUE) {

  x@mass <- x@mass[i]
  x@intensity <- x@intensity[i]
  x@snr <- x@snr[i]

  x
})

## MassPeaks
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
