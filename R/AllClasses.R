#' @title OnDiskVector class
#'
#' @name OnDiskVector
#'
#' @aliases OnDiskVector-class
#'
#' @description
#'
#' [OnDiskVector-class] objects support the storage of numeric data on-disk. The
#' data are just loaded into memory when they have to be processed.
#'
#' @slot path file path
#' @slot mpath file path to the modification counter file
#' @slot modification counter, to detect modification after `odv2 <- odv`
#' @slot n length of the vector
#' @slot offset offset of the data in the file
#' @slot size size of one vector element in the file
#'
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#'
#' @noRd
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

setClassUnion("NumericOrOnDiskVector", c("numeric", "OnDiskVector"))

## basic class for all spectra based information
setClass("AbstractMassObject",
         slots=list(mass="NumericOrOnDiskVector",
                    intensity="NumericOrOnDiskVector",
                    metaData="list"),
         contains="VIRTUAL")

## represent a spectrum
setClass("MassSpectrum",
         contains="AbstractMassObject")

setClass("MassSpectrumOnDisk",
         contains="AbstractMassObject")

setClassUnion("MassSpectra", c("MassSpectrum", "MassSpectrumOnDisk"))

## represent a peak list from a single spectrum
setClass("MassPeaks",
         slots=list(snr="numeric"),
         prototype=list(snr=numeric()),
         contains="AbstractMassObject")
