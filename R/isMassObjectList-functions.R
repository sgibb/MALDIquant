.isMassObjectList <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }

  length(x) && all(unname(vapply(x, .isMassObject, logical(1L))))
}

.stopIfNotIsMassObjectList <- function(x) {
  if (!.isMassObjectList(x)) {
    parentCall <- deparse(sys.call(-1L))
    stop(parentCall, " : ", sQuote(deparse(substitute(x))),
         " is no list of MALDIquant::AbstractMassObject objects!", call.=FALSE)
  }
  TRUE
}

isMassSpectrumList <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }

  length(x) && all(unname(vapply(x, isMassSpectrum, logical(1L))))
}

.stopIfNotIsMassSpectrumList <- function(x) {
  if (!isMassSpectrumList(x)) {
    parentCall <- deparse(sys.call(-1L))
    stop(parentCall, " : ", sQuote(deparse(substitute(x))),
         " is no list of MALDIquant::MassSpectrum objects!", call.=FALSE)
  }
  TRUE
}

isMassPeaksList <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }

  length(x) && all(unname(vapply(x, isMassPeaks, logical(1L))))
}

.stopIfNotIsMassPeaksList <- function(x) {
  if (!isMassPeaksList(x)) {
    parentCall <- deparse(sys.call(-1L))
    stop(parentCall, " : ", sQuote(deparse(substitute(x))),
         " is no list of MALDIquant::MassPeaks objects!", call.=FALSE)
  }
  TRUE
}
