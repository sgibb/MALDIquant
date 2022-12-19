## local maxima function called by .findLocalMaxima
##
## This function looks for local maxima in a numeric vector.
##
## params:
##  y: double, intensity
##  halfWindowSize: numeric, half window size.
##
## returns:
##  logical vector of local maxima
##

.localMaxima <- function(y, halfWindowSize=1L) {
  y <- c(rep.int(0L, halfWindowSize), y, rep.int(0L, halfWindowSize))
  i <- (halfWindowSize + 1L):(length(y) - halfWindowSize)
  .Call(C_localMaxima, y, halfWindowSize)[i]
}
