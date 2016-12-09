## .stopIfNotIsValidHalfWindowSize
##  test for too small/large half window size
##
## params:
##  halfWindowSize: half window size
##  n: length of data
##
## returns:
##  TRUE if valid
##
.stopIfNotIsValidHalfWindowSize <- function(halfWindowSize, n) {
  parentCall <- deparse(sys.call(-1L))

  if (halfWindowSize < 1L) {
    stop(parentCall, " : ", sQuote("halfWindowSize"),
         " is too small!", call.=FALSE)
  }

  windowSize <- halfWindowSize * 2L + 1L

  if (windowSize > n) {
    stop(parentCall, " : ", sQuote("halfWindowSize"),
         " is too large!", call.=FALSE)
  }

  TRUE
}
