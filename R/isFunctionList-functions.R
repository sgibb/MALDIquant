.isFunctionList <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }

  length(x) && all(.unlist(vapply(x, is.function, logical(1L))))
}

.stopIfNotIsFunctionList <- function(x) {
  if (!.isFunctionList(x)) {
    parentCall <- deparse(sys.call(-1L))
    stop(parentCall, " : ", sQuote(deparse(substitute(x))),
         " is no list of functions!", call.=FALSE)
  }
  TRUE
}
