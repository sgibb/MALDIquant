.colMedians <- function(x, na.rm=FALSE) {
  stopifnot(is.matrix(x), is.logical(na.rm))
  .Call(C_colMedians, x, na.rm)
}

#' .colMaxs
#'
#' Similar to apply(x, 2, max).
#'
#' @param x matrix/data.frame
#' @return double
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.colMaxs <- function(x) {
  x[max.col(t(x), ties.method="first") + 0L:(ncol(x) - 1L) * nrow(x)]
}

#' .colCors
#'
#' Calculate the correlation for two matrices columnwise.
#'
#' @param x matrix/data.frame
#' @return double
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.colCors <- function(x, y, na.rm=FALSE) {
  stopifnot(is.matrix(x) && is.matrix(y))
  stopifnot(all(dim(x) == dim(y)))

  if (na.rm) {
    isNA <- is.na(x) | is.na(y)
    x[isNA] <- NA_real_
    y[isNA] <- NA_real_
  }

  cmX <- colMeans(x, na.rm=na.rm)
  cmY <- colMeans(y, na.rm=na.rm)

  (colMeans(x * y, na.rm=na.rm) - (cmX * cmY)) /
    (sqrt(colMeans(x * x, na.rm=na.rm) - cmX * cmX) *
     sqrt(colMeans(y * y, na.rm=na.rm) - cmY * cmY))
}
