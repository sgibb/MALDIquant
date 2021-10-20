.colMedians <- function(x, na.rm=FALSE) {
  stopifnot(is.logical(na.rm))
  # stopifnot(is.matrix(x), is.logical(na.rm))
  if (is(x, 'sparseMatrix')) {
    ret <- .Call("_MALDIquant_colMediansArma", x, na.rm) 
  } else {
    ret <- .Call("C_colMedians", x, na.rm)
  }
  return(as.numeric(ret))
}

.colMeans <- function(x, na.rm=FALSE) {
  stopifnot(is.logical(na.rm))
  if (is(x, 'sparseMatrix')) {
    ret <- .Call("_MALDIquant_colMeansArma", x, na.rm)
  } else {
    ret <- colMeans(x, na.rm=na.rm)
  }
  return(as.numeric(ret))
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
  if (is(x, 'sparseMatrix')) {
    apply(x, 2, max)
  } else {
    x[max.col(t(x), ties.method="first") + 0L:(ncol(x) - 1L) * nrow(x)]
  }
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
  # stopifnot(is.matrix(x) && is.matrix(y))
  stopifnot(all(dim(x) == dim(y)))

  if (na.rm) {
    if (is(x, "sparseMatrix") & is.matrix(y)) {
      isMissing <- as.matrix((x == 0) | is.na(y))
      x[isMissing] <- 0
      y[isMissing] <- NA_real_
    } else if (is.matrix(x) & is(y, "sparseMatrix")) {
      isMissing <- as.matrix(is.na(x) | (y == 0))
      x[isMissing] <- NA_real_
      y[isMissing] <- 0
    } else if (is(x, "sparseMatrix") & is(y, "sparseMatrix")) {
      isMissing <- (x == 0) | (y == 0)
      x[isMissing] <- 0
      y[isMissing] <- 0
    } else {
      isMissing <- is.na(x) | is.na(y)
      x[isMissing] <- NA_real_
      y[isMissing] <- NA_real_
    }
  }

  cmX <- .colMeans(x, na.rm=na.rm)
  cmY <- .colMeans(y, na.rm=na.rm)

  (.colMeans(x * y, na.rm=na.rm) - (cmX * cmY)) /
    (sqrt(.colMeans(x * x, na.rm=na.rm) - cmX * cmX) *
     sqrt(.colMeans(y * y, na.rm=na.rm) - cmY * cmY))
}
