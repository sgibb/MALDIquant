#' Relaxed Value Matching
#'
#' \code{match.closest} returns a vector of the positions of (first) matches
#' its first arguments in its second. In contrast to the similar
#' \code{\link{match}} it just accept \code{numeric} arguments but
#' has an additional \code{tolerance} argument that allows relaxed
#' matching.
#'
#' @param x \code{numeric}, the values to be matched.
#' @param table \code{numeric}, the values to be matched against. In contrast to
#' \code{\link{match}} \code{table} has to be sorted in increasing order.
#' @param tolerance \code{numeric}, accepted tolerance. Use \code{Inf} to match without
#' restrictions. Could be of length one or the same length as \code{table}.
#' @param nomatch \code{numeric}, if the difference
#' between the value in \code{x} and \code{table} is larger than
#' \code{tolerance} \code{nomatch} is returned. Has to be of length one.
#'
#' @return An \code{integer} vector of the same length as \code{x} giving the
#' closest position in \code{table} of the first match or \code{nomatch} if
#' there is no match.
#'
#' @seealso \code{\link{match}}
#' @aliases match.closest
#' @export
#' @examples
#' library("MALDIquant")
#' match.closest(c(1.1, 1.4, 9.8), 1:10)
#' # [1]  1  1 10
#' match.closest(c(1.1, 1.4, 9.8), 1:10, tolerance=0.25)
#' # [1]  1 NA 10
#' match.closest(c(1.1, 1.4, 9.8), 1:10, tolerance=0.25, nomatch=0)
#' # [1]  1  0 10
#'
#' ## this function is most useful if you want to subset an intensityMatrix
#' ## by a few (reference) peaks
#'
#' ## create an example intensityMatrix
#' im <- matrix(1:10, nrow=2, dimnames=list(NULL, 1:5))
#' attr(im, "mass") <- 1:5
#' im
#' #      1 2 3 4  5
#' # [1,] 1 3 5 7  9
#' # [2,] 2 4 6 8 10
#' # attr(,"mass")
#' # [1] 1 2 3 4 5
#'
#' ## reference peaks
#' ref <- c(2.2, 4.8)
#'
#' im[, match.closest(ref, attr(im, "mass"), tolerance=0.25, nomatch=0)]
#' #      2  5
#' # [1,] 3  9
#' # [2,] 4 10
#'
match.closest <- function(x, table, tolerance=Inf, nomatch=NA_integer_) {
  lIdx <- findInterval(x, table, rightmost.closed=FALSE, all.inside=TRUE)
  rIdx <- lIdx + 1L

  lDiff <- abs(table[lIdx] - x)
  rDiff <- abs(table[rIdx] - x)

  d <- which(lDiff >= rDiff)

  lIdx[d] <- rIdx[d]

  if (any(is.finite(tolerance))) {
    if (any(tolerance < 0L)) {
      warning(sQuote("tolerance"), " < 0 is meaningless. Set to zero.")
      tolerance[tolerance < 0L] <- 0L
    }

    if (length(nomatch) != 1L) {
      stop("Length of ", sQuote("nomatch"), " has to be one.")
    }

    tolerance <- rep_len(tolerance, length(table))

    lDiff[d] <- rDiff[d]
    lIdx[lDiff > tolerance[lIdx]] <- nomatch
  }

  lIdx
}
