## .which.closest
##  a relaxed version of which (returns the nearest index)
##
## params:
##  x: numeric key value to look for
##  vec: numeric, has to be sorted
##  tolerance: accepted tolerance; Inf to choose the closest (old behaviour)
##  nomatch: if tolerance !Inf and the difference is larger than tolerance
##  return nomatch.
##
## returns:
##  a vector of indices
##
.which.closest <- function(x, vec, tolerance=Inf, nomatch=NA_integer_) {

  ## find left interval
  lIdx <- findInterval(x, vec, rightmost.closed=FALSE, all.inside=TRUE)
  rIdx <- lIdx + 1L

  ## calculate differences for left and right
  lDiff <- abs(vec[lIdx] - x)
  rDiff <- abs(vec[rIdx] - x)

  if (any(is.finite(tolerance))) {
    lIdx[lDiff > tolerance] <- nomatch
    rIdx[rDiff > tolerance] <- nomatch
  }

  d <- which(lDiff != pmin.int(lDiff, rDiff))
  lIdx[d] <- rIdx[d]
  lIdx
}
