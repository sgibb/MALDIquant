## deprecated since MALDIquant 1.15.2
## We have to keep it until May 2017 because the current MSnbase 2.0.x depends
## on this internal function.
.which.closest <- function(x, vec) {
  match.closest(x, vec, tolerance=Inf, nomatch=NA_integer_)
}
