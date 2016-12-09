## .lapply
##  wrapper for lapply with different defaults
##
## params:
##  see ?lapply
##
## returns:
##  see ?lapply
##
.lapply <- function(X, FUN, ..., mc.cores=1L) {
  parallel::mclapply(X, FUN, ..., mc.cores=mc.cores)
}
