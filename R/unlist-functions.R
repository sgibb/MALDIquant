## .unlist
##  wrapper for unlist
##
## params:
##  x: an R object
##
## returns:
##  see also ?unlist
##
.unlist <- function(x) {
  unlist(x, recursive=FALSE, use.names=FALSE)
}
