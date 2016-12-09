## .prepareShowGroupName
##  prepares a group name for show (appends numbers to name if needed)
##
## params:
##  x: vector
##  name: group name
##
## returns:
##
.prepareShowGroupName <- function(x, name) {
  if (!is.null(x)) {
    n <- length(x)

    if (n > 1L) {
      name <- paste0(name, seq_len(n))
    }

  } else {
    name <- NULL
  }

  name
}
