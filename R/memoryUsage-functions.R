## .memoryUsageStr.object_size
##  pretty string of memory usage
##
## params:
##  x: object_size
##
## returns:
##  character
##
.memoryUsageStr <- function(x) {
  os <- object.size(x)
  iec <- c("B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB")
  l <- trunc(log(os) / log(1024L))
  i <- pmin(l + 1L, 9L)

  paste(round(os / (1024L^l), digits=3L), iec[i])
}
