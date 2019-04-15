## .memoryUsageStr.object_size
##  pretty string of memory usage
##
## params:
##  x: an R object for inMemory = TRUE and a file path otherwise. 
##  inMemory: logical, TRUE for in memory objects and FALSE for stored files on-disk.
##
## returns:
##  character
##
.memoryUsageStr <- function(x, inMemory=TRUE) {
       
  if(inMemory){
         os <- object.size(x)
  } else {
         if(!file.exists(x)) stop("Error in .memoryUsageStr; specified file deos not exist.")
         os <- file.info(x)$size
       }
   
  iec <- c("B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB")
  l <- trunc(log(os) / log(1024L))
  i <- pmin(l + 1L, 9L)

  paste(round(os / (1024L^l), digits=3L), iec[i])
}
