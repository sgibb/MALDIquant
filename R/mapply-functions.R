## .mapply
##  wrapper for mapply with different defaults
##
## params:
##  see ?mapply
##
## returns:
##  see ?mapply
##
.mapply <- function(FUN, ..., MoreArgs=NULL, SIMPLIFY=FALSE, USE.NAMES=FALSE,
                    mc.cores=1L) {
  parallel::mcmapply(FUN=FUN, ..., MoreArgs=MoreArgs, SIMPLIFY=SIMPLIFY,
                     USE.NAMES=USE.NAMES, mc.cores=mc.cores)
}
