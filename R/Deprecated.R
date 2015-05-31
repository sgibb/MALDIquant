## deprecated since MALDIquant 1.11.12
plotImsSlice <- function(x, range=c(0, Inf),
                         sub=paste0("m/z: ", range[1L], "-", range[2L], ""),
                         removeEmptyRows=TRUE,
                         removeEmptyCols=TRUE,
                         colRamp=colorRamp(c("black", "blue", "green",
                                             "yellow", "red")),
                         interpolate=FALSE, ...) {
  .deprecatedFunction("1.11.12", new="plotMsiSlice")

  .prepareImsSlice <- function(x, range) {

    .stopIfNotIsMassObjectList(x)

    ## display only mass in range
    suppressWarnings(x <- trim(x, range=range))

    ## find x and y positions
    pos <- lapply(x, function(y)metaData(y)$imaging$pos)
    pos <- do.call(rbind, pos)

    if (is.null(pos)) {
      stop("The spectra do not have any position information.")
    }

    ## max x/y to build image matrix
    nc <- max(pos[, "x"])
    nr <- max(pos[, "y"])

    ## init matrix
    m <- matrix(NA, nrow=nr, ncol=nc)

    ## fill matrix with intensity values
    for (i in seq(along=x)) {
      m[pos[i, "y"], pos[i, "x"]] <- sum(intensity(x[[i]]), na.rm=TRUE)
    }

    ## scale matrix (better contrast)
    m/max(m, na.rm=TRUE)
  }

  m <- .prepareImsSlice(x = x, range = range)

  if (removeEmptyRows) {
    kr <- rowSums(is.na(m)) != ncol(m)
    m <- m[kr, , drop=FALSE]
  }
  if (removeEmptyCols) {
    kc <- colSums(is.na(m)) != nrow(m)
    m <- m[, kc, drop=FALSE]
  }

  nr <- nrow(m)
  nc <- ncol(m)

  ## create color raster
  isNotNA <- which(!is.na(m))
  m[isNotNA] <- rgb(colRamp(m[isNotNA]), maxColorValue=255L)

  ## prepare plot area
  plot(NA, type="n", xlim=c(1L, nc), ylim=c(1L, nr), axes=FALSE, asp=1L,
       xlab="", ylab="", sub=sub, ...)
  ## plot image
  rasterImage(as.raster(m), xleft=1L, xright=nc, ybottom=1L, ytop=nr,
              interpolate=interpolate)
}
