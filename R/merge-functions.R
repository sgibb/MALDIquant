## mergeMassPeaks
##  merge MassPeaks objects
##
## params:
##  l: list of MassPeaks objects
##  labels: factor, labels for samples
##  fun: merge function
##
## returns:
##  a new MassPeaks object or a list of new MassPeaks objects
##
mergeMassPeaks <- function(l, labels, method=c("mean", "median", "sum"),
                           ignore.na=TRUE, ...) {

  ## test arguments
  .stopIfNotIsMassPeaksList(l)

  method <- match.arg(method)

  fun <- switch(method,
              "mean" = {
                .colMeans
              },
              "median" = {
                .colMedians
              },
              "sum" = {
                colSums
              }
  )

  .doByLabels(l=l, labels=labels, FUN=.mergeMassPeaks, fun=fun,
              ignore.na=ignore.na, ...)
}

## .mergeMassPeaks
##  merge MassPeaks objects
##
## params:
##  l: list of MassPeaks objects
##  fun: merge function
##
## returns:
##  a new MassPeaks object
##
.mergeMassPeaks <- function(l, fun=.colMeans, ignore.na=TRUE) {

  fun <- match.fun(fun)

  ## create a matrix which could merged
  m <- .as.matrix.MassObjectList(l)
  
  mass <- attr(m, "mass")
  
  if (!ignore.na) {
    m[m == 0] <- .Machine$double.xmin
  }

  ## avoid named intensity/snr slot
  colnames(m) <- NULL

  ## merge intensities
  intensity <- fun(m, na.rm=TRUE)

  ## merge snr
  ij <- lapply(1:nrow(m), function(r) {
    cbind(r, which(m[r, ] > .Machine$double.xmin))
  })
  ij <- Reduce(rbind, ij)
  
  if (ignore.na) {
    m <- sparseMatrixNA(i=ij[, 1], j=ij[, 2], unlist(lapply(l, function(z) z@snr)), 
                        dims=dim(m), keep.zeros=TRUE)
  } else {
    m <- sparseMatrix(i=ij[, 1], j=ij[, 2], x=unlist(lapply(l, function(z) z@snr)),
                      dims=dim(m))
    m[m == 0] <- .Machine$double.xmin
  }

  snr <- fun(m, na.rm = TRUE)

  ## merge metaData
  metaData <- .mergeMetaData(lapply(l, function(x)x@metaData))

  createMassPeaks(mass=mass, intensity=as.numeric(intensity), snr=as.numeric(snr), metaData=metaData)
}

## merge different metaData by equal list names
##
## params
##  m: list of metaData
##
## returns:
##  merged list
##
.mergeMetaData <- function(m) {

  .flat <- function(x)unname(unlist(x))

  nm <- names(m[[1L]])
  names(nm) <- nm
  lapply(nm, function(n) {
    cur <- m[[1L]][[n]]
    all <- lapply(m, "[[", n)
    len <- lengths(all)

    if (!all(length(cur) == len) ||
        !all(.flat(cur) == .flat(all))) {
      if (!is.list(cur)) {
        all <- unlist(all)
      }
      return(unname(all))
    } else {
      return(cur)
    }
  })
}
