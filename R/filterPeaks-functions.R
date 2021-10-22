## filterPeaks
##  filter peaks which are not frequently represented in different samples
##
## params:
##  l: list of MassPeaks objects
##  minFrequency: double, minimal frequency of a peak to be not removed
##  minNumber: double, minimal (absolute) number of peaks to be not removed
##  labels: factor, labelwise filtering
##  mergeWhitelists: logical, apply whitelists local (FALSE) or global (TRUE)
##
## returns:
##  a list of adjusted MassPeaks objects
##
filterPeaks <- function(l, minFrequency, minNumber, labels,
                        mergeWhitelists=FALSE) {

  ## test arguments
  .stopIfNotIsMassPeaksList(l)

  ## labels
  if (missing(labels)) {
    labels <- rep.int(0L, length(l))
  }

  ## drop unused levels and turn argument into factor
  if (is.factor(labels)) {
    labels <- droplevels(labels)
  } else {
    ## preserve order in labels
    labels <- factor(labels, levels=unique(labels))
  }

  if (missing(minFrequency)) {
    minFrequency <- NA
  }

  if (missing(minNumber)) {
    minNumber <- NA
  }

  ll <- levels(labels)
  nl <- length(ll)

  if (length(labels) != length(l)) {
    stop("For each item in ", sQuote("l"), " there must be a label in ",
         sQuote("labels"), "!")
  }

  ## recycle arguments if needed
  minFrequency <- rep_len(minFrequency, nl)
  minNumber <- rep_len(minNumber, nl)
  mergeWhitelists <- mergeWhitelists[1]

  ## use peaks occurrence list
  o <- .as.occurrence.list(l)

 # group indices by labels
  idx <- lapply(ll, function(x)which(labels == x))

  ## collect whitelists
  
  w <- matrix(FALSE, nrow = nl, ncol = length(o$masses))
  
  for (i in seq_along(idx)) {
    
    wl <- .whitelist.list(o, idx[[i]], minFrequency=minFrequency[i], minNumber=minNumber[i])
    
    if (sum(wl)) {
      if (mergeWhitelists) {
        ## R uses columnwise recycling
        w <- t(t(w) | wl)
      } else {
        ## R uses columnwise recycling
        w[i, ] <- t(t(w[i, , drop=FALSE]) | wl)
      }
    } else {
      warning("Empty peak whitelist for level ", sQuote(ll[i]), ".")
    }
  }
  
  ## turn matrix back into MassPeaks objects
  
  for (i in seq_along(idx)) {
    for (j in idx[[i]]) {
      l[[j]]@mass <- l[[j]]@mass[w[i, o$i[o$r == j]]]
      l[[j]]@intensity <- l[[j]]@intensity[w[i, o$i[o$r == j]]]
      l[[j]]@snr <- l[[j]]@snr[w[i, o$i[o$r == j]]]
    }
  }
  
  l
}

## .whitelist
##  helper function to create whitelists for filtering
##
## params:
##  m: matrix
##  rows: index of rows which should filtered
##  minFrequency: double, minimal frequency of a peak to be not removed
##  minNumber: double, minimal (absolute) number of peaks to be not removed
##
## returns:
##  a logical vector representing the whitelist
##
.whitelist <- function(m, rows, minFrequency, minNumber) {

  ## test arguments
  if (is.na(minFrequency) && is.na(minNumber)) {
    stop(sQuote(minFrequency), " or ", sQuote(minNumber),
         " has to be a meaningful number!")
  }

  if (!is.na(minFrequency) && minFrequency < 0L) {
    minFrequency <- 0L
    warning(sQuote("minFrequency"),
            " < 0 does not make sense! Using 0 instead.")
  }

  if (!is.na(minNumber) && minNumber < 0L) {
    minNumber <- 0L
    warning(sQuote("minNumber"), " < 0 does not make sense! Using 0 instead.")
  }

  if (!is.na(minFrequency) && !is.na(minNumber)) {
    warning(sQuote("minFrequency"), " and ", sQuote("minNumber"),
            " arguments are given. Choosing the higher one.")
  }

  ## calculate minimal number of peaks
  minPeakNumber <- max(minFrequency * length(rows), minNumber, na.rm=TRUE)

  colSums(m[rows, , drop=FALSE]) >= minPeakNumber
}


.whitelist.list <- function(l, rows, minFrequency, minNumber) {
  
  ## test arguments
  if (is.na(minFrequency) && is.na(minNumber)) {
    stop(sQuote(minFrequency), " or ", sQuote(minNumber),
         " has to be a meaningful number!")
  }
  
  if (!is.na(minFrequency) && minFrequency < 0L) {
    minFrequency <- 0L
    warning(sQuote("minFrequency"),
            " < 0 does not make sense! Using 0 instead.")
  }
  
  if (!is.na(minNumber) && minNumber < 0L) {
    minNumber <- 0L
    warning(sQuote("minNumber"), " < 0 does not make sense! Using 0 instead.")
  }
  
  if (!is.na(minFrequency) && !is.na(minNumber)) {
    warning(sQuote("minFrequency"), " and ", sQuote("minNumber"),
            " arguments are given. Choosing the higher one.")
  }
  
  ## calculate minimal number of peaks
  
  keep.rows <- (l$r %in% rows)
  l$r <- l$r[keep.rows]
  l$i <- l$i[keep.rows]
  
  minPeakNumber <- max(minFrequency * length(unique(l$r)), minNumber, na.rm=TRUE)
  
  return(
    sapply(seq_along(l$masses), function(m) {
    sum(l$i == m) >= minPeakNumber
  }))
  
}