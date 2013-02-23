## Copyright 2012-2013 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This file is part of MALDIquant for R and related languages.
##
## MALDIquant is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## MALDIquant is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with MALDIquant. If not, see <http://www.gnu.org/licenses/>

## binPeaks
##  binning peaks by splitting at the largest gap
##  This is a wrapper function around .binPeaks which prepares the peak list
##  before and recreates a correct peak list after binning.
##
## params:
##  l: list of MassPeaks objects
##  method: character, grouper to used (strict: don't allow multiple peaks of
##          the same sample in the same bin, relaxed: allow them)
##  tolerance: double, maximal deviation of a peak position to be
##             considered as same peak
##
## returns:
##  a list of adjusted MassPeaks objects
##
binPeaks <- function(l, method=c("strict", "relaxed"), tolerance=0.002) {

  ## test arguments
  .stopIfNotIsMassPeaksList(l)

  method <- match.arg(method, c("strict", "relaxed"), several.ok=FALSE)

  ## fetch all mass
  mass <- unname(.unlist(lapply(l, function(x)x@mass)))

  ## fetch all intensities
  intensities <- .unlist(lapply(l, function(x)x@intensity))

  ## fetch all snr 
  snr <- .unlist(lapply(l, function(x)x@snr))

  ## store original mass sample number/id
  samples <- .unlist(lapply(1:length(l), function(x) {
    return(rep(x, length(l[[x]])))
  }))
  
  ## sort values by mass
  s <- sort(mass, method="quick", index.return=TRUE)
  
  mass <- s$x
  intensities <- intensities[s$ix]
  snr <- snr[s$ix]
  samples <- samples[s$ix]

  ## select grouper
  grouper <- switch(method,
            "strict"  = { 
              .grouperStrict
            },
            "relaxed" = {
              .grouperRelaxed
            },
            {
              stop("Unknown ", sQuote("method"), ".")
            }
  )
 
  ## binning
  mass <- .binPeaks(mass=mass, intensities=intensities, samples=samples,
                    tolerance=tolerance, grouper=grouper)

  ## resort mass (order could change if "relaxed" is used)
  if (method == "relaxed") {
    s <- sort(mass, method="quick", index.return=TRUE)
    mass <- s$x
    intensities <- intensities[s$ix]
    snr <- snr[s$ix]
    samples <- samples[s$ix]
  } 

  ## group mass/intensities/snr by sample ids
  lIdx <- tapply(X=1:length(mass), INDEX=samples, FUN=function(x) {
    return(x)
  })

  ## create adjusted peak list
  l <- mapply(FUN=function(p, i) {
    p@mass <- mass[i]
    p@intensity <- intensities[i]
    p@snr <- snr[i]
    return(p)
  }, p=l, i=lIdx)

  return(l)
}

## .binPeaks
##  binning peaks by splitting at the largest gap
##
## params:
##  mass: double, sorted mass
##  intensities: double, corresponding intensities
##  samples: double, corresponding sample id numbers
##  tolerance: double, maximal deviation of a peak position to be
##             considered as same peak
##  grouper: grouping function
##  ...: arguments passed to grouping function
##
## returns:
##  vector (double) of modified mass
##
.binPeaks <- function(mass, intensities, samples, tolerance,
            grouper=.grouperStrict, ...) {
  n <- length(mass)

  ## calculate difference
  d <- diff(mass)

  ## grouper function
  grouper <- match.fun(grouper)

  ## stack based implementation taken from 
  ## caMassClass 1.9 R/msc.peaks.clust.R written by 
  ## Jarek Tuszynski <jaroslaw.w.tuszynski@saic.com>
  ## it is a lot of faster than recursion

  ## store boundaries in a stack
  nBoundaries <- max(20, floor(3*log(n)))
  boundary <- vector("list", length=2)
  boundary$left <- double(nBoundaries)
  boundary$right <- double(nBoundaries)

  currentBoundary <- 1
  boundary$left[currentBoundary] <- 1
  boundary$right[currentBoundary] <- n

  ## workhorse loop
  while (currentBoundary > 0) {
    ## find largest gap
    left <- boundary$left[currentBoundary]
    right <- boundary$right[currentBoundary]
    currentBoundary <- currentBoundary-1
    gaps <- d[left:(right-1)]

    gapIdx <- which.max(gaps)+left-1

    ## left side
    l <- grouper(mass=mass[left:gapIdx],
                 intensities=intensities[left:gapIdx],
                 samples=samples[left:gapIdx],
                 tolerance=tolerance, ...)
    ## further splitting needed?
    if (is.na(l[1])) {
      currentBoundary <- currentBoundary+1
      boundary$left[currentBoundary] <- left
      boundary$right[currentBoundary] <- gapIdx
    } else {
      mass[left:gapIdx] <- l
    }
  
    ## right side
    r <- grouper(mass=mass[(gapIdx+1):right],
                 intensities=intensities[(gapIdx+1):right],
                 samples=samples[(gapIdx+1):right],
                 tolerance=tolerance, ...)
    ## further splitting needed?
    if (is.na(r[1])) {
      currentBoundary <- currentBoundary+1
      boundary$left[currentBoundary] <- gapIdx+1
      boundary$right[currentBoundary] <- right
    } else {
      mass[(gapIdx+1):right] <- r
    }

    ## stack size have to be increased?
    ## (should rarely happen because recursion deep is mostly < 20)
    if (currentBoundary == nBoundaries) {
      nBoundaries <- floor(nBoundaries*1.5)
      boundary$left <- c(boundary$left,
                         double(nBoundaries-currentBoundary))
      boundary$right <- c(boundary$right,
                          double(nBoundaries-currentBoundary))
    }
  }
  return(mass)
}

