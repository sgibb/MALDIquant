## determineWarpingFunctions
##  calculates warping functions for each MassPeaks object
##
## params:
##  l: list, list of MassPeaks objects
##  reference: MassPeaks, a reference MassPeaks object to which all other
##             MassPeaks objects should be aligned
##  tolerance: double, maximal deviation of a peak position to be
##             considered as same peak
##  method: choose type of base warping function
##  allowNoMatches: logical, don't throw an error if a single MassPeaks object
##                can't match to the reference.
##  plot: logical, plots warping function
##
## returns:
##  a list of warping functions
##
determineWarpingFunctions <- function(l, reference, tolerance=0.002,
                                      method=c("lowess", "linear", "quadratic",
                                               "cubic"),
                                      allowNoMatches=FALSE,
                                      plot=FALSE, plotInteractive=FALSE, ...) {

  ## test arguments
  if (!isMassPeaksList(l) && !isMassPeaks(l)) {
    stop(sQuote("l"), " is no list of MALDIquant::MassPeaks objects!")
  }

  method <- match.arg(method)

  if (allowNoMatches) {
      nomatchReporter <- warning
  } else {
      nomatchReporter <- stop
  }

  warpingFunction <- switch(method,
    "lowess" = {
      .warpingFunctionLowess
    },
    "linear" = {
      .warpingFunctionLinear
    },
    "quadratic" = {
      .warpingFunctionQuadratic
    },
    "cubic" = {
      .warpingFunctionCubic
    }
  )

  minPeaks <- switch(method,
    "lowess" = 2L,
    "linear" = 2L,
    "quadratic" = 3L,
    "cubic" = 4L
  )

  optArgs <- list(...)

  ## find reference peaks
  if (missing(reference)) {
    arguments <- list(l=l, tolerance=tolerance)
    argumentNames <- c("method", "minFrequency")

    arguments <- modifyList(arguments, optArgs[argumentNames])
    optArgs[argumentNames] <- NULL

    reference <- do.call(referencePeaks, arguments)
  }

  if (isEmpty(reference)) {
    stop("Reference MassPeaks object contains no peaks!")
  }

  if (length(reference) < 10L) {
    warning("Reference MassPeaks object contains very few peaks (n == ",
            length(reference), "). The warping could be instable. ",
            "Consider to reduce ", sQuote("minFrequency"),
            " or/and to increase ", sQuote("tolerance"), ".")
  }

  ## fetch plot.default arguments (debug plot)
  if (plot) {
    plotNames <- c("xlim", "ylim", "xlab", "ylab", "type", "lwd", "col",
                   "col.sub", "cex.main", "cex.sub", "main", "sub")

    givenPlotArgs <- optArgs[plotNames]
    optArgs[plotNames] <- NULL
  }

  ## reference has to become sample no 1
  tmpPeakList <- c(reference, l)

  ## same procedure as in binPeaks
  ## fetch all mass
  mass <- unname(.unlist(lapply(tmpPeakList, function(x)x@mass)))

  ## fetch all intensities
  intensities <- .unlist(lapply(tmpPeakList, function(x)x@intensity))

  ## store original mass sample number/id
  samples <- rep.int(seq_along(tmpPeakList), lengths(tmpPeakList))

  ## sort values by mass
  s <- sort.int(mass, index.return=TRUE)

  mass <- s$x
  intensities <- intensities[s$ix]
  samples <- samples[s$ix]

  ## run peak binning and use relaxed grouper which choose the highest test
  ## sample peaks
  binnedMass <- .binPeaks(mass=mass, intensities=intensities, samples=samples,
                          tolerance=tolerance,
                          grouper=.grouperRelaxedHighestAtReference, nomatch=0L)

  ## group mass/intensities by sample ids
  lIdx <- split(seq_along(binnedMass), samples)

  ## calculate differences
  binnedMass[binnedMass == 0L] <- NA_real_
  d <- binnedMass - mass

  ## each function which determines a warping function uses these 3 arguments
  arguments <- list(x=NULL, d=NULL)
  if (length(optArgs)) {
    arguments <- c(arguments, optArgs)
  }

  ## determine warping functions
  warpingFunctions <- lapply(lIdx[-1L], function(i) {
    ## fetch changed mass == aligned peaks
    notNA <- !is.na(binnedMass[i])

    arguments$x <- mass[i][notNA] ## original mass
    arguments$d <- d[i][notNA]  ## difference to reference

    if (!length(arguments$x)) {
      nomatchReporter("Could not match any peak in spectrum ",
                      samples[i[1L]] - 1L, " to a reference peak.")
      NA
    } else if (length(arguments$x) < minPeaks) {
      nomatchReporter("Could not match enough peaks in spectrum ",
                      samples[i[1L]] - 1L, " to the reference peaks.\n",
                      minPeaks, " matches required, just ",
                      length(arguments$x), " found.")
      NA
    } else {
      do.call(warpingFunction, arguments)
    }
  })

  ## clean misleading names (names == idx+1 because reference is idx == 1)
  names(warpingFunctions) <- NULL

  ## number of matched peaks
  nmatch <-  vapply(split(d, samples)[-1L], function(dd) {
      sum(!is.na(dd), na.rm=TRUE)
  }, NA_real_, USE.NAMES=FALSE)
  names(nmatch) <- names(l)
  attr(warpingFunctions, "nmatch") <- nmatch

  ## debug plot
  if (plot) {
    ## non interactive device (pdf, png, ...) available?
    isNonInteractivePlot <- dev.cur() != 1L && !dev.interactive()

    if (!isNonInteractivePlot && !plotInteractive) {
      warning(sQuote("plot"), " is ", sQuote("TRUE"),
              " but no non-interactive device is available. ",
              "Using pdf() to create a default one.")
      pdf(paper="a4r", width=12)
    } else if (dev.cur() == 1L && plotInteractive) {
      warning(sQuote("plot"), " is ", sQuote("TRUE"),
              " but no interactive device is available. ",
              "Using dev.new() to create a default one.")
      dev.new()
    }

    ## set default plot arguments
    plotArgsDefaults <- list(xlim=range(mass),
                             ylim=range(d, na.rm=TRUE),
                             xlab="mass",
                             ylab="difference",
                             type="p",
                             lwd=1L,
                             col=1L,
                             cex.main=0.8,
                             cex.sub=0.75,
                             col.sub="#808080")

    plotArgs <- modifyList(plotArgsDefaults, givenPlotArgs)

    nReference <- length(reference)
    x <- plotArgs$xlim[1L]:plotArgs$xlim[2L]

    ## workaround to avoid error:
    ## Error in l[[i]] : this S4 class is not subsettable
    if (!is.list(l)) {
      l <- list(l)
    }

    for (i in seq_along(l)) {
      ## fetch changed mass == aligned peaks
      notNA <- !is.na(binnedMass[lIdx[[i + 1L]]])

      if (is.null(givenPlotArgs$main)) {
        plotArgs$main <- paste0("sample ", i, " vs reference\n",
                                "(matched peaks: ", sum(notNA), "/",
                                nReference, ")")
      }

      if (is.null(givenPlotArgs$sub)) {
        plotArgs$sub <- l[[i]]@metaData$file
      }

      ## plot reference vs sample
      plotArgs$x <- l[[i]]@mass[notNA]
      plotArgs$y <- d[lIdx[[i + 1L]]][notNA]
      do.call(plot.default, plotArgs)

      ## draw warping function
      lines(x, warpingFunctions[[i]](x), lwd=plotArgs$lwd, col=plotArgs$col)
    }

    if (!isNonInteractivePlot && !plotInteractive) {
      dev.off()
    }
  }

  warpingFunctions
}
