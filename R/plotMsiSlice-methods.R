setMethod(f="plotMsiSlice",
          signature=signature(x="list"),
          definition=function(x, center, tolerance,
                              colRamp=colorRamp(c("black", "blue", "green",
                                                   "yellow", "red")),
                              interpolate=FALSE, legend=TRUE, alignLabels=FALSE,
                              combine=FALSE, ...) {
  .stopIfNotIsMassObjectList(x)
  slides <- msiSlices(x, center=center, tolerance=tolerance)
  plotMsiSlice(slides, colRamp=colRamp, interpolate=interpolate, legend=legend,
               alignLabels=alignLabels, combine=combine, ...)
})

setMethod(f="plotMsiSlice",
          signature=signature(x="array"),
          definition=function(x, colRamp=colorRamp(c("black", "blue", "green",
                                                     "yellow", "red")),
                              interpolate=FALSE, legend=TRUE, alignLabels=FALSE,
                              combine=FALSE, plotInteractive=FALSE, ...) {
  n <- dim(x)[3L]

  if (!is.list(colRamp)) {
    colRamp <- rep_len(list(colRamp), n)
  }

  if (n != length(colRamp)) {
    stop(sQuote("dim(x)[3L]"), " (number of centers) has to be the same as ",
         "the length of the list ", sQuote("colRamp"), "!\n",
         "See ", sQuote("?plotMsiSlice"), " for details.")
  }

  if (combine) {
   .plotMsiSlice(x, colRampList=colRamp, interpolate=interpolate,
                  legend=legend, alignLabels=alignLabels, ...)

  } else {
    isNonInteractivePlot <- dev.cur() != 1L && !dev.interactive()
    if (n > 1L && !isNonInteractivePlot && !plotInteractive) {
      warning(sQuote("plotMsiSlice"), " was called for multiple slices on an ",
              "interactive device. Only the first slice is plotted. Use ",
              sQuote("pdf"), " or a similar device to plot all slices at once.",
              " Alternatively use ", dQuote("combine=TRUE"), " to plot ",
              "multiple centers in one plot.\n",
              "See ", sQuote("?plotMsiSlice"), " for details.")
      n <- 1L
    }

    tolerance <- rep_len(attr(x, "tolerance"), n)

    for (i in seq_len(n)) {
      .plotMsiSlice(x[,, i, drop=FALSE],
                    center=attr(x, "center")[i],
                    tolerance=tolerance[i],
                    colRampList=colRamp[i], interpolate=interpolate,
                    legend=legend, ...)
    }
  }
})

setMethod(f="plotMsiSlice",
          signature=signature(x="matrix"),
          definition=function(x, colRamp=colorRamp(c("black", "blue", "green",
                                                     "yellow", "red")),
                                interpolate=FALSE, scale=TRUE, legend=scale,
                                ...) {
  if (!is.list(colRamp)) {
    colRamp <- list(colRamp)
  }

  dim(x) <- c(dim(x), 1L)

  .plotMsiSlice(x, colRampList=colRamp, interpolate=interpolate, scale=scale,
                legend=legend, ...)
})
