## Copyright 2015 Sebastian Gibb
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
    if (n > 1L && dev.interactive() && !plotInteractive) {
      warning(sQuote("plotMsiSlice"), " was called for multiple slice on an ",
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
