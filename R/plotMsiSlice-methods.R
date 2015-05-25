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

## list
setMethod(f="plotMsiSlice",
          signature=signature(x="list"),
          definition=function(x, center, tolerance,
                              colRamp=colorRamp(c("black", "blue", "green",
                                                  "yellow", "red")),
                              interpolate=FALSE, scale=TRUE, legend=TRUE, ...) {
  .stopIfNotIsMassObjectList(x)
  slides <- msiSlices(x, center=center, tolerance=tolerance)
  plotMsiSlice(slides, colRamp=colRamp, interpolate=interpolate, scale=scale,
               legend=legend, ...)
})

## array
setMethod(f="plotMsiSlice",
          signature=signature(x="array"),
          definition=function(x, colRamp=colorRamp(c("black", "blue", "green",
                                                     "yellow", "red")),
                              interpolate=FALSE, scale=TRUE, legend=TRUE,
                              sub=NULL, ...) {
  n <- dim(x)[3L]
  if (n > 1L && dev.interactive()) {
    warning(sQuote("plotMsiSlice"), " was called for multiple slice on an ",
            "interactive device. Only the first slice is plotted. Use ",
            sQuote("pdf"), " or a similar device to plot all slices at once.")
    n <- 1L
  }

  center <- attr(x, "center")
  tolerance <- attr(x, "tolerance")

  for (i in 1L:n) {
    if (is.null(sub)) {
      sub <- bquote(.(center[i]) %+-% .(tolerance))
    }
    plotMsiSlice(x[,, i], colRamp=colRamp, interpolate=interpolate, scale=scale,
                 legend=legend, sub=sub, ...)
  }
})

## matrix
setMethod(f="plotMsiSlice",
          signature=signature(x="matrix"),
          definition=function(x, colRamp=colorRamp(c("black", "blue", "green",
                                                     "yellow", "red")),
                              interpolate=FALSE, scale=TRUE, legend=TRUE, ...) {
  .plotMsiSlice(x, colRamp=colRamp, interpolate=interpolate, scale=scale,
                legend=legend, ...)
})
