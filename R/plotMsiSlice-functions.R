## Copyright 2014-2015 Sebastian Gibb
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

.plotMsiSlice <- function(x, colRamp=colorRamp(c("black", "blue", "green",
                                                 "yellow", "red")),
                          xlab="", ylab="", interpolate=FALSE, scale=TRUE,
                          legend=TRUE, ...) {
  stopifnot(is.matrix(x))

  if (scale) {
    x <- x/max(x, na.rm=TRUE)
  }

  notNA <- which(!is.na(x))
  x[notNA] <- rgb(colRamp(x[notNA]), maxColorValue=255L)

  xlim <- c(0L, nrow(x) + (2L * legend))
  ylim <- c(0L, ncol(x))

  ## prepare plot area
  plot(NA, type="n", xlim=xlim, ylim=ylim,
       axes=FALSE, xlab=xlab, ylab=ylab,
       asp=1L, ...)

  ## plot image
  rasterImage(as.raster(t(x)),
              xleft=0L, xright=nrow(x), ybottom=0L, ytop=ncol(x),
              interpolate=interpolate)

  if (legend) {
    .msiLegend(xlim[2L]-1L, xlim[2L], ylim[1L], ylim[2L],
               colRamp=colRamp, interpolate=interpolate)
  }
}

.msiLegend <- function(xleft, xright, ybottom, ytop,
                       colRamp=colorRamp(c("black", "blue", "green", "yellow",
                                           "red")),
                       interpolate=FALSE) {
  col <- rgb(colRamp(seq.int(1, 0, length.out=100L)), maxColorValue=255L)
  gradient <- as.raster(matrix(col, ncol=1))
  rect(xleft=xleft, xright=xright, ybottom=ybottom, ytop=ytop, col="black")
  rasterImage(gradient, xleft=xleft, xright=xright, ybottom=ybottom, ytop=ytop,
              interpolate=interpolate)
}
