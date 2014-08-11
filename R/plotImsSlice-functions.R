## Copyright 2014 Sebastian Gibb
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

## plotImsSlice
##  plot IMS slice from a list of MassSpectrum/MassPeaks objects
##
## params:
##  x: list of MassSpectrum/MassPeaks objects
##  range: double, length == 2, range/thickness of the slice
##  sub: sub-title
##  removeEmptyRows: logical, Should empty rows be removed?
##  removeEmptyCols: logical, Should empty cols be removed?
##  colRamp: colours as colorRamp function
##  interpolate: logical, see rasterImage for details
##  ...: passed to plot
##
plotImsSlice <- function(x, range=c(0, Inf),
                         sub=paste0("m/z: ", range[1L], "-", range[2L], ""),
                         removeEmptyRows=TRUE,
                         removeEmptyCols=TRUE,
                         colRamp=colorRamp(c("black", "blue", "green",
                                             "yellow", "red")),
                         interpolate=FALSE, ...) {

  m <- .prepareImsSlice(x = x, range = range)

  if (removeEmptyRows) {
    kr <- rowSums(is.na(m)) != ncol(m)
    m <- m[kr, ]
    nr <- nrow(m)
  }
  if (removeEmptyCols) {
    kc <- colSums(is.na(m)) != nrow(m)
    m <- m[, kc]
    nc <- ncol(m)
  }

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

## .prepareImsSlice
##  create intensity matrix for a slice from a list of MassSpectrum/MassPeaks
##  objects
##
## params:
##  x: list of MassSpectrum/MassPeaks objects
##  range: double, length == 2, range/thickness of the slice
##
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

