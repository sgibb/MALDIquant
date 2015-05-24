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

## .msiSlices
##  create intensity matrix for each slice from a list of MassSpectrum/MassPeaks
##  objects
##
## params:
##  x: list of MassSpectrum/MassPeaks objects
##  range: double, length == 2, range/thickness of the slice
##
## returns:
##  an array (dim: x, y, z=slice nr)
##
msiSlices <- function(x, center, tolerance, method=c("sum", "mean", "median"),
                      adjust=TRUE) {
  x <- suppressWarnings(trim(x, range=range(center)+c(-tolerance, tolerance)))

  .msiSlices(m=.as.matrix.MassObjectList(x),
             coord=coordinates(x, adjust=adjust),
             center=center, tolerance=tolerance, method=method)
}

.msiSlices <- function(m, coord, center, tolerance,
                       method=c("sum", "mean", "median"), scale=TRUE) {
  method <- match.arg(method)

  fun <- switch(method,
                "sum" = rowSums,
                "mean" = rowMeans,
                "median" = function(x, ...).colMedians(t(x), ...))

  n <- unname(apply(coord, MARGIN=2L, FUN=max))

  l <- findInterval(center-tolerance, attr(m, "mass"), all.inside=TRUE)
  r <- findInterval(center+tolerance, attr(m, "mass"), all.inside=TRUE)

  nr <- nrow(coord)

  slices <- array(NA, dim=c(x=n[1L], y=n[2L], z=length(center)))

  for (i in seq(along=center)) {
    slice <- fun(m[, l[i]:r[i], drop=FALSE], na.rm=TRUE)
    slices[cbind(coord, i)] <- slice
  }

  attr(slices, "center") <- center
  attr(slices, "tolerance") <- tolerance
  slices
}
