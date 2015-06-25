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

## msiSlices
##  create intensity matrix for each slice from a list of MassSpectrum/MassPeaks
##  objects
##
## params:
##  x: list of MassSpectrum/MassPeaks objects
##  center: position(s) of interest (in mass values)
##  tolerance: aggregate data around center +/- tolerance
##  method: aggregate method
##  adjust: set coordinates to 1,1 (if there are empty/missing spectra before)
##
## returns:
##  an array (dim: x, y, z=slice nr)
##
msiSlices <- function(x, center, tolerance, method=c("sum", "mean", "median"),
                      adjust=TRUE) {
  x <- suppressWarnings(trim(x, range=range(center) + c(-tolerance, tolerance)))

  .msiSlices(m=.as.matrix.MassObjectList(x),
             coord=coordinates(x, adjust=adjust),
             center=center, tolerance=tolerance, method=method)
}

.msiSlices <- function(m, coord, center, tolerance,
                       method=c("sum", "mean", "median")) {
  method <- match.arg(method)

  fun <- switch(method,
                "sum" = rowSums,
                "mean" = rowMeans,
                "median" = function(x, ...).colMedians(t(x), ...))

  n <- unname(apply(coord, MARGIN=2L, FUN=max))

  l <- pmin(findInterval(center - tolerance - .Machine$double.eps,
                         attr(m, "mass")) + 1L, ncol(m))
  r <- findInterval(center + tolerance + .Machine$double.eps, attr(m, "mass"))

  slices <- array(NA_real_, dim=c(x=n[1L], y=n[2L], z=length(center)))

  for (i in seq_along(center)) {
    slices[cbind(coord, i)] <- fun(m[, l[i]:r[i], drop=FALSE], na.rm=TRUE)
  }

  attr(slices, "center") <- center
  attr(slices, "tolerance") <- tolerance
  attr(slices, "method") <- method
  slices
}
