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

## referencePeaks
##  calculate reference peaks (a wrapper around filterPeaks and binPeaks)
##
## params:
##  l: list of MassPeaks objects
##  method: character, grouper to used (strict: don't allow multiple peaks of
##          the same sample in the same bin, relaxed: allow them)
##  minFrequency: double, minimal frequency of a peak to be not removed
##  tolerance: double, maximal deviation of a peak position to be
##             considered as same peak
##
## returns:
##  a new MassPeaks object
##
referencePeaks <- function(l, method=c("strict", "relaxed"), minFrequency=0.9,
                           tolerance=0.002) {

  .stopIfNotIsMassPeaksList(l)

  ## find reference peaks by binning and filtering
  referencePeaks <- filterPeaks(binPeaks(l, method=method,
                                         tolerance=tolerance),
                                minFrequency=minFrequency)

  m <- .as.binary.matrix(.as.matrix.MassObjectList(referencePeaks))

  ## set peak intensity to number of occurrence
  intensity <- unname(colMeans(m))

  createMassPeaks(mass=attr(m, "mass"), intensity=intensity)
}
