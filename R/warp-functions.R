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

## warpMassSpectra
##  warp MassSpectrum objects
##
## params:
##  l: list of MassSpectrum objects
##  w: list of warping functions determined by determineWarpingFunctions
##
## returns:
##  a list of warped MassSpectrum objects
##
warpMassSpectra <- function(l, w) {
  .stopIfNotIsMassSpectrumList(l)
  .stopIfNotIsFunctionList(w)

  .warp(l, w)
}

## warpMassPeaks
##  warp MassPeaks objects
##
## params:
##  l: list of MassPeaks objects
##  w: list of warping functions determined by determineWarpingFunctions
##
## returns:
##  a list of warped MassPeaks objects
##
warpMassPeaks <- function(l, w) {
  .stopIfNotIsMassPeaksList(l)
  .stopIfNotIsFunctionList(w)

  .warp(l, w)
}


## .warp
##  .warp bstractMassObject objects
##
## params:
##  l: list of AbstractMassObject objects
##  w: list of warping functions determined by determineWarpingFunctions
##
## returns:
##  a list of warped AbstractMassObject objects
##
.warp <- function(l, w) {
  .mapply(function(m, wf) {
           m@mass <- m@mass + wf(m@mass)
           m
  }, m=l, wf=w)
}
