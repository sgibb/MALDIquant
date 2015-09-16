## Copyright 2011-2015 Sebastian Gibb
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

## MassSpectrum

## createMassSpectrum
##  default constructor: MassSpectrum class
##
## params:
##  mass: vector, spectrum mass
##  intensity: vector, spectrum intensities
##  metaData: list, metadata
##
## returns:
##  a MassSpectrum object
##
createMassSpectrum <- function(mass, intensity, metaData=list()) {
  new(Class="MassSpectrum", mass=mass, intensity=intensity, metaData=metaData)
}

## end of MassSpectrum

## MassPeaks

## createMassPeaks
##  default constructor: MassPeaks class
##
## params:
##  mass: vector, peaks mass
##  intensity: vector, peaks intensities
##  snr: vector, peaks snr
##  metaData: list, metadata
##
## returns:
##  a MassPeaks object
##
createMassPeaks <- function(mass, intensity,
                            snr=rep.int(NA_real_, length(intensity)),
                            metaData=list()) {
  new(Class="MassPeaks", mass=mass, intensity=intensity, snr=snr,
      metaData=metaData)
}

## end of MassPeaks
