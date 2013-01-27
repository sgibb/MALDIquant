## Copyright 2011-2013 Sebastian Gibb
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

## basic class for all spectra based information
setClass("AbstractMassObject",
         representation=representation(mass="vector", intensity="vector",
                                       metaData="list", .cache="environment"),
         prototype=prototype(mass=vector(mode="numeric"),
                             intensity=vector(mode="numeric"), metaData=list()),
         contains="VIRTUAL")

## represent a spectrum
setClass("MassSpectrum",
         contains="AbstractMassObject")

## represent a peak list from a single spectrum
setClass("MassPeaks",
         representation=representation(snr="vector"),
         prototype=prototype(snr=vector(mode="numeric")),
         contains="AbstractMassObject")

