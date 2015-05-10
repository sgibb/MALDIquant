## Copyright 2013 Sebastian Gibb
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

## alignSpectra
##  wrapper around detectPeaks, determineWarpingFunctions and warpMassSpectra
##
## params:
##  spectra: list, list of MassSpectrum objects
##  halfWindowSize: numeric, half window size.
##  noiseMethod: character, noise estimation method
##  SNR: double, signal-to-noise ratio
##  reference: MassPeaks, a reference MassPeaks object to which all other
##             MassPeaks objects should be aligned
##  tolerance: double, maximal deviation of a peak position to be
##             considered as same peak
##  warpingMethod: choose type of base warping function
##
## returns:
##  a list of aligned MassSpectrum objects
##
alignSpectra <- function(spectra,
                         ## peak detection
                         halfWindowSize=20, noiseMethod="MAD", SNR=2,
                         ## warping
                         reference, tolerance=0.002, warpingMethod="lowess",
                         ...) {

  ## test arguments
  .stopIfNotIsMassSpectrumList(spectra)

  peaks <- detectPeaks(spectra, halfWindowSize=halfWindowSize,
                       method=noiseMethod, SNR=SNR, ...)
  wf <- determineWarpingFunctions(peaks, reference=reference,
                                  tolerance=tolerance, method=warpingMethod)
  warpMassSpectra(spectra, wf)
}
