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

## AbstractMassObject
if (is.null(getGeneric("as.matrix"))) {
  setGeneric("as.matrix", function(x, ...) standardGeneric("as.matrix"))
}
if (is.null(getGeneric("length"))) {
  setGeneric("length", function(x) standardGeneric("length"))
}
if (is.null(getGeneric("lines"))) {
  setGeneric("lines", function(x, ...) standardGeneric("lines"))
}
if (is.null(getGeneric("plot"))) {
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))
}
if (is.null(getGeneric("plotMsiSlice"))) {
  setGeneric("plotMsiSlice", function(x, ...) standardGeneric("plotMsiSlice"))
}
if (is.null(getGeneric("points"))) {
  setGeneric("points", function(x, ...) standardGeneric("points"))
}
if (is.null(getGeneric(".prepareShow"))) {
  setGeneric(".prepareShow", function(object) standardGeneric(".prepareShow"))
}
if (is.null(getGeneric("transformIntensity"))) {
  setGeneric("transformIntensity",
             function(object, ...) standardGeneric("transformIntensity"))
}
if (is.null(getGeneric(".transformIntensity"))) {
  setGeneric(".transformIntensity",
             function(object, ...) standardGeneric(".transformIntensity"))
}
if (is.null(getGeneric("trim"))) {
  setGeneric("trim", function(object, range, ...) standardGeneric("trim"))
}

## get/set slots
if (is.null(getGeneric("mass"))) {
  setGeneric("mass", function(object, ...) standardGeneric("mass"))
}
if (is.null(getGeneric("mass<-"))) {
  setGeneric("mass<-", function(object, value) standardGeneric("mass<-"))
}
if (is.null(getGeneric("intensity"))) {
  setGeneric("intensity", function(object, ...) standardGeneric("intensity"))
}
if (is.null(getGeneric("intensity<-"))) {
  setGeneric("intensity<-",
             function(object, value) standardGeneric("intensity<-"))
}
if (is.null(getGeneric("isEmpty"))) {
  setGeneric("isEmpty", function(x) standardGeneric("isEmpty"))
}
if (is.null(getGeneric(".isEmptyWarning"))) {
  setGeneric(".isEmptyWarning", function(x) standardGeneric(".isEmptyWarning"))
}
if (is.null(getGeneric("metaData"))) {
  setGeneric("metaData", function(object) standardGeneric("metaData"))
}
if (is.null(getGeneric("metaData<-"))) {
  setGeneric("metaData<-",
             function(object, value) standardGeneric("metaData<-"))
}
if (is.null(getGeneric("coordinates"))) {
  setGeneric("coordinates",
             function(object, ...) standardGeneric("coordinates"))
}
if (is.null(getGeneric("coordinates<-"))) {
  setGeneric("coordinates<-",
             function(object, value) standardGeneric("coordinates<-"))
}

## end of AbstractMassObject

## MassSpectrum
if (is.null(getGeneric("approxfun"))) {
  setGeneric("approxfun",
             function(x, y=NULL, method="linear", yleft, yright, rule=1, f=0,
                      ties=mean)
               standardGeneric("approxfun"))
}
if (is.null(getGeneric("calibrateIntensity"))) {
  setGeneric("calibrateIntensity",
             function(object, ...) standardGeneric("calibrateIntensity"))
}
if (is.null(getGeneric("detectPeaks"))) {
  setGeneric("detectPeaks",
             function(object, ...) standardGeneric("detectPeaks"))
}
if (is.null(getGeneric("estimateBaseline"))) {
  setGeneric("estimateBaseline",
             function(object, method=c("SNIP", "ConvexHull", "Median"), ...)
               standardGeneric("estimateBaseline"))
}
if (is.null(getGeneric("estimateNoise"))) {
  setGeneric("estimateNoise",
             function(object, ...) standardGeneric("estimateNoise"))
}
if (is.null(getGeneric(".findLocalMaxima"))) {
  setGeneric(".findLocalMaxima",
             function(object, halfWindowSize=20L)
               standardGeneric(".findLocalMaxima"))
}
if (is.null(getGeneric(".findLocalMaximaLogical"))) {
  setGeneric(".findLocalMaximaLogical",
             function(object, halfWindowSize=20L)
               standardGeneric(".findLocalMaximaLogical"))
}
if (is.null(getGeneric("isRegular"))) {
  setGeneric("isRegular",
             function(object, ...) standardGeneric("isRegular"))
}
if (is.null(getGeneric("removeBaseline"))) {
  setGeneric("removeBaseline",
             function(object, ...) standardGeneric("removeBaseline"))
}
if (is.null(getGeneric("smoothIntensity"))) {
  setGeneric("smoothIntensity",
             function(object, ...)
               standardGeneric("smoothIntensity"))
}
if (is.null(getGeneric("totalIonCurrent"))) {
  setGeneric("totalIonCurrent",
             function(object) standardGeneric("totalIonCurrent"))
}
## end of MassSpectrum

## MassPeaks
if (is.null(getGeneric("labelPeaks"))) {
  setGeneric("labelPeaks", function(object, ...) standardGeneric("labelPeaks"))
}
if (is.null(getGeneric("snr"))) {
  setGeneric("snr", function(object) standardGeneric("snr"))
}
## end of MassPeaks
