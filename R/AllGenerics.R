## AbstractMassObject
setGeneric("plotMsiSlice", function(x, ...) standardGeneric("plotMsiSlice"))
setGeneric(".prepareShow", function(object) standardGeneric(".prepareShow"))
setGeneric(
    "transformIntensity",
    function(object, ...) standardGeneric("transformIntensity")
)
setGeneric(
    ".transformIntensity",
    function(object, ...) standardGeneric(".transformIntensity")
)
setGeneric("trim", function(object, range, ...) standardGeneric("trim"))

## get/set slots
setGeneric("mass", function(object, ...) standardGeneric("mass"))
setGeneric("mass<-", function(object, value) standardGeneric("mass<-"))

# from ProtGenerics (same as mass)
setGeneric("mz", function(object, ...) standardGeneric("mz"))
setGeneric("mz<-", function(object, value) standardGeneric("mz<-"))

setGeneric("intensity", function(object, ...) standardGeneric("intensity"))
setGeneric(
    "intensity<-",
    function(object, value) standardGeneric("intensity<-")
)
setGeneric("isEmpty", function(x) standardGeneric("isEmpty"))
setGeneric(".isEmptyWarning", function(x) standardGeneric(".isEmptyWarning"))
setGeneric("metaData", function(object) standardGeneric("metaData"))
setGeneric("metaData<-", function(object, value) standardGeneric("metaData<-"))
setGeneric("coordinates", function(object, ...) standardGeneric("coordinates"))
setGeneric(
    "coordinates<-",
    function(object, value) standardGeneric("coordinates<-")
)
## end of AbstractMassObject

## MassSpectrum
setGeneric(
    "calibrateIntensity",
    function(object, ...) standardGeneric("calibrateIntensity")
)
setGeneric("detectPeaks", function(object, ...) standardGeneric("detectPeaks"))
setGeneric(
    "estimateBaseline",
    function(object, method=c("SNIP", "ConvexHull", "Median"), ...)
        standardGeneric("estimateBaseline"))
setGeneric(
    "estimateNoise", function(object, ...) standardGeneric("estimateNoise")
)
setGeneric(
    ".findLocalMaxima",
    function(object, halfWindowSize=20L) standardGeneric(".findLocalMaxima")
)
setGeneric(
    ".findLocalMaximaLogical",
    function(object, halfWindowSize=20L)
        standardGeneric(".findLocalMaximaLogical")
)
setGeneric("isRegular", function(object, ...) standardGeneric("isRegular"))
setGeneric(
    "removeBaseline", function(object, ...) standardGeneric("removeBaseline")
)
setGeneric(
    "smoothIntensity", function(object, ...) standardGeneric("smoothIntensity")
)
setGeneric(
    "totalIonCurrent", function(object) standardGeneric("totalIonCurrent")
)
## end of MassSpectrum

## MassPeaks
setGeneric("labelPeaks", function(object, ...) standardGeneric("labelPeaks"))
setGeneric(
    "monoisotopicPeaks",
    function(object, ...) standardGeneric("monoisotopicPeaks")
)
setGeneric("snr", function(object) standardGeneric("snr"))
## end of MassPeaks
