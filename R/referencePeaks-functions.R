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
