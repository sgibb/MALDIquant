## grouper functions called by .binPeaks

## .grouperStrict
##  strict grouping function
##  Don't allow peaks of one sample in the same bin.
##
## params:
##  mass: double, sorted mass
##  intensities: double, corresponding intensities
##  samples: double, corresponding sample id numbers
##  tolerance: double, maximal deviation of a peak position to be
##             considered as same peak
##
## returns:
##  NA if further splitting is needed
##  meanMass (double) if all criteria are matched
##
.grouperStrict <- function(mass, intensities, samples, tolerance) {
  ## don't accept two or more peaks of the same sample
  if (anyDuplicated(samples)) {
    return(NA)
  }

  meanMass <- mean(mass)

  ## all peaks in range?
  if (any(abs(mass - meanMass) / meanMass > tolerance)) {
    return(NA)
  }

  meanMass
}

## .grouperRelaxed
##  relaxed grouping function (more than one peak of one sample per bin possible)
##  choose highest peak in range.
##
## params:
##  mass: double, sorted mass
##  intensities: double, corresponding intensities
##  samples: double, corresponding sample id numbers
##  tolerance: double, maximal deviation of a peak position to be
##             considered as same peak
##  meanMass: double, mean of mass (new peak position)
##
## returns:
##  NA if further splitting is needed
##  meanMass (double) if all criteria are matched
##
.grouperRelaxed <- function(mass, intensities, samples, tolerance) {
  meanMass <- mean(mass)

  ## all peaks in range?
  if (any(abs(mass - meanMass) / meanMass > tolerance)) {
    return(NA)
  }

  ## choose highest peak in duplicates
  if (anyDuplicated(samples)) {
    s <- sort.int(intensities, decreasing=TRUE, index.return=TRUE)
    samples <- samples[s$ix]

    noDup <- !duplicated(samples)

    noDup[s$ix] <- noDup

    ## replace mass corresponding to highest intensity
    mass[noDup] <- mean(mass[noDup])

    return(mass)
  }

  meanMass
}

## .grouperRelaxedHighestAtReference
##  relaxed grouping function (more than one peak of one sample per bin possible)
##  Choose highest test sample peaks in range around a reference peak.
##
## params:
##  mass: double, sorted mass
##  intensities: double, corresponding intensities
##  samples: double, corresponding sample id numbers (1==reference)
##  tolerance: double, maximal deviation of a peak position to be
##             considered as same peak
##  nomatch: return value if no reference peak found, mass for original mass, 0L
##           for `determineWarpingFunctions`
##
## returns:
##  NA if further splitting is needed
##  meanMass (double) if all criteria are matched else 0
##
.grouperRelaxedHighestAtReference <- function(mass, intensities, samples,
                                              tolerance, nomatch=mass) {
  ## any reference peaks in current samples?
  ref <- samples == 1L
  nRef <- sum(ref)
  if (nRef == 0L) {
    ## no reference peak
    return(nomatch)
  } else if (nRef > 1L) {
    ## too many reference peaks => further splitting needed
    return(NA)
  }

  ## only one mass should left as reference mass
  meanMass <- mass[ref]

  ## all peaks in range?
  if (any(abs(mass - meanMass) / meanMass > tolerance)) {
    return(NA)
  }

  ## choose highest peak in duplicates
  if (anyDuplicated(samples)) {
    s <- sort.int(intensities, decreasing=TRUE, index.return=TRUE)
    samples <- samples[s$ix]
    noDup <- !duplicated(samples)
    noDup[s$ix] <- noDup

    ## replace mass corresponding to highest intensity
    mass[] <- nomatch
    mass[noDup] <- meanMass

    return(mass)
  }

  meanMass
}
