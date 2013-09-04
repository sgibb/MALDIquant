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

## AbstractMassObject
setMethod(f="smoothIntensity",
          signature=signature(object="MassSpectrum"),
          definition=function(object,
                              method=c("SavitzkyGolay", "MovingAverage"),
                              halfWindowSize, ...) {
  ## empty spectrum?
  if (.isEmptyWarning(object)) {
    return(object)
  }

  method <- match.arg(method)

  switch(method,
         "SavitzkyGolay" = {
           fun <- .savitzkyGolay
           if (missing(halfWindowSize)) {
             halfWindowSize <- 10L
           }
         },
         "MovingAverage" = {
           fun <- .movingAverage
           if (missing(halfWindowSize)) {
             halfWindowSize <- 2L
           }
         },
         {
           stop("Unknown ", sQuote("method"), ".")
         }
  )

  return(.transformIntensity(object, fun=fun,
                             halfWindowSize=halfWindowSize, ...))
})

## list
setMethod(f="smoothIntensity",
          signature=signature(object="list"),
          definition=function(object, ...) {

  ## test arguments
  .stopIfNotIsMassSpectrumList(object)

  return(lapply(object, smoothIntensity, ...))
})

