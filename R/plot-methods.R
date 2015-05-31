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
setMethod(f="plot",
          signature=signature(x="AbstractMassObject", y="missing"),
          definition=function(x, col="black", xlab="mass", ylab="intensity",
                              type=ifelse(isMassPeaks(x), "h", "l"),
                              xlim=c(ifelse(length(x@mass),
                                            min(x@mass, na.rm=TRUE), 0L),
                                     ifelse(length(x@mass),
                                            max(x@mass, na.rm=TRUE), 1L)),
                              ylim=c(0, ifelse(length(x@intensity),
                                               max(x@intensity, na.rm=TRUE),
                                               1L)),
                              main=x@metaData$name, sub=x@metaData$file,
                              cex.sub=0.75, col.sub="#808080",
                              abline.col="#808080", ...) {

  if (all(sub == x@metaData$file) && length(x@metaData$file) > 1L) {
    sub <- paste0(ifelse(isMassSpectrum(x),
                         "averaged spectrum", "merged peaks"),
                  " composed of ", length(x@metaData$file), " ",
                  ifelse(isMassSpectrum(x), "MassSpectrum", "MassPeaks"),
                  " objects")
  }

  plot(x=x@mass, y=x@intensity, col=col, type=type, xlab=xlab, ylab=ylab,
       xlim=xlim, ylim=ylim, main=main, sub=sub, cex.sub=cex.sub,
       col.sub=col.sub, ...)
  abline(h=0L, col=abline.col)
})
