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

setMethod(f="labelPeaks",
          signature=signature(object="MassPeaks"),
          definition=function(object, index, mass, labels, digits=3L,
                              underline=TRUE,
                              ## verticalOffset ca. 0.01 of plot height
                              verticalOffset=abs(diff(par("usr")[3L:4L]))*0.01,
                              absoluteVerticalPos, adj=c(0.5, 0L), cex=0.7,
                              avoidOverlap=FALSE, arrowLength=0L, arrowLwd=0.5,
                              arrowCol=1L, ...) {

  ## index
  if (missing(index) && missing(mass)) {
    index <- seq_along(object@mass)
  } else if (!missing(index) && is.logical(index)) {
    index <- which(index)
  }

  if (!missing(mass) && is.numeric(mass)) {
    massIdx <- .which.closest(mass, object@mass)

    if (missing(index)) {
      index <- massIdx
    } else {
      index <- c(index, massIdx)
    }

    ## remove duplicated indices
    index <- unique(index)
  }

  isValidIndex <- length(index) &&
                  length(index) <= length(object@mass) &&
                  (min(index) >= 1L && max(index) <= length(object@mass))

  if (!isValidIndex) {
    stop("No valid ", sQuote("index"), " nor ", sQuote("mass"), " given.")
  }

  x <- object@mass[index]

  ## labels
  if (missing(labels)) {
    labels <- round(x=x, digits=digits)
  } else if (!missing(labels) && length(index) != length(labels)) {
    stop("Lenghts of ", sQuote("index"), "/", sQuote("mass"), " and ",
         sQuote("labels"), " have to be equal.")
  }

  if (underline) {
    labels <- as.expression(sapply(labels,
      function(x)substitute(underline(a), list(a=x))))
  }

  if (missing(absoluteVerticalPos)) {
    y <- object@intensity[index] + verticalOffset
  } else {
    y <- absoluteVerticalPos
  }

  if (avoidOverlap) {
    ## inspired by Ian Fellows' wordcloud::wordlayout
    p <- .calculateLabelPositions(object, x, y, labels, adj=adj, cex=cex)

    ## create arrows from label to peak
    arrows(x0=p$x, y0=p$y, x1=x, y1=y, col=arrowCol, length=arrowLength,
           lwd=arrowLwd)
    ## no transparent background
    rect(xleft=p$xleft, ybottom=p$ybottom, xright=p$xright, ytop=p$ytop,
         col="white", border=NA, density=-1L)
    x <- p$x
    y <- p$y
  }
  text(x=x, y=y, labels=labels, adj=adj, cex=cex, ...)
})
