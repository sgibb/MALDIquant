## Copyright 2011-2012 Sebastian Gibb
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
    definition=function(object, 
        index,
        mass,
        digits=3,
        underline=TRUE, 
        ## verticalOffset ca. 0.01 of plot height
        verticalOffset=abs(diff(par("usr")[3:4]))*0.01,
        absoluteVerticalPos,
        adj=c(0.5, 0), cex=0.7, 
        avoidOverlap=FALSE,
        arrowLength=0, arrowLwd=0.5, arrowCol=1,
        ...) {

    if (!missing(mass) && is.numeric(mass)) {
        massIndex <- .which.closest(mass, object@mass)

        if (!missing(index)) {
            if (is.logical(index)) {
                warning("Could not handle a logical ", sQuote("index"), 
                        " and a numeric ", sQuote("mass"), " vector. ",
                        "Replacing ", sQuote("index"), " by ", sQuote("mass"),
                        ".");
                index <- massIndex;
            } else {
                index <- c(index, massIndex);
            }
        } else {
            index <- massIndex;
        }
        
        ## remove duplicated indices
        index <- unique(index);
    } else if (missing(mass) && missing(index)) {
        index <- 1:length(object@mass);
    }

    isValidIndex <- length(index) >= 1 && 
                    length(index) <= length(object@mass) && 
                    ((min(index) >= 1 && max(index) <= length(object@mass)) ||
                      is.logical(index));
    if (!isValidIndex) {
        stop("No valid ", sQuote("index"), " nor ", sQuote("mass"), " given.");
    }

    x <- object@mass[index];

    if (missing(absoluteVerticalPos)) {
        y <- object@intensity[index]+verticalOffset;
    } else {
        y <- absoluteVerticalPos;
    }

    peakLabels <- round(x=x, digits=digits);

    if (underline) {
        peakLabels <- as.expression(sapply(peakLabels,
                function(x)substitute(underline(a), list(a=x))));
    }

    if (avoidOverlap) {
      ## inspired by Ian Fellows' wordcloud::wordlayout
      p <- .calculateLabelPositions(object, x, y, peakLabels, adj=adj, cex=cex)

      ## create arrows from label to peak
      arrows(x0=p$x, y0=p$y, x1=x, y1=y, col=arrowCol, length=arrowLength,
             lwd=arrowLwd)
      ## no transparent background
      rect(xleft=p$xleft, ybottom=p$ybottom, xright=p$xright, ytop=p$ytop,
           col="white", border=NA, density=-1)
      x <- p$x
      y <- p$y
    }
    text(x=x, y=y, labels=peakLabels, adj=adj, cex=cex,...);
});

