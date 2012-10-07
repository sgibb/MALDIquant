## Copyright 2012 Sebastian Gibb
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

## .textLabelRects
##  calculate label rectangles
##
## params:
##  x: original x coordinates for labels
##  y: original y coordinates for labels
##  text: peak labels
##  adj: text alignment ?par("adj")
##  cex: font size ?par("cex")
##  offset: offset
##
## returns:
##  a matrix of coordinates
##
.textLabelRects <- function(x, y, text, adj, cex, offset=c(0.0, 0.2)) {
  return(t(mapply(function(xc, yc, t) {
    w <- strwidth(t, cex=cex)
    h <- strheight(t, cex=cex)
    ## extra calculation of offsets to be independend of adj
    woffset <- w*offset[1]
    hoffset <- h*offset[2]
    return(c(x0=xc-w*adj[1]-woffset, y0=yc-h*adj[2]-hoffset, 
             x1=xc+w*(1-adj[1])+woffset, y1=yc+h*(1-adj[2])+hoffset,
             x=xc, y=yc, w=w, h=h))
  }, xc=x, yc=y, t=text)))
}

