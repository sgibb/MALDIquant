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

## .calculateLabelPositions
##  calculate label positions to avoid collisions 
##
## params:
##  x: original x coordinates for labels
##  y: original y coordinates for labels
##  peakLabels: peak labels
##  adj: text alignment ?par("adj")
##  cex: font size ?par("cex")
##  maxSteps: max tries to avoid collisons
##
## returns:
##  a matrix of coordinates
##
setMethod(f=".calculateLabelPositions",
  signature=signature(object="MassPeaks"),
  definition=function(object, x, y, peakLabels, adj, cex, maxSteps=100) {
  ## start with smallest peak
  i <- sort(y, index.return=TRUE, method="quick")$ix

  ## calculate label rectangles
  rects <- .textLabelRects(x[i], y[i], peakLabels[i], adj=adj, cex=cex)

  ## move rectangles around to avoid collisons
  for (j in seq(along=x)) {
    rects[j, ] <- .testLabelOverlap(object, rects, currentIndex=j, maxSteps=maxSteps) 
  }

  ## undo sorting
  rects[i, ] <- rects

  return(list(x=rects[, "x"], y=rects[, "y"], 
              xleft=rects[, "x0"], ybottom=rects[, "y0"],
              xright=rects[, "x1"], ytop=rects[, "y1"]))
})

## .testLabelOverlap
##  try to avoid overlap/collisions
##
## params:
##  rects: a matrix of coordinates (created by .textRects)
##  currentIndex: which coordinates should moved around
##  maxSteps: max tries to avoid collisons
##
## returns:
##  a vector of coordinates
##
setMethod(f=".testLabelOverlap",
  signature=signature(object="MassPeaks"),
  definition=function(object, rects, currentIndex, maxSteps) {
  
  r <- pi/180*c(90, as.vector(rbind(seq(80, 40, by=-10), seq(100, 140, by=10))))
  
  for (k in 0:maxSteps) {
    ## move up
    cur <- rects[currentIndex, ]
    cur[c("y0", "y1", "y")] <- cur[c("y0", "y1", "y")] + k * cur[c("h")]
    isOverlapped <- .labelOverlap(object, cur, rects[1:(currentIndex-1), ])

    if (isOverlapped) {
      for (l in r) {
        ## move in curve
        oldcur <- cur
        cur[c("y0", "y1", "y")] <- cur[c("y0", "y1", "y")] + sin(l) * cur[c("h")]
        cur[c("x0", "x1", "x")] <- cur[c("x0", "x1", "x")] + cos(l) * cur[c("w")] 
        isOverlapped <- .labelOverlap(object, cur, rects[1:(currentIndex-1), ])

        if (!isOverlapped) {
          ## success
          return(cur)
        }
        cur <- oldcur
      }
    } else {
      ## success
      return(cur)
    }
  }
  ## no success, return original pos
  return(rects[currentIndex, ])
})

## .labelOverlap
##  does current rect overlap any other one?
##
## params:
##  cur: vector of coordinates (which should test agains 'rects')
##  rects: a matrix of coordinates (created by .textRects)
##
## returns:
##  TRUE/FALSE
##
setMethod(f=".labelOverlap",
  signature=signature(object="MassPeaks"),
  definition=function(object, cur, rects) {

  x <- cur[c(1, 3)]
  y <- cur[c(2, 4)]

  rects <- matrix(rects, ncol=8)

  ## peak overlap?
  peakOverlap <- any(x[1] <= object@mass & x[2] >= object@mass & 
                     y[1] <= object@intensity)
                 
  if (peakOverlap) {
    return(TRUE)
  }

  ## text overlap?
  textOverlap <- any(((x[1] > rects[, 1] & x[1] < rects[, 3]) |
                      (x[2] > rects[, 1] & x[2] < rects[, 3]) |
                      (x[1] < rects[, 1] & x[2] > rects[, 3])) &
                     ((y[1] > rects[, 2] & y[1] < rects[, 4]) |
                      (y[2] > rects[, 2] & y[2] < rects[, 4]) |
                      (y[1] < rects[, 2] & y[2] > rects[, 4])))

  return(textOverlap)
})

