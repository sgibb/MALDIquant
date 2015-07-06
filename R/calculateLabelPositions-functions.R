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
##  object: a single MassPeaks object
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
.calculateLabelPositions <- function(object, x, y, peakLabels, adj, cex,
                                     maxSteps=100L) {
  ## start with smallest peak
  i <- sort.int(y, index.return=TRUE, method="quick")$ix

  ## calculate label rectangles
  rects <- .textLabelRects(x[i], y[i], peakLabels[i], adj=adj, cex=cex)

  ## move rectangles around to avoid collisons
  for (j in seq_along(x)) {
    rects[j, ] <- .testLabelOverlap(object, rects, currentIndex=j,
                                    maxSteps=maxSteps)
  }

  ## undo sorting
  rects[i, ] <- rects

  list(x=rects[, "x"], y=rects[, "y"],
       xleft=rects[, "x0"], ybottom=rects[, "y0"],
       xright=rects[, "x1"], ytop=rects[, "y1"])
}

## .testLabelOverlap
##  try to avoid overlap/collisions
##
## params:
##  object: a single MassPeaks object
##  rects: a matrix of coordinates (created by .textRects)
##  currentIndex: which coordinates should moved around
##  maxSteps: max tries to avoid collisons
##
## returns:
##  a vector of coordinates
##
.testLabelOverlap <- function(object, rects, currentIndex, maxSteps) {
  r <- pi / 180L * c(90, as.vector(rbind(seq(80L, 40L, by=-10L),
                                         seq(100L, 140L, by=10L))))

  for (k in 0L:maxSteps) {
    ## move up
    cur <- rects[currentIndex, ]
    cur[c("y0", "y1", "y")] <- cur[c("y0", "y1", "y")] + k * cur[c("h")]
    isOverlapped <- .labelOverlap(object, cur,
                                  rects[seq_len(currentIndex - 1L), ])

    if (isOverlapped) {
      for (l in r) {
        ## move in curve
        oldcur <- cur
        cur[c("y0", "y1", "y")] <- cur[c("y0", "y1", "y")] +
                                   sin(l) * cur[c("h")]
        cur[c("x0", "x1", "x")] <- cur[c("x0", "x1", "x")] +
                                   cos(l) * cur[c("w")]
        isOverlapped <- .labelOverlap(object, cur,
                                      rects[seq_len(currentIndex - 1L), ])
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
  rects[currentIndex, ]
}

## .labelOverlap
##  does current rect overlap any other one?
##
## params:
##  object: a single MassPeaks object
##  cur: vector of coordinates (which should test agains 'rects')
##  rects: a matrix of coordinates (created by .textRects)
##
## returns:
##  TRUE/FALSE
##
.labelOverlap <- function(object, cur, rects) {
  x <- cur[c(1L, 3L)]
  y <- cur[c(2L, 4L)]

  rects <- matrix(rects, ncol=8L)

  ## peak overlap?
  peakOverlap <- any(x[1L] <= object@mass & x[2L] >= object@mass &
                     y[1L] <= object@intensity)

  if (peakOverlap) {
    return(TRUE)
  }

  ## text overlap?
  textOverlap <- any(((x[1L] > rects[, 1L] & x[1L] < rects[, 3L]) |
                      (x[2L] > rects[, 1L] & x[2L] < rects[, 3L]) |
                      (x[1L] < rects[, 1L] & x[2L] > rects[, 3L])) &
                     ((y[1L] > rects[, 2L] & y[1L] < rects[, 4L]) |
                      (y[2L] > rects[, 2L] & y[2L] < rects[, 4L]) |
                      (y[1L] < rects[, 2L] & y[2L] > rects[, 4L])))

  textOverlap
}
