## Copyright 2012-2016 Sebastian Gibb
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

  testrects <- rbind(matrix(c(object@mass, rep(0L, length(object)),
                              object@mass, object@intensity), ncol=4L),
                       rects[seq_len(currentIndex - 1L),
                             c("x0", "y0", "x1", "y1")])

  for (k in 0L:maxSteps) {
    ## move up
    cur <- rects[currentIndex, ]
    cur[c("y0", "y1", "y")] <- cur[c("y0", "y1", "y")] + k * cur["h"]

    for (l in r) {
        ## move in curve
        oldcur <- cur
        cur[c("y0", "y1", "y")] <- cur[c("y0", "y1", "y")] +
                                   sin(l) * k * cur["h"]
        cur[c("x0", "x1", "x")] <- cur[c("x0", "x1", "x")] +
                                   cos(l) * k * cur["w"]
      ## success
      if (!.overlaps(cur, testrects)) {
        return(cur)
      }
      cur <- oldcur
    }
  }
  ## no success, return original pos
  rects[currentIndex, ]
}

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
.textLabelRects <- function(x, y, text, adj=c(0.5, 0L), cex=0.7,
                            offset=c(0.0, 0.2)) {
  w <- strwidth(text, cex=cex)
  h <- strheight(text, cex=cex)

  ## extra calculation of offsets to be independend of adj
  woffset <- w * offset[1L]
  hoffset <- h * offset[2L]

  matrix(c(x - w * adj[1L] - woffset,
           y - h * adj[2L] - hoffset,
           x + w * (1L - adj[1L]) + woffset,
           y + h * (1L - adj[2L]) + hoffset,
           x, y, w, h),
         ncol = 8L, dimnames = list(c(), c("x0", "y0", "x1", "y1",
                                           "x", "y", "w", "h")))
}

## .overlaps
## does rectangles overlap?
##
## params:
##  a: vector, length 4, c(x0, y0, x1, y1); c(x0, y0) bottom left
##  b: matrix, at least 4 columns, c(x0, y0, x1, y1)
##
## returns:
##  TRUE/FALSE
.overlaps <- function(a, b) {
  if (is.vector(b)) {
    b <- t(b)
  }
  any(# rectangles on left/right of each other?
      !((a[1L] >= b[, 3L] | a[3L] <= b[, 1L]) |
      # rectangles on top/bottom of each other?
      (a[2L] >= b[, 4L] | a[4L] <= b[, 2L])))
}
