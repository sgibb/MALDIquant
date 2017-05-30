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
##  srt: rotation in degree
##  maxSteps: max tries to avoid collisons
##
## returns:
##  a matrix of coordinates
##
.calculateLabelPositions <- function(object, x, y, peakLabels, adj, cex, srt,
                                     maxSteps=100L) {
  ## start with smallest peak
  i <- sort.int(y, index.return=TRUE)$ix

  ## calculate label rectangles
  rects <- .textLabelRects(x[i], y[i], peakLabels[i], adj=adj, cex=cex, srt=srt)

  ## move rectangles around to avoid collisons
  for (j in seq_along(x)) {
    rects[j, ] <- .testLabelOverlap(object, rects, currentIndex=j,
                                    maxSteps=maxSteps)
  }

  ## undo sorting
  rects[i, ] <- rects

  rects
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
    s <- k / 4L
    cur <- rects[currentIndex, ]
    cur[c("y0", "y1", "y")] <- cur[c("y0", "y1", "y")] + s * cur["h"]

    for (l in r) {
        ## move in curve
        oldcur <- cur
        cur[c("y0", "y1", "y")] <- cur[c("y0", "y1", "y")] +
                                   sin(l) * s * cur["h"]
        cur[c("x0", "x1", "x")] <- cur[c("x0", "x1", "x")] +
                                   cos(l) * s * cur["w"]
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
##  srt: rotation in degree
##  offset: offset
##
## returns:
##  a matrix of coordinates
##
.textLabelRects <- function(x, y, text, adj=c(0.5, 0L), cex=0.7, srt=0,
                            offset=c(0.0, 0.2)) {
  wh <- .strWH(text, srt=srt, cex=cex)
  ## extra calculation of offsets to be independend of adj
  offset <- t(t(wh) * offset)

  m <- matrix(c(x - wh[, "w"] * adj[1L] - offset[, "w"],
                y - wh[, "h"] * adj[2L] - offset[, "h"],
                x + wh[, "w"] * (1L - adj[1L]) + offset[, "w"],
                y + wh[, "h"] * (1L - adj[2L]) + offset[, "h"],
                x, y, wh[, "w"], wh[, "h"]),
              ncol = 8L, dimnames = list(c(), c("x0", "y0", "x1", "y1",
                                                "x", "y", "w", "h")))
  if (srt) {
    m[, c("x0", "x1", "y0", "y1")] <-
      .rotate(rbind(m[, c("x0", "y0")], m[, c("x1", "y1")]),
              center=m[, c("x", "y")], srt=srt)

    if (m[1L, "x0"] > m[1L,  "x1"]) {
      m[, c("x1", "x0")] <- m[, c("x0", "x1")]
    }
    if (m[1L, "y0"] > m[1L,  "y1"]) {
      m[, c("y1", "y0")] <- m[, c("y0", "y1")]
    }
    m[, c("w", "h")] <- m[, c("x1", "y1")] - m[, c("x0", "y0")]
  }
  m
}

## .overlaps
## does rectangles overlap?
##
## HINT: doesn't work for rotated rectangles (!= 0, 90, 180, 270, 360)
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

## .scaleFactor
## get height to width scale factor of a graphic device
##
## returns:
##  double, scale factor
##
.scaleFactor <- function() {
  usr <- par("usr")
  pin <- par("pin")

  dx <- usr[2L] - usr[1L]
  dy <- usr[4L] - usr[3L]

  (dy * pin[1L]) / (dx * pin[2L])
}

## .rotate
## rotate rectangle
##
## params:
##  x: matrix, 2 columns, x,y coordinates
##  center: matrix, 2 columns, rotation center x,y coordinate
##  srt: rotation in degree
## returns:
##  double, length 2, x, y, rotated
##
.rotate <- function(x, center, srt) {
  stopifnot(all(dim(x)) == all(dim(center)))
  a <- pi * srt / 180L
  cosa <- cos(a)
  sina <- sin(a)

  dx <- x[, 1L] - center[, 1L]
  dy <- x[, 2L] - center[, 2L]

  x[, 1L] <- center[, 1L] + dx * cosa - dy * sina
  x[, 2L] <- center[, 2L] + dx * sina + dy * cosa

  x
}

## .strWH
## string width and height scaled to device scale and rotatet
##
## TODO: doesn't work for srt != 0, 90, 180, ... (.overlaps doesn't work for
## rotated rectangles)
##
## params:
##  text: text
##  cex: character scaling factor (see ?par)
##  srt: rotation in degree
##  scale: scale factor
##
## returns:
##  double, length 2, w, h
##
.strWH <- function(text, srt, cex=0.7, scale=.scaleFactor()) {
  a <- pi * srt / 180L

  scale <- abs(cos(a) + sin(a) * scale)

  matrix(c(strwidth(text, cex=cex) * scale,
           strheight(text, cex=cex) / scale), ncol = 2L,
         dimnames = list(c(), c("w", "h")))
}
