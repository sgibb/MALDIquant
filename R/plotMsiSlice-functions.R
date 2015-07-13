## Copyright 2014-2015 Sebastian Gibb
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

.plotMsiSlice <- function(x, center=attr(x, "center"),
                          tolerance=attr(x, "tolerance"),
                          colRampList=list(colorRamp(c("black", "blue", "green",
                                                       "yellow", "red"))),
                          xlab="", ylab="", interpolate=FALSE, scale=TRUE,
                          legend=scale, alignLabels=FALSE, label.cex=0.75,
                          label.col=NULL, ...) {
  stopifnot(is.array(x))

  d <- dim(x)
  tolerance <- rep_len(tolerance, length(center))

  xlim <- c(0L, d[1L] + (2L * d[3L] * legend))
  ylim <- c(0L, d[2L])

  ## prepare plot area
  plot(NA, type="n", xlim=xlim, ylim=ylim,
       axes=FALSE, xlab=xlab, ylab=ylab, asp=1L, ...)

  if (d[3L] > 1L) {
    col <- x

    for (i in seq_len(d[3L])) {
      col[,, i] <- .colorMatrix(.array2matrix(x, z=i), colRamp=colRampList[[i]],
                                scale=scale)
    }

    x <- .combineColorMatrices(x, col)
  } else {
    x <- .colorMatrix(.array2matrix(x), colRamp=colRampList[[1L]], scale=scale)
  }

  ## plot image
  .rasterSlice(x, interpolate=interpolate)

  if (legend) {

    if (!is.null(center)) {
      labels <- .mapply(function(cnt, tol)bquote(.(cnt) %+-% .(tol)),
                        cnt=center, tol=tolerance)
      strh <- max(strheight(labels, cex=label.cex)) * 1.2
    } else {
      labels <- character(d[3L])
      strh <- 0L
    }

    xleft <- xlim[2L] - seq(from=d[3L] * 2L - 1L, to=1L, by=-2L)
    xright <- xleft + 1L
    ybottom <- rep.int(d[3L] * strh, d[3L])
    ytext <- (d[3L] - 1L):0L * strh

    xtext <- if (alignLabels) {
      rep.int(xlim[2L], 3L)
    } else {
      xright
    }

    if (is.null(label.col) && d[3L] == 1L) {
      label.col <- "black"
    } else if (is.null(label.col) && d[3L] > 1L) {
      label.col <- lapply(colRampList, function(x).rgb(x(1L)))
    }

    for (i in seq_len(d[3L])) {
      .msiLegend(xleft=xleft[i], xright=xright[i],
                 ybottom=ybottom[1L], ytop=ylim[2L],
                 colRamp=colRampList[[i]], interpolate=interpolate)
      text(x=xtext[i], y=ytext[i], labels=as.expression(labels[i]),
           col=label.col[[i]], cex=label.cex, adj=c(1L, 0L))
    }
  }
}

.rasterSlice <- function(x, interpolate=FALSE) {
  rasterImage(as.raster(t(x)),
              xleft=0L, xright=nrow(x), ybottom=0L, ytop=ncol(x),
              interpolate=interpolate)
}

.array2matrix <- function(a, z=1L) {
## subset function that preserves a matrix even if x or y 1
## ([,,drop=TRUE]) creates a vector
  d <- dim(a)
  matrix(a[,, z, drop=TRUE], nrow=d[1L], ncol=d[2L])
}

.msiLegend <- function(xleft, xright, ybottom, ytop,
                       colRamp=colorRamp(c("black", "blue", "green", "yellow",
                                           "red")), interpolate=FALSE) {
  gradient <- matrix(.rgb(colRamp(seq.int(1L, 0L, length.out=100L))),
                     ncol=1L)
  rect(xleft=xleft, xright=xright, ybottom=ybottom, ytop=ytop,
       col="black")
  rasterImage(as.raster(gradient),
              xleft=xleft, xright=xright, ybottom=ybottom, ytop=ytop,
              interpolate=interpolate)
}

.colorMatrix <- function(x, colRamp, scale=TRUE) {
  if (scale) {
    x <- x / max(x, na.rm=TRUE)
  }

  notNA <- which(!is.na(x))
  x[notNA] <- .rgb(colRamp(x[notNA]))
  x
}

.combineColorMatrices <- function(x, col) {
  i <- apply(x, 2L, max.col, ties.method="first")
  j <- cbind(x=rep.int(seq_len(nrow(x)), ncol(x)),
             y=rep(seq_len(ncol(x)), each=nrow(x)),
             z=as.vector(i))
  y <- .array2matrix(col)
  y[] <- col[j]
  y
}

.rgb <- function(x) {
  rgb(x, maxColorValue=255L)
}
