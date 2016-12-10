setMethod(f="labelPeaks",
          signature=signature(object="MassPeaks"),
          definition=function(object, index, mass, labels, digits=3L,
                              underline=TRUE,
                              ## verticalOffset ca. 0.01 of plot height
                              verticalOffset=abs(diff(par("usr")[3L:4L]))*0.01,
                              absoluteVerticalPos, adj=c(0.5, 0L), cex=0.7,
                              srt=0L, avoidOverlap=FALSE,
                              arrowLength=0L, arrowLwd=0.5, arrowCol=1L, ...) {

  ## index
  if (missing(index) && missing(mass)) {
    index <- seq_along(object@mass)
  } else if (!missing(index) && is.logical(index)) {
    index <- which(index)
  }

  if (!missing(mass) && is.numeric(mass)) {
    massIdx <- match.closest(mass, object@mass)

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
    if (srt %% 90L != 0L) {
      stop(sQuote("avoidOverlap = TRUE"), " and ", sQuote("srt != x * 90"),
           " is not supported.")
    }

    ## inspired by Ian Fellows' wordcloud::wordlayout
    p <- .calculateLabelPositions(object, x, y, labels, adj=adj, cex=cex,
                                  srt=srt)

    ## create arrows from label to peak
    arrows(x0=p[, "x"], y0=p[, "y"], x1=x, y1=y, col=arrowCol,
           length=arrowLength, lwd=arrowLwd)
    ## no transparent background
    rect(xleft=p[, "x0"], ybottom=p[, "y0"], xright=p[, "x1"], ytop=p[, "y1"],
         col="white", border=NA, density=-1L)
    x <- p[, "x"]
    y <- p[, "y"]
  }
  text(x=x, y=y, labels=labels, adj=adj, cex=cex, srt=srt, ...)
})
