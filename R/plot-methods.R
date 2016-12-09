## AbstractMassObject
setMethod(f="plot",
          signature=signature(x="AbstractMassObject", y="missing"),
          definition=function(x, col="black",
                              xlab=expression(italic(m/z)), ylab="intensity",
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
