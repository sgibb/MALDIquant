## Copyright 2013 Sebastian Gibb
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

.iplot <- function(spectra, peaks, xlim=NULL, ylim=NULL,
                   spectrumStyleType="l", peakStyleType="h",
                   showPeakLabels=FALSE, ...) {
  ## init values
  i <- 1
  n <- length(spectra)

  ## test arguments
  if (missing(peaks)) {
    peaks <- vector(mode="list", length=n)
  }

  if (is.null(xlim)) {
    xlim <- range(unlist(lapply(spectra, function(x)range(mass(x)))))
    xlim <- c(floor(xlim[1]), ceiling(xlim[2]))
  }

  if (is.null(ylim)) {
    ylim <- c(0, max(unlist(lapply(spectra, function(x)max(intensity(x))))))
  }

  backup <- list(xlim=xlim, ylim=ylim)

  ## open interactive device if needed
  if (.openInteractiveDevice()) {
    on.exit(dev.off())
  }

  ## keyboard handler
  .keyboard <- function(key) {
    switch(key,
      ## quit (\033 == ESC)
      "\033" =, "ctrl-C" =, "q" = { return(invisible(1)) },
      ## move horizontal
      "Left" =, "h" = { xlim <<- .moveXlim(-1) },
      "Right" =, "l" = { xlim <<- .moveXlim(1) },
      ## go through spectra list
      "PgUp" =, "k" =  { i <<- ifelse(i <= 1, 1, i-1) },
      "PgDn" =, "j" =  { i <<- ifelse(i >= n, n, i+1) },
      "Home" =, "g" = { i <<- 1 },
      "End" =, "G" = { i <<- n },
      ## zoom (horizontal)
      "ctrl-*" =, "+" =, "Up" =, "o" = { xlim <<- .zoomXlim(0.9) },
      "ctrl-_" =, "-" =, "Down" =,  "O" = { xlim <<- .zoomXlim(1.1) },
      ## zoom (vertical)
      "i" = { ylim <<- c(0, .zoomYlim(0.9)[2]) },
      "I" = { ylim <<- c(0, .zoomYlim(1.1)[2]) },
      ## print limits
      "p" = { message("xlim=c(",
                      paste(round(.xlim(), digits=3), collapse=", "), "), ",
                      "ylim=c(",
                      paste(round(.ylim(), digits=3), collapse=", "), ")") },
      ## reset limits
      "r" = { xlim <<- backup$xlim
              ylim <<- backup$ylim },
      ## spectrum style
      "s" = { types <- c("l", "p", "b", "n")
              cur <- which(spectrumStyleType == types)
              spectrumStyleType <<- types[(cur %% 4)+1] },
      ## peak style
      "d" = { types <- c("h", "p", "b", "n")
              cur <- which(peakStyleType == types)
              peakStyleType <<- types[(cur %% 4)+1] },
      ## show peak labels
      "a" = { showPeakLabels <<- !showPeakLabels },
      {}
    )

    .iplotSingle(spectrum=spectra[[i]], peaks=peaks[[i]],
                 spectrumStyleType=spectrumStyleType,
                 peakStyleType=peakStyleType, showPeakLabels=showPeakLabels,
                 xlim=xlim, ylim=ylim, ...)
    if (n > 1) {
      mtext(paste(i, "/",  n, sep=""), side=3)
    }
    return(NULL)
  }

  .iplotSingle(spectra[[i]], peaks[[i]],
               spectrumStyleType=spectrumStyleType,
               peakStyleType=peakStyleType, showPeakLabels=showPeakLabels,
               xlim=xlim, ylim=ylim, ...)
  if (n > 1) {
    mtext(paste(i, "/",  n, sep=""), side=3)
  }

  grDevices::setGraphicsEventHandlers(onKeybd=.keyboard)
  grDevices::getGraphicsEvent(consolePrompt=.iplotUsage(n > 1,
                                                        !is.null(peaks[[1]])))
}

.iplotSingle <- function(spectrum, peaks,
                         spectrumStyleType="l", peakStyleType="h",
                         showPeakLabels=FALSE, ...) {
  plot(spectrum, type=spectrumStyleType, ...)

  if (!missing(peaks) && !is.null(peaks)) {
    if (peakStyleType == "h" || peakStyleType == "b") {
      lines(peaks, col=2) #, type=peakStyleType) type="b" isn't working h+p
    }

    if (peakStyleType == "p" || peakStyleType == "b") {
      points(peaks, col=2, pch=4)
    }

    if (showPeakLabels) {
      labelPeaks(peaks)
    }
  }
}

## helper functions
.openInteractiveDevice <- function() {
  if (!interactive()) {
    stop("R has to run in interactive mode.")
  }

  if (.Platform$OS.type == "unix") {
    if (names(dev.cur()) != "X11") {
      # Could not use X11(type="Xlib") because R CMD check gives a NOTE
      # about an unused argument (type) on windows.
      do.call(X11, list(type="Xlib"))
      return(TRUE)
    }
  } else if (.Platform$OS.type == "windows") {
    if (names(dev.cur()) != "windows") {
      windows()
      return(TRUE)
    }
  } else {
    stop("Unknown platform.")
  }
  return(FALSE)
}

## limits
.lim <- function(usr) {
  d <- diff(usr)/1.08
  return(usr[1:2]+d*c(0.04, -0.04))
}

.xlim <- function() {
  return(.lim(par("usr")[1:2]))
}

.ylim <- function() {
  return(.lim(par("usr")[3:4]))
}

## move limits
.moveXlim <- function(direction, width=0.1) {
  xlim <- .xlim()
  step <- diff(xlim)*width*direction
  return(xlim+step)
}

## zoom limits
.zoomLim <- function(lim, width) {
  d <- diff(lim)/2*width
  m <- sum(lim)/2
  return(m+d*c(-1, 1))
}

.zoomXlim <- function(width) {
  return(.zoomLim(.xlim(), width))
}

.zoomYlim <- function(width) {
  return(.zoomLim(.ylim(), width))
}

## usage
.iplotUsage <- function(isList=FALSE, hasPeaks=FALSE) {
  keys <- c("q",
            "o/O", "i/I",
            "h/l",
            "k/j", "g/G",
            "s", "d", "a",
            "p", "r")

  text <- c("quit",
            "zoom in/out (x-axis)", "zoom in/out (y-axis)",
            "move left/right (x-axis)",
            "show prev/next spectrum", "show first/last spectrum",
            "toggle spectrum style (l, p, b, n)",
            "toggle peaks style (h, p, b, n)",
            "toggle peak labels on/off",
            "print xlim, ylim", "reset")

  if (!isList) {
    keys[keys %in% c("j/k", "g/G")] <- NA
  }

  if (!hasPeaks) {
    keys[keys %in% c("d", "a")] <- NA
  }

  text <- text[!is.na(keys)]
  keys <- keys[!is.na(keys)]

  msg <- paste("See ", sQuote("help(\"iplot\", \"MALDIquant\")"),
               " for details and more shortcuts.\n", sep="")
  keys <- format(keys, justify="left")
  return(paste(msg, paste(keys, ": ", text, "\n", sep="", collapse=""),
               sep="\n"))
}

