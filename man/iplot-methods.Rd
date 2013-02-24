\name{iplot-methods}
\docType{methods}
\alias{iplot}
\alias{iplot,MassSpectrum-method}
\alias{iplot,list-method}
\title{Interactive plot of MassSpectrum objects.}
\description{
This method provides an interactive plot to investigate
\code{\linkS4class{MassSpectrum}} and \code{\linkS4class{MassPeaks}} objects
more easily.
}
\usage{
\S4method{iplot}{MassSpectrum}(object, peaks, \dots)
}
\arguments{
  \item{object}{\code{\linkS4class{MassSpectrum}} object or a \code{list} of
    \code{\linkS4class{MassSpectrum}} objects.}
  \item{peaks}{\code{\linkS4class{MassPeaks}} object or a \code{list} of
    \code{\linkS4class{MassPeaks}} objects.}
  \item{\dots}{arguments to be passed to \code{\link[graphics]{plot}}}
}
\details{
  \code{iplot} depends on \code{\link[grDevices]{getGraphicsEvent}} which
  currently supports only a few interactive graphic devices
  (only the \code{windows()} (Windows) and the \code{X11(type="Xlib")} (Unix)
  devices).

  \code{iplot} supports the following keyboard shortcuts:
  \describe{
    \item{\sQuote{q}, \sQuote{ESC}, \sQuote{Ctrl-C}:}{Close \code{iplot}.}
    \item{\sQuote{o}, \sQuote{Up Arrow}, \sQuote{+}, \sQuote{Ctrl-+}:}{Zoom in
      (mass/x-axis).}
    \item{\sQuote{O}, \sQuote{Down Arrow}, \sQuote{-}, \sQuote{Ctrl--}:}{Zoom out
      (mass/x-axis).}
    \item{\sQuote{i}:}{Zoom in (intensity/y-axis).}
    \item{\sQuote{I}:}{Zoom out (intensity/y-axis).}
    \item{\sQuote{h}, \sQuote{Left Arrow}:}{Move to the left.}
    \item{\sQuote{l}, \sQuote{Right Arrow}:}{Move to the right.}
    \item{\sQuote{k}, \sQuote{Page Up}:}{Switch to the previous spectrum.}
    \item{\sQuote{j}, \sQuote{Page Down}:}{Switch to the next spectrum.}
    \item{\sQuote{g}, \sQuote{Home}:}{Switch to the first spectrum.}
    \item{\sQuote{G}, \sQuote{End}:}{Switch to the last spectrum.}
    \item{\sQuote{s}:}{Toggle spectrum style (\code{c("l", "p", "b", "n")},
      see \code{\link[graphics]{plot}}: \sQuote{type})}
    \item{\sQuote{d}:}{Toggle peaks style (\code{c("h", "p", "b", "n")},
      see \code{\link[graphics]{plot}}: \sQuote{type})}
    \item{\sQuote{a}:}{Toggle peaks labels on/off (see
      \code{\link[MALDIquant]{labelPeaks,MassPeaks-method}})}
    \item{\sQuote{p}:}{Print xlim/ylim.}
    \item{\sQuote{r}:}{Reset xlim/ylim.}
  }
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\linkS4class{MassPeaks}},
\code{\linkS4class{MassSpectrum}},
\code{\link[MALDIquant]{plot,MassSpectrum,missing-method}},
\code{\link[MALDIquant]{labelPeaks,MassPeaks-method}},
\code{\link[grDevices]{getGraphicsEvent}}
}
\examples{
## load library
library("MALDIquant")

## load example data
data("fiedler2009subset", package="MALDIquant")

## run a typical workflow

## transform intensities
s <- transformIntensity(fiedler2009subset, sqrt)

## smooth spectra
s <- transformIntensity(s, movingAverage)

## remove baseline
s <- removeBaseline(s)

## detect peaks
p <- detectPeaks(s)

\dontrun{
## create an interactive plot for the first spectrum
iplot(s[[1]])

## create an interactive plot for the first spectrum
## and its corresponding peaks
iplot(s[[1]], p[[1]])

## create an interactive plot all 16 example spectra
## and their corresponding peaks
iplot(s, p)
}

}
\keyword{methods}
\keyword{Graphics|plot}

