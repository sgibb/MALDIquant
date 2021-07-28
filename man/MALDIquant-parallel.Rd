\name{MALDIquant-parallel}
\alias{MALDIquant-parallel}
\title{Parallel Support in Package \pkg{MALDIquant}}
\description{
  \code{\link{MALDIquant}} offers multi-core support using
  \code{\link[parallel]{mclapply}} and \code{\link[parallel]{mcmapply}}. This
  approach is limited to unix-based platforms.

  Please note that not all functions benfit from parallelisation. Often the
  overhead to create/copy objects outrun the time saving of parallel runs. This
  is true for functions that are very fast to compute (e.g.
  \code{sqrt}-transformation). That's why the default value for the
  \code{mc.cores} argument in all functions is \code{1L}.
  It depends on the size of the dataset which step (often only
  \code{\link[MALDIquant]{removeBaseline}} and
  \code{\link[MALDIquant]{detectPeaks}}) benefits from parallelisation. \cr
  In general it is faster to encapsulate the complete workflow into a function
  and parallelise it using \code{\link[parallel]{mclapply}} instead of using the
  \code{mc.cores} argument of each method. The reason is the reduced overhead
  for object management (only one split/combine is needed instead of doing these
  operations in each function again and again).
}
\details{
\describe{
  The following functions/methods support the \code{mc.cores} argument:
  \itemize{
    \item{\code{\link[MALDIquant]{trim,list,numeric-method}}}
    \item{\code{\link[MALDIquant]{transformIntensity,list-method}}}
    \item{\code{\link[MALDIquant]{smoothIntensity,list-method}}}
    \item{\code{\link[MALDIquant]{removeBaseline,list-method}}}
    \item{\code{\link[MALDIquant]{calibrateIntensity,list-method}}}
    \item{\code{\link[MALDIquant]{detectPeaks,list-method}}}
    \item{\code{\link[MALDIquant]{alignSpectra}}}
    \item{\code{\link[MALDIquant]{averageMassSpectra}}}
    \item{\code{\link[MALDIquant]{mergeMassPeaks}}}
}
}
}
\seealso{
  \code{\link[parallel]{mclapply}},
  \code{\link[parallel]{mcmapply}}
}
\examples{
\dontrun{
## load package
library("MALDIquant")

## load example data
data("fiedler2009subset", package="MALDIquant")

## run single-core baseline correction
print(system.time(
  b1 <- removeBaseline(fiedler2009subset, method="SNIP")
))

if(.Platform$OS.type == "unix") {
  ## run multi-core baseline correction
  print(system.time(
    b2 <- removeBaseline(fiedler2009subset, method="SNIP", mc.cores=2)
  ))
  stopifnot(all.equal(b1, b2))
}

## parallelise complete workflow
workflow <- function(spectra, cores) {
  s <- transformIntensity(spectra, method="sqrt", mc.cores=cores)
  s <- smoothIntensity(s, method="SavitzkyGolay", halfWindowSize=10,
                       mc.cores=cores)
  s <- removeBaseline(s, method="SNIP", iterations=100, mc.cores=cores)
  s <- calibrateIntensity(s, method="TIC", mc.cores=cores)
  detectPeaks(s, method="MAD", halfWindowSize=20, SNR=2, mc.cores=cores)
}

if(.Platform$OS.type == "unix") {
  ## parallelise the complete workflow is often faster because the overhead is
  ## reduced
  print(system.time(
    p1 <- unlist(parallel::mclapply(fiedler2009subset,
                                    function(x)workflow(list(x), cores=1),
                                    mc.cores=2), use.names=FALSE)
  ))
  print(system.time(
    p2 <- workflow(fiedler2009subset, cores=2)
  ))
  stopifnot(all.equal(p1, p2))
}
}
}
\keyword{misc}
