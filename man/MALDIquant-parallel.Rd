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
  \code{\link[MALDIquant]{detectPeaks}}) benefits from parallelisation.
}
\details{
\describe{
  The following functions/methods support the \code{mc.cores} argument:
  \itemize{
    \item{\code{\link[MALDIquant]{trim,list,numeric-method}}}
    \item{\code{\link[MALDIquant]{transformIntensity,list-method}}}
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
  print(all.equal(b1, b2))
}
}
\keyword{misc}
