\name{OnDiskVector-class}
\Rdversion{1.1}
\docType{class}
\alias{OnDiskVector}


\title{Class "OnDiskVector"}
\description{
\code{\linkS4class{AbstractMassObject}} is an internal class for creating and attaching to on-disk vectors. It is used within the \code{mass} and \code{intensity} slots of \code{\linkS4class{MassSpectrumOnDisk}} objects.
It shouldn't create or handle by the user because it is for internal use only.
}

\section{Slots}{
\describe{
  \item{\code{path}:}{\code{character}, path to the on-disk vector}
  \item{\code{mpath}:}{\code{character}, path to the modification counter file}
  \item{\code{modification}:}{\code{integer}, file modification counter}
  \item{\code{n}:}{\code{integer}, length of the vector}
  \item{\code{offset}:}{\code{integer}, offset of the data in the file}
  \item{\code{size}:}{\code{integer}, size of one vector element in the file}
}
}




\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}

\seealso{
\code{\linkS4class{MassSpectrumOnDisk}}}


\examples{
## load package
library("MALDIquant")

## create example vector
odv <- OnDiskVector(1:10)

## query path
odv@path
# [1] "/tmp/Rtmpaz3SQG/file13ce3a31c136"

## get stored values
odv[1:5]
# [1] 1 2 3 4 5
}
\keyword{classes}

