\encoding{utf8}
\name{fiedler2009subset}
\docType{data}
\alias{fiedler2009subset}
\title{Example Mass Spectra (raw)}
\description{
This dataset contains 16 example mass spectra.
It is used to demonstrate the usage of \code{\link{MALDIquant-package}}.
}
\usage{data(fiedler2009subset)}
\format{
A list containing 16 \code{\link[MALDIquant]{MassSpectrum-class}}
objects.}
\details{
The dataset is a subset of data used in \emph{Fiedler et al 2009}. \cr
It contains spectra of 8 different patients (each one has 2 technical
replicates).
\tabular{rlllll}{
list_index  \tab laboratory \tab patient_id \tab sex    \tab age \tab type   \cr
1           \tab Leipzig    \tab LC77       \tab male   \tab 37  \tab control\cr
2           \tab Leipzig    \tab LC77       \tab male   \tab 37  \tab control\cr
3           \tab Leipzig    \tab LC213      \tab female \tab 51  \tab control\cr
4           \tab Leipzig    \tab LC213      \tab female \tab 51  \tab control\cr
5           \tab Leipzig    \tab LT178      \tab male   \tab 58  \tab cancer \cr
6           \tab Leipzig    \tab LT178      \tab male   \tab 58  \tab cancer \cr
7           \tab Leipzig    \tab LT157      \tab male   \tab 60  \tab cancer \cr
8           \tab Leipzig    \tab LT157      \tab male   \tab 60  \tab cancer \cr
9           \tab Heidelberg \tab HC49       \tab male   \tab 43  \tab control\cr
10          \tab Heidelberg \tab HC49       \tab male   \tab 43  \tab control\cr
11          \tab Heidelberg \tab HC54       \tab female \tab 71  \tab control\cr
12          \tab Heidelberg \tab HC54       \tab female \tab 71  \tab control\cr
13          \tab Heidelberg \tab HT151      \tab male   \tab 53  \tab cancer \cr
14          \tab Heidelberg \tab HT151      \tab male   \tab 53  \tab cancer \cr
15          \tab Heidelberg \tab HT429      \tab female \tab 58  \tab cancer \cr
16          \tab Heidelberg \tab HT429      \tab female \tab 58  \tab cancer \cr
}
}
\references{
G.M. Fiedler, A.B. Leichtle, J. Kase, S. Baumann, U. Ceglarek, K. Felix,
T. Conrad, H. Witzigmann, A. Weimann, C. Schütte, J. Hauss, M. Büchler
and J. Thiery
\cr
\dQuote{Serum Peptidome Profiling Revealed Platelet Factor 4 as a Potential
Discriminating Peptide Associated with Pancreatic Cancer} \cr
Clinical Cancer Research, 11(15): 3812-3819, 2009 \cr
ISSN 1557-3265; \doi{10.1158/1078-0432.CCR-08-2701}
}
\seealso{
\code{\link[MALDIquant]{MassSpectrum-class}}

Website: \url{https://strimmerlab.github.io/software/maldiquant/}
}
\keyword{datasets}
