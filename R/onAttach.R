.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\nThis is MALDIquant version ",
    utils::packageVersion("MALDIquant"), "\n",
    "Quantitative Analysis of Mass Spectrometry Data\n",
    " See ", sQuote("?MALDIquant"),
    " for more information about this package.\n")
}
