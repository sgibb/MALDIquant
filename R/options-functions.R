#' MALDIquant options
#'
#' MALDIquant defines a few options globally using the standard R
#' options mechanism. The current values of these options can be
#' queried with \code{MALDIquantOptions()}. The options are:
#'
#' \itemize{
#' \item{\code{allowNegativeIntensities}: flag, that controls whether negative
#' intensities are allowed. Please note that negative intensities could
#' badly influence the results of some algorithms e.g. baseline correction,
#' smoothing, \dots. The default value is \code{FALSE}}
#' }
#'
#' @return A \code{list} of MALDIquant options and the single option
#' values for the individual accessors.
#' @examples
#' # save old options
#' opt <- options()
#'
#' # set MALDIquant options
#' options(MALDIquant=list(allowNegativeIntensities=FALSE))
#'
#' MALDIquantOptions()
#'
#' # reset old options
#' options(opt)
#' @rdname options-functions
MALDIquantOptions <- function() {
    getOption(
        "MALDIquant",
        list(allowNegativeIntensities=FALSE)
    )
}

#' @rdname options-functions
.negativeIntensitiesAllowed <- function() {
    if (!isTRUE(MALDIquantOptions()$allowNegativeIntensities))
        FALSE
    else
        TRUE
}
