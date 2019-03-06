#' @include hidden_aliases.R
NULL

#' @title OnDiskVector class
#'
#' @name OnDiskVector
#'
#' @aliases OnDiskVector-class
#'
#' @description
#'
#' [OnDiskVector-class] objects support the storage of numeric data on-disk. The
#' data are just loaded into memory when they have to be processed.
#'
#' @slot path file path
#' @slot mpath file path to the modification counter file
#' @slot modification counter, to detect modification after `odv2 <- odv`
#' @slot n length of the vector
#' @slot offset offset of the data in the file
#' @slot size size of one vector element in the file
#'
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#'
#' @noRd
setClass("OnDiskVector",
    slots=list(
        path="character",
        mpath="character",
        modification="integer",
        n="numeric",
        offset="numeric",
        size="integer"
    ),
    prototype=list(
        path=character(),
        mpath=character(),
        modification=0L,
        n=numeric(),
        offset=numeric(),
        size=integer()
    )
)

OnDiskVector <- function(x, path, n=length(x), offset=0L, size=8L) {
    if (missing(x) && missing(path))
        stop("'x' or 'path' are necessary.")
    if (!missing(x)) {
        if (missing(path))
            path <- tempfile()
        writeBin(as.double(x), con=path, size=size, endian="little")
    }
    mpath <- paste(path, "mod", sep=".")
    writeBin(0L, mpath, size=NA_integer_, endian="little")
    new("OnDiskVector", path=path, mpath=mpath, n=n, offset=offset, size=size)
}

.valid.OnDiskVector.path <- function(x) {
    if (length(x) != 1L)
        return("'path' has to be a 'character' of length 1.")
    if (!file.exists(x))
        return(paste0("File '", x, "' doesn't exists!"))
    NULL
}

.valid.OnDiskVector.modification <- function(x) {
    if (length(x) != 1L)
        return("'modification' has to be a 'numeric' of length 1.")
    NULL
}

.valid.OnDiskVector.n <- function(x) {
    if (length(x) != 1L)
        return("'n' has to be a 'numeric' of length 1.")
    NULL
}

.valid.OnDiskVector.offset <- function(x) {
    if (length(x) != 1L)
        return("'offset' has to be a 'numeric' of length 1.")
    if (x < 0)
        return("'offset' has to be >= 0.")
    NULL
}

.valid.OnDiskVector.size <- function(x) {
    if (length(x) != 1L)
        return("'size' has to be a 'integer' of length 1.")
    if (log2(x) %% 1)
        return("'size' has to be 2^x.")
    NULL
}

.isModified.OnDiskVector <- function(x) {
    m <- readBin(x@mpath, integer(), n=1L, size=NA_integer_, endian="little")
    if (m != x@modification)
        stop(x@path, " was modified by a different object.")
    FALSE
}

setValidity("OnDiskVector", function(object) {
    msg <- c(
        .valid.OnDiskVector.path(object@path),
        .valid.OnDiskVector.path(object@mpath),
        .valid.OnDiskVector.modification(object@n),
        .valid.OnDiskVector.n(object@n),
        .valid.OnDiskVector.offset(object@offset),
        .valid.OnDiskVector.size(object@size)
    )
    if (is.null(msg)) { TRUE } else { msg }
})

#' @rdname hidden_aliases
setMethod("length", "OnDiskVector", function(x)x@n)

#' @rdname hidden_aliases
setMethod(f="[",
    signature=signature(x="OnDiskVector", i="numeric", j="missing"),
    definition=function(x, i, j, ..., drop=FALSE) {
    if (any(i < 1) || any (i > x@n))
        stop("Index out of boundaries.")

    .isModified.OnDiskVector(x)
    f <- file(x@path, "rb")
    on.exit(close(f))

    if (length(i) == 1L) {
        if (x@offset || i > 1L)
            seek(f, where=x@offset + (i - 1L) * x@size, rw="read")
        .readBin(f, n=1L, size=x@size)
    } else if (length(i) == 2) {
        if (x@offset || i[1L] > 1L)
            seek(f, where=x@offset + (i[1L] - 1L) * x@size, rw="read")
        y <- .readBin(f, n=1L, size=x@size)
            seek(f, where=x@offset + (i[2L] - 1L) * x@size, rw="read")
        c(y, .readBin(f, n=1L, size=x@size))
    } else {
        if (x@offset)
            seek(f, where=x@offset, rw="read")
        # that's stupid but not used often
        .readBin(f, n=x@n, size=x@size)[i]
    }
})

#' @rdname hidden_aliases
setMethod(f="[",
    signature=signature(x="OnDiskVector", i="missing", j="missing"),
    definition=function(x, i, j, ..., drop=FALSE) {
    .isModified.OnDiskVector(x)
    f <- file(x@path, "rb")
    on.exit(close(f))
    if (x@offset)
        seek(f, where=x@offset, rw="read")
    .readBin(f, n=x@n, size=x@size)
})

#' @rdname hidden_aliases
setReplaceMethod(f="[",
    signature=signature(x="OnDiskVector", i="missing", j="missing"),
    definition=function(x, i, j, ..., value) {
    if (length(value) != x@n) {
        stop("Length of 'value' doesn't match length of 'x'.")
    }
    writeBin(as.double(value), x@path, size=x@size, endian="little")
    x@modification <- x@modification + 1L
    writeBin(x@modification, x@mpath, size=NA_integer_, endian="little")
    x
})

.readBin <- function(x, n, size) {
    readBin(x, double(), n=n, size=size, signed=TRUE, endian="little")
}
