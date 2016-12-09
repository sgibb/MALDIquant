## .deprecated
##  mark a function as deprecated
##  throw an error if current major version or minor is larger than major, or
##  minor+1; if minor == minor+1L wrote warning; otherwise message
##
## params:
##  version: last working version
##  ...: arguments to be passed to stop, warning, message
##
## returns:
##  nothing
##
.deprecated <- function(version, ...) {
  current <- packageVersion("MALDIquant")
  version <- as.package_version(version)

  if (current$major > version$major || current$minor > version$minor + 1L) {
    stop(..., call.=FALSE)
  } else if (current$minor > version$minor) {
    warning(..., call.=FALSE)
  } else {
    message(...)
  }

  invisible()
}

## .deprecatedFunction
##  mark a function as deprecated and show default message
##
## params:
##  version: last working version
##  old: old function
##  new: new function
##
## returns:
##  nothing
##
.deprecatedFunction <- function(version, old, new) {
  if (missing(old)) {
    old <- sys.call(-1L)[[1]]
  }

  msg <- paste0("\"", old , "\" is deprecated.")

  if (!missing(new)) {
    msg <- paste0(msg, "\nUse \"", new, "\" instead. See help(\"", new ,"\").")
  }
  .deprecated(version, msg)
}

## .deprecatedArgument
##  mark an function as deprecated and show default message
##
## params:
##  version: last working version
##  old: old argument
##  new: new argument
##  help: help file
##
## returns:
##  nothing
##
.deprecatedArgument <- function(version, old, new, help) {
  msg <- paste0("Argument \"", old, "\" is deprecated.")

  if (!missing(new)) {
    if (missing(help)) {
      parentCall <- sys.call(-1L)[[1]]
    } else {
      parentCall <- call(help)[[1]]
    }
    msg <- paste0(msg, "\nUse \"", new, "\" instead. See help(\"",
                  deparse(parentCall),"\").")
  }
  .deprecated(version, msg)
}
