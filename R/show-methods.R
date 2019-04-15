## AbstractMassObject
setMethod(f="show",
          signature=signature(object="AbstractMassObject"),
          definition=function(object) {

  l <- .prepareShow(object)

  isFilename <- grepl(pattern="^File.*", x=l$groups)

  ## to avoid newlines in other values don't format filenames
  ## (they could be very long)
  l$values[!isFilename] <- format(l$values[!isFilename], justify="left")

  l$groups <- format(l$groups, justify="left")

  cat(paste0(l$groups, ": ", l$values, collapse="\n"), sep="\n")
})

setMethod(f=".prepareShow",
          signature=signature(object="AbstractMassObject"),
          definition=function(object) {

  groups <- c("S4 class type",
              "Number of m/z values",
              "Range of m/z values",
              "Range of intensity values",
              "Memory usage")

  values <- class(object)[1L]

  if (isEmpty(object)) {
    values <- c(values, 0L, NA, NA)
  } else {
    values <- c(values,
                length(object@mass),
                paste0(round(range(object@mass), digits=3L), collapse=" - "),
                paste0(format(min(object@intensity), digits=4L,
                              scientific=TRUE), " - ",
                       format(max(object@intensity), digits=4L,
                              scientific=TRUE)))
  }
  values <- c(values, .memoryUsageStr(object))

  groups <- c(groups, .prepareShowGroupName(object@metaData$name, "Name"))
  values <- c(values, object@metaData$name)

  groups <- c(groups, .prepareShowGroupName(object@metaData$file, "File"))
  values <- c(values, object@metaData$file)

  list(groups=groups, values=values)
})


setMethod(f=".prepareShow",
          signature=signature(object="MassSpectrumOnDisk"),
          definition=function(object) {
                 
                 l <- callNextMethod(object)
                 
                 
                 
                 if (isEmpty(object)) {
                        values <- NA
                 } else {
                        values <- c(object@mass@path, 
                                    object@intensity@path)
                 }
                 
                 
                 groups <- c("Path to on-disk mass values",
                             "Path to on-disk intensities")
                 
                 
                 ## append path info
                 l$groups <- append(l$groups, groups)
                 l$values <- append(l$values, values)
                 
                 list(groups=l$groups, values=l$values)
          })


setMethod(f=".prepareShow",
          signature=signature(object="MassPeaks"),
          definition=function(object) {

  l <- callNextMethod(object)

  groups <- "Range of snr values"

  if (isEmpty(object)) {
    values <- NA
  } else {
    values <- paste0(round(range(object@snr), digits=3), collapse=" - ")
  }

  ## append snr info after intensity
  l$groups <- append(l$groups, groups, after=4L)
  l$values <- append(l$values, values, after=4L)

  list(groups=l$groups, values=l$values)
})

## OnDiskVector
setMethod(f="show",
          signature=signature(object="OnDiskVector"),
          definition=function(object) {
                 
                 #l <- .prepareShow(object)
                 
                 groups <- c("S4 class type",
                             "Vector length",
                             "Vector range",
                             "Vector file path",
                             "Modification counter path",
                             "Memory usage on-disk",
                             "Memory usage in-memory")
                 
                 
                 
                 values <- c(class(object)[1L],
                             length(object),
                             paste0(format(min(object), digits=4L,
                                           scientific=TRUE), " - ",
                                    format(max(object), digits=4L,
                                           scientific=TRUE)),
                             object@path,
                             object@mpath,
                             .memoryUsageStr(object@path, FALSE),
                             .memoryUsageStr(object, TRUE))
                 
                 l <- list(groups=groups, values=values)
                 
                 isFilename <- grepl(pattern="^File.*|^path.*", x=l$groups)
                 
                 ## to avoid newlines in other values don't format filenames
                 ## (they could be very long)
                 l$values[!isFilename] <- format(l$values[!isFilename], justify="left")
                 
                 l$groups <- format(l$groups, justify="left")
                 
                 cat(paste0(l$groups, ": ", l$values, collapse="\n"), sep="\n")
          })

