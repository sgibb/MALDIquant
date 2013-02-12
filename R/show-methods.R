## Copyright 2011-2013 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This file is part of MALDIquant for R and related languages.
##
## MALDIquant is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## MALDIquant is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with MALDIquant. If not, see <http://www.gnu.org/licenses/>

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

  cat(paste(l$groups, ": ", l$values, sep="", collapse="\n"), sep="\n")
})

setMethod(f=".prepareShow",
          signature=signature(object="AbstractMassObject"),
          definition=function(object) {

  groups <- c("S4 class type",
              "Number of m/z values",
              "Range of m/z values",
              "Range of intensity values")

  values <- class(object)[1]

  if (isEmpty(object)) {
    values <- c(values, 0, NA, NA)
  } else {
    values <- c(values,
                length(object@mass),
                paste(round(range(object@mass), digits=3), collapse=" - "),
                paste(format(min(object@intensity), digits=4,
                             scientific=TRUE), " - ",
                      format(max(object@intensity), digits=4,
                             scientific=TRUE), sep=""))
  }

  groups <- c(groups, .prepareShowGroupName(object@metaData$name, "Name"))
  values <- c(values, object@metaData$name)

  groups <- c(groups, .prepareShowGroupName(object@metaData$file, "File"))
  values <- c(values, object@metaData$file)

  return(list(groups=groups, values=values))
})

setMethod(f=".prepareShow",
          signature=signature(object="MassPeaks"),
          definition=function(object) {

  l <- callNextMethod(object)

  groups <- "Range of snr values"

  if (isEmpty(object)) {
    values <- NA
  } else {
    values <- paste(round(range(object@snr), digits=3), collapse=" - ")
  }

  ## append snr info after intensity
  l$groups <- append(l$groups, groups, after=4)
  l$values <- append(l$values, values, after=4)

  return(list(groups=l$groups, values=l$values))
})
