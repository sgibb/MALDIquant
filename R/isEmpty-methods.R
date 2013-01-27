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
setMethod(f="isEmpty",
          signature=signature(x="AbstractMassObject"),
          definition=function(x) {

  return(length(x@intensity) == 0)
})

setMethod(f=".isEmptyWarning",
          signature=signature(x="AbstractMassObject"),
          definition=function(x) {

  if (isEmpty(x)) {
    msg <- paste(class(x)[1], " object", sep="")

    if (!is.null(x@metaData$file)) {
      msg <- paste(msg, " (file: ", x@metaData$file, ")", sep="")
    }

    parentCall <- sys.call(-1)
    warning(paste("In ", deparse(parentCall), " : ", msg, " is empty!",
                  sep=""), call.=FALSE)
    return(TRUE)
  }

  return(FALSE)
})

