## Copyright 2011-2015 Sebastian Gibb
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

  !sum(as.double(x@intensity), na.rm=TRUE)
})

setMethod(f=".isEmptyWarning",
          signature=signature(x="AbstractMassObject"),
          definition=function(x) {

  if (isEmpty(x)) {
    msg <- paste0(class(x)[1L], " object")

    if (!is.null(x@metaData$file)) {
      msg <- paste0(msg, " (file: ", x@metaData$file, ")")
    }

    parentCall <- sys.call(-1L)
    warning("In ", deparse(parentCall), " : ", msg, " is empty!",
            call.=FALSE)
    return(TRUE)
  }

  FALSE
})
