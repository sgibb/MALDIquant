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
setMethod(f="mass",
          signature=signature(object="AbstractMassObject"),
          definition=function(object, ...) {

  object@mass
})

## AbstractMassObject
setReplaceMethod(f="mass",
          signature=signature(object="AbstractMassObject", value="numeric"),
          definition=function(object, value) {

  if (length(object@mass) == length(value)) {
    object@mass <- as.double(value)
  } else {
    stop("Lengths of mass (", length(object@mass), ") and value (",
         length(value), ") have to be equal.")
  }
  object
})
