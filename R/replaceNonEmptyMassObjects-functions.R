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

## .replaceNonEmptyMassObjects
##  find and replace non-empty AbstractMassObject objects in a MassObjectsList
##
## params:
##  l: list of AbstractMassObject objects
##  replaceByList: list with elements to replace non-empty objects
##
## returns:
##  a list with empty objects and modified non-empty elements
##
.replaceNonEmptyMassObjects <- function(l, replaceByList) {

  .stopIfNotIsMassObjectList(l)

  .stopIfNotIsMassObjectList(replaceByList)

  ## find empty MassPeaks objects
  notEmpty <- !(1:length(l) %in% findEmptyMassObjects(l))

  if (sum(notEmpty) != length(replaceByList)) {
    stop("Length of non-empty list elements of ", sQuote("l"),
         " and ", sQuote("replaceByList"), " have to be equal.")
  }

  ## replace list (combine empty and not empty elements)
  l[notEmpty] <- replaceByList

  return(l)
}
