## Copyright 2012-2013 Sebastian Gibb
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

.isFunctionList <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }

  length(x) && all(.unlist(vapply(x, is.function, logical(1L))))
}

.stopIfNotIsFunctionList <- function(x) {
  if (!.isFunctionList(x)) {
    parentCall <- deparse(sys.call(-1L))
    stop(parentCall, " : ", sQuote(deparse(substitute(x))),
         " is no list of functions!", call.=FALSE)
  }
  TRUE
}
