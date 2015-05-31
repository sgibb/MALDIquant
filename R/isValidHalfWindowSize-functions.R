## Copyright 2013 Sebastian Gibb
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

## .stopIfNotIsValidHalfWindowSize
##  test for too small/large half window size
##
## params:
##  halfWindowSize: half window size
##  n: length of data
##
## returns:
##  TRUE if valid
##
.stopIfNotIsValidHalfWindowSize <- function(halfWindowSize, n) {
  parentCall <- deparse(sys.call(-1L))

  if (halfWindowSize < 1L) {
    stop(parentCall, " : ", sQuote("halfWindowSize"),
         " is too small!", call.=FALSE)
  }

  windowSize <- halfWindowSize * 2L + 1L

  if (windowSize > n) {
    stop(parentCall, " : ", sQuote("halfWindowSize"),
         " is too large!", call.=FALSE)
  }

  TRUE
}
