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

## .prepareShowGroupName
##  prepares a group name for show (appends numbers to name if needed)
##
## params:
##  x: vector
##  name: group name
##
## returns:
##
.prepareShowGroupName <- function(x, name) {
  if (!is.null(x)) {
    n <- length(x)

    if (n > 1L) {
      name <- paste0(name, seq_len(n))
    }

  } else {
    name <- NULL
  }

  name
}
