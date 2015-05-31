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

## .memoryUsageStr.object_size
##  pretty string of memory usage
##
## params:
##  x: object_size
##
## returns:
##  character
##
.memoryUsageStr <- function(x) {
  os <- object.size(x)
  iec <- c("B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB")
  l <- trunc(log(os) / log(1024L))
  i <- pmin(l + 1L, 9L)

  paste(round(os / (1024L^l), digits=3L), iec[i])
}
