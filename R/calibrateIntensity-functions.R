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

## .calibrateIntensitySimple
## calibrate intensity values by offset and scaling factor
##
## params:
##  y: double, intensity values
##  offset: double/function
##  scaling: double/function
##
## returns:
##  double, calibrated intensity values
##
.calibrateIntensitySimple <- function(y, offset=0, scaling=1) {
  if (is.function(offset)) {
    offset <- offset(y)
  }
  if (is.function(scaling)) {
    scaling <- scaling(y)
  }
  return( (y-offset)/scaling )
}

