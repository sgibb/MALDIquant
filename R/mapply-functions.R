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

## .mapply
##  wrapper for mapply with different defaults
##
## params:
##  see ?mapply
##
## returns:
##  see ?mapply
##
.mapply <- function(FUN, ..., MoreArgs=NULL, SIMPLIFY=FALSE, USE.NAMES=FALSE,
                    mc.cores=1L) {
  parallel::mcmapply(FUN=FUN, ..., MoreArgs=MoreArgs, SIMPLIFY=SIMPLIFY,
                     USE.NAMES=USE.NAMES, mc.cores=mc.cores)
}
