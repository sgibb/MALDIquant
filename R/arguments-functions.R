## Copyright 2012 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This file is part of MALDIquant for R and related languages.
##
## PACKAGE is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## PACKAGE is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with PACKAGE. If not, see <http://www.gnu.org/licenses/>

## .isArgument
##  test whether a (argument) list contains a special name
##
## params:
##  x: character, single argument name
##  arguments: list, arguments
##
## returns:
##  TRUE/FALSE
##
.isArgument <- function(x, arguments) {
    return(x %in% names(arguments));
}

## .removeArguments
##  removes arguments from argument list
##
## params:
##  x: character, argument names
##  arguments: list, arguments
##
## returns:
##  reduced argument list
##
.removeArguments <- function(x, arguments) {
    keep <- !na.omit(names(arguments) %in% x);
    return(arguments[seq(along=arguments)[keep]]);
}
