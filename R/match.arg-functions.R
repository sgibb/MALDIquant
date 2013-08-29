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

## .match.arg
##  wrapper for match.arg to provide case insensitve argument matching
##
## params:
##  arg: character
##
## returns:
##  see also ?match.arg
##
.match.arg <- function(arg, choices, several.ok=FALSE) {
  ## stolen from base::match.arg
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }
  if (is.null(arg)) {
    return(choices[1L])
  } else if (!is.character(arg)) {
    stop("'arg' must be NULL or a character vector")
  }

  if (identical(arg, choices)) {
    return(arg[1L])
  }
  if (length(arg) > 1L) {
    stop("'arg' must be of length 1")
  } else if (length(arg) == 0L) {
    stop("'arg' must be of length >= 1")
  }
  i <- pmatch(tolower(arg), tolower(choices), nomatch=0L, duplicates.ok=FALSE)

  if (all(i == 0L)) {
    stop("'arg' should be one of ",
         paste0(dQuote(choices), collapse = ", "))
  }
  i <- i[i > 0L]
  if (!several.ok && length(i) > 1) {
    stop("there is more than one match in 'match.arg'")
  }
  return(choices[i])
}
