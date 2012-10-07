## Copyright 2011-2012 Sebastian Gibb
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
setMethod(f="show",
    signature=signature(object="AbstractMassObject"),
    definition=function(object) {
    
    groups <- c("S4 class type",
                "Number of m/z values", 
                "Range of m/z values",
                "Range of intensity values");

    values <- class(object)[1];

    if (isEmpty(object)) {
        values <- c(values, 0, NA, NA);
    } else {
        values <- c(values,
                    length(object@mass), 
                    paste(round(range(object@mass), digits=3), collapse=" - "),
                    paste(format(min(object@intensity), digits=4, 
                                 scientific=TRUE), " - ",
                          format(max(object@intensity), digits=4,
                                 scientific=TRUE), sep=""));
    }

    if (!is.null(object@metaData$name)) {
        groups <- c(groups, "Name");    
        values <- c(values, object@metaData$name);
    }

    groups <- format(groups, justify="left");
    values <- format(values, justify="left");

    if (!is.null(object@metaData$file)) {
        n <- length(object@metaData$file);

        if (n > 1) {
            groups <- c(groups, paste("File", 1:n, sep=""));
        } else {
            groups <- c(groups, "File");
        }
        groups <- format(groups, justify="left");

        ## to avoid newlines in other values don't format filenames
        ## (they could be very long)
        values <- c(values, object@metaData$file);
    }

    for (i in seq(along=groups)) {
        cat(groups[i], ": ", values[i], "\n", sep="");
    }
});

