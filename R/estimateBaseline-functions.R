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

## estimateBaselineConvexHull 
##  estimate baseline by creating a convex hull 
##
##  A. M. Andrew, "Another Efficient Algorithm for Convex Hulls in Two
##  Dimensions", Info. Proc. Letters 9, 216-219 (1979).
##  only calculate lower hull (we don't need the upper one)
##
## params:
##  x: vector of x values
##  y: vector of y values
##
## returns:
##  a matrix of the estimate baseline (col1: mass; col2: intensity)
##

## C version
.estimateBaselineConvexHull <- function(x, y) {
    n <- length(x);
    y <- .C("R_lowerConvexHull",
            as.double(x),
            as.double(y),
            as.integer(n),
            output=double(n),
            DUP=TRUE,
            PACKAGE="MALDIquant")$output;
    return(cbind(x, y));
}

## R only: obsolete because too slow
.lowerConvexHullR <- function(x, y) {
    
    ## .left()
    ## build cross product of 2 vectors and compare to zero
    ## returns true for >= P2(x2, y2) left/on the line of P0->P1
    .left <- function(x0, y0, x1, y1, x2, y2) {
        return(((x1-x0)*(y2-y0) - (x2-x0)*(y1-y0)) > 0);
    }

    # typically x values have to been sorted
    # our x values are already sorted
    index <- double(length(x));
    k <- 1;

    for (i in seq(along=x)) {
        while (k > 2 && !.left(x[index[k-2]], y[index[k-2]],
                               x[index[k-1]], y[index[k-1]],
                               x[i], y[i])) {
            ## remove last point
            k <- k-1;
        }
        index[k] <- i;
        k <- k+1;
    }

    index <- index[1:(k-1)];

    b <- matrix(.unlist(approx(x=x[index], y=y[index], xout=x, method="linear",
                               rule=2)),
                nrow=length(x), ncol=2);

    return(b);
}

## estimateBaselineMedian
##  estimate baseline by computing moving median
##
## params:
##  x: vector of x values
##  y: vector of y values
##  halfWindowSize: size of local window 
##
## returns:
##  a matrix of the estimate baseline (col1: mass; col2: intensity)
##
.estimateBaselineMedian <- function(x, y, halfWindowSize=100) {
    if (halfWindowSize<1) {
        stop(sQuote("halfWindowSize"), "=", halfWindowSize, " is too small!");
    }
    
    m <- runmed(y, k=(2*halfWindowSize+1));

    return(cbind(x, m));
}

## estimateBaselineSnip
##  estimate baseline by SNIP algorithm 
##
##  SNIP algorithm based on:
##  C.G. Ryan, E. Clayton, W.L. Griffin, S.H. Sie, and D.R. Cousens. 
##  "Snip, a statistics-sensitive background treatment for the quantitative
##  analysis of pixe spectra in geoscience applications."
##  Nuclear Instruments and Methods in Physics Research Section B: 
##  Beam Interactions with Materials and Atoms, 34(3):396-402, 1988. 
##  ISSN 0168-583X. doi:10.1016/0168-583X(88)90063-8. 
##  URL http://www.sciencedirect.com/science/article/B6TJN-46YSYTJ-30/2/e0d015ceb8ea8a7bc0702a857a19750b
##
## params:
##  x: vector of x values (only needed for create a matrix as return value)
##  y: vector of y values
##  iterations: number of iterations 
##
## returns:
##  a matrix of the estimate baseline (col1: mass; col2: intensity)

## C version
.estimateBaselineSnip <- function(x, y, iterations=100) {
    n <- length(y);
    y <- .C("R_snip",
            as.double(y),
            as.integer(n),
            as.integer(iterations),
            output=double(n),
            DUP=TRUE,
            PACKAGE="MALDIquant")$output;
    return(cbind(x, y));
}

## R only: obsolete because too slow
.snipR <- function(x, y, iterations=100) {
    n <- length(y);

    for (i in seq(from=1, to=iterations)) {
        j <- (i+1):(n-i);
        jl <- j-i;
        ju <- j+i;
        m <- (y[jl]+y[ju])/2;
        ## too slow
        #y[j] <- ifelse(y[j] > m, m, y[j]);
        ml <- y[j]>m;
        y[j][ml] <- m[ml];
    }
    return(cbind(x, y));
}

