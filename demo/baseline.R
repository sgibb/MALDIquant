## Copyright 2012 Sebastian Gibb
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


## baseline demo


## load necessary libraries
library("MALDIquant");

## load example spectra
data("fiedler2009subset", package="MALDIquant");

## choose only spectrum 1
s <- fiedler2009subset[[1]];

## test different baseline estimation methods
bSnip <- estimateBaseline(s, method="SNIP");
plot(s, main="SNIP Baseline [default]");
lines(bSnip, lwd=2, col=2);

bConvexHull <- estimateBaseline(s, method="ConvexHull");
plot(s, main="ConvexHull Baseline");
lines(bConvexHull, lwd=2, col=2);

bMedian <- estimateBaseline(s, method="Median");
plot(s, main="Median Baseline");
lines(bMedian, lwd=2, col=2);

