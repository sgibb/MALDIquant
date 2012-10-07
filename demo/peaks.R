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


## peaks demo


## load necessary libraries
library("MALDIquant");

## load example spectra
data("fiedler2009subset", package="MALDIquant");

## choose only spectrum 1
s <- fiedler2009subset[[1]];

## remove baseline
s <- removeBaseline(s);

## detect peaks
p <- detectPeaks(s, SNR=10);

## plot spectrum
plot(s, main="example spectrum 1");

## draw vertical lines
lines(p, col=4);

## mark peaks
points(p, col=2, pch=4);

## label some peaks
labelPeaks(p, mass=c(1020, 1206, 1350, 1466, 1616, 2660, 2932, 3191,
                     3262, 3883, 4209, 5336, 5904, 7766, 9290));

