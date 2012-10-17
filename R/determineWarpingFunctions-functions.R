### Copyright 2012 Sebastian Gibb
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

## determineWarpingFunctions
##  calculates warping functions for each MassPeaks object
##
## params:
##  l: list, list of MassPeaks objects
##  reference: MassPeaks, a reference MassPeaks object to which all other
##             MassPeaks objects should be aligned
##  tolerance: double, maximal deviation of a peak position to be
##             considered as same peak
##  warpingFunction: function, used to create warping function
##  plot: logical, plots warping function
##
## returns:
##  a list of warping functions
##
determineWarpingFunctions <- function(l, reference, tolerance=0.002,
                                      warpingFunction=.warpingFunctionLowess,
                                      plot=FALSE, plotInteractive=FALSE, ...) {

    ## test arguments
    if (!isMassPeaksList(l) && !isMassPeaks(l)) {
        stop(sQuote("l"),
             " is no list of MALDIquant::MassPeaks objects!");
    }

    warpingFunction <- match.fun(warpingFunction);

    optArgs <- list(...);

    ## find reference peaks
    if (missing(reference)) {
        arguments <- list(l=l, tolerance=tolerance);

        for (i in c("method", "minFrequency")) {
            if (.isArgument(i, optArgs)) {
                arguments[[i]] <- optArgs[[i]];
                optArgs <- .removeArguments(i, optArgs);
            }
        }
        reference <- do.call(referencePeaks, arguments);
    }

    if (isEmpty(reference)) {
        stop("Reference MassPeaks Object contains no peaks!");
    }

    ## fetch plot.default arguments (debug plot)
    if (plot) {
        plotNames <- c("xlim", "ylim", "xlab", "ylab", "main", "sub", "lwd");
        plotArgs <- vector("list", length=length(plotNames));
        names(plotArgs) <- plotNames;

        for (i in plotNames) {
            if (.isArgument(i, optArgs)) {
                plotArgs[[i]] <- optArgs[[i]];
                optArgs <- .removeArguments(i, optArgs);
            }
        }
    }

    ## reference has to become sample no 1
    tmpPeakList <- c(reference, l);

    ## same procedure as in binPeaks
    ## fetch all mass
    mass <- unname(.unlist(lapply(tmpPeakList, function(x)x@mass)));

    ## fetch all intensities
    intensities <- .unlist(lapply(tmpPeakList, function(x)x@intensity));
    
    ## store original mass sample number/id
    samples <- .unlist(lapply(1:length(tmpPeakList), function(x) {
                              return(rep(x, length(tmpPeakList[[x]])));
    }));
    
    ## sort values by mass
    s <- sort(mass, method="quick", index.return=TRUE);
    
    mass <- s$x;
    intensities <- intensities[s$ix];
    samples <- samples[s$ix];
 
    ## run peak binning and use relaxed grouper which choose the highest test
    ## sample peaks
    binnedMass <- .binPeaks(mass=mass, intensities=intensities,
                            samples=samples, tolerance=tolerance,
                            grouper=.grouperRelaxedHighestAtReference);

    ## group mass/intensities by sample ids
    lIdx <- tapply(X=1:length(binnedMass), INDEX=samples, FUN=function(x) {
          return(x);
    });

    ## calculate differences
    binnedMass[binnedMass == 0] <- NA;
    d <- binnedMass-mass; 

    ## each function which determines a warping function uses these 3 arguments
    arguments <- list(x=NULL, d=NULL);
    if (length(optArgs) > 0) {
        arguments <- c(arguments, optArgs);
    }

    ## determine warping functions
    warpingFunctions <- lapply(lIdx[-1], function(i) {
        ## fetch changed mass == aligned peaks
        notNA <- !is.na(binnedMass[i]);

        arguments$x <- mass[i][notNA]; ## original mass
        arguments$d <- d[i][notNA];    ## difference to reference

        if (!length(arguments$x)) {
          stop("Could not match any peak in spectrum ", samples[i[1]]-1,
               " to a reference peak.")
        }

        w <- do.call(warpingFunction, arguments);

        return(w);
    });

    ## debug plot
    if (plot) {
        ## non interactive device (pdf, png, ...) available?
        isNonInteractivePlot <- dev.cur() != 1 && !dev.interactive();

        if (!isNonInteractivePlot && !plotInteractive) {
            warning(sQuote("plot"), " is ", sQuote("TRUE"),
                    " but no non-interactive devices is available. ",
                    "Using pdf() to create a default one.");
            pdf(paper="a4r", width=12);
        } else if (dev.cur() == 1 && plotInteractive) {
            warning(sQuote("plot"), " is ", sQuote("TRUE"), 
                    " but no interactive devices is available. ",
                    "Using dev.new() to create a default one.");
            dev.new();
        }

        ## set default plot arguments
        plotArgsDefaults <- list(xlim=range(mass),
                                 ylim=range(d, na.rm=TRUE),
                                 xlab="mass",
                                 ylab="difference",
                                 lwd=1);
        for (i in plotNames) {
            if (is.null(plotArgs[[i]])) {
                plotArgs[[i]] <- plotArgsDefaults[[i]];
            }
        }

        nReference <- length(reference);
        x <- plotArgs$xlim[1]:plotArgs$xlim[2];

        ## workaround to avoid error:
        ## Error in l[[i]] : this S4 class is not subsettable
        if (!is.list(l)) {
            l <- list(l);
        }

        for (i in seq(along=l)) {
            ## fetch changed mass == aligned peaks
            notNA <- !is.na(binnedMass[lIdx[[i+1]]]);

            if (is.null(plotArgs$main)) {
                main <- paste("sample ", i, " vs reference\n",
                               "(matched peaks: ", sum(notNA), "/", 
                               nReference, ")", sep="");
            } else {
                main <- plotArgs$main;
            }

            if (is.null(plotArgs$sub)) {
                sub <- l[[i]]@metaData$file;
            } else {
                sub <- plotArgs$sub;
            }

            ## plot reference vs sample
            plot(l[[i]]@mass[notNA], d[lIdx[[i+1]]][notNA], type="p",
                 xlim=plotArgs$xlim, ylim=plotArgs$ylim, 
                 xlab=plotArgs$xlab, ylab=plotArgs$ylab,
                 cex.main=0.8, main=main, sub=sub,
                 lwd=plotArgs$lwd, cex.sub=0.75, col.sub="#808080");

            ## draw warping function
            lines(x, warpingFunctions[[i]](x), lwd=plotArgs$lwd);
        }

        if (!isNonInteractivePlot && !plotInteractive) {
            dev.off();
        }
    }

    return(warpingFunctions);
}

