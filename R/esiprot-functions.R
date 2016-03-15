## Copyright 2016 Sebastian Gibb
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

#' ESIprot: an universal tool for charge state determination
#' and molecular weight calculation of proteins from electrospray ionization
#' mass spectrometry data calculate.
#'
#' @param x \code{double} consecutive peaks
#' @param range \code{double}, charge state range
#'  \code{c(min charge, max charge)}
#' @param hydrogen \code{double}, hydrogen mass
#'
#' @author Original python implementation by
#'  Robert Winkler \email{robert.winkler@@bioprocess.org}.
#'
#' R implementation by Sebastian Gibb \email{mail@@sebastiangibb.de}.
#'
#' @references
#' R. Winkler. 2010.
#' ESIprot: a universal tool for charge state determination and molecular
#' weight calculation of proteins from electrospray ionization mass
#' spectrometry data.
#' Rapid Communications in Mass Spectrometry 24(3): 285-294
#'
#' J. Meija, T.B. Coplen, M. Berglund, W.A. Brand, P. De Bièvre, M. Gröning,
#' N.E. Holden, J. Irrgeher, R.D. Loss, T. Walczyk, and T. Prohaska. 2016.
#' Atomic weights of the elements 2013 (IUPAC Technical Report).
#' Pure and Applied Chemistry (ahead of print)
#'
#' @examples
#'
#' library("MALDIquant")
#'
#' cytochromc <- c(651.6, 687.7, 728.1, 773.5, 825.0, 883.9, 951.7,
#'                 1030.9, 1124.5)
#' MALDIquant:::.esiprot(cytochromc)
#'
#' @noRd
.esiprot <- function(x, range=c(1L, 100L), hydrogen=1.00784) {
  if (!is.numeric(x) || length(x) <= 1L) {
    stop(sQuote("x"), " has to be a numeric vector with more than 1 element.")
  }
  if (!is.numeric(range) || length(range) != 2L) {
    stop(sQuote("range"), " has to be a numeric vector of length 2.")
  }

  x <- x - hydrogen
  n <- length(x)
  z <- floor(max(range[1L], n)):ceiling(max(range[2L], n))
  m <- .tembed(z, n) * x
  s <- .colSd(m)
  i <- which.min(s)
  c(mw=mean(m[, i]), sd=s[i], z=z[i] + n - 1L)
}

#' .tembed
#'
#' identical to t(embed(x, dimension)) but slightly faster (and not working for
#' matrices).
#'
#' @param x numeric vector
#' @param dimension
#' @return matrix
#' @seealso \code{\link[stats]{embed}}
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.tembed <- function(x, dimension=1L) {
  n <- length(x)
  m <- n - dimension + 1L
  d <- x[rep.int(dimension:n, rep.int(dimension, m)) - 0L:(dimension - 1L)]
  dim(d) <- c(dimension, m)
  d
}

#' .consecutiveIndices
#'
#' returns n consecutive peaks around the center peak; center would be in the
#' middle (if n is even and method == "left", length(LHS) == length(RHS) + 1;
#' if center - n/2 < 0 RHS would be extended)
#'
#' @param x double, mass of a MassPeaks object
#' @param center integer, center index
#' @param n integer, how many peaks?
#' @param method prefer left/right if n is even
#' @return indices around the center
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.consecutiveIndices <- function(x, center, n,
                                method=c("left", "right")) {
  method <- match.arg(method)
  if (method == "right") {
    center <- center + 1L
  }
  n <- min(length(x), n)
  left <- max(1L, center - floor(n/2L))
  left:(left + n - 1L)
}
