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

#' .pseudoCluster
#'
#' Find possible isotopic cluster in mass/mz data.
#'
#' @param x double, mass
#' @param size integer, cluster size, number of peaks per cluster
#' @param stepSize double, distance between isotopes (mass of a neutron; see
#' Park et al 2008)
#' @param tolerance double, mass tolerance
#' @return a matrix of indices (nrow(x) == n) of potential clusters
#' @references
#' K. Park, J.Y. Yoon, S. Lee, E. Paek, H. Park, H.J. Jung, and S.W. Lee. 2008.
#' Isotopic peak intensity ratio based algorithm for determination of isotopic
#' clusters and monoisotopic masses of polypeptides from high-resolution
#' mass spectrometric data.
#' Analytical Chemistry, 80: 7294-7303.
#'
.pseudoCluster <- function(x, size=3L, stepSize=1.00235, tolerance=1e-4) {
  if (size < 2L) {
    stop("The ", sQuote("size"), " of a cluster has to be at least 2!")
  }
  stepSize <- stepSize * 0L:(size - 1L)
  nr <- length(stepSize)
  mm <- matrix(x, nrow=nr, ncol=length(x), byrow=TRUE)
  ms <- mm + stepSize

  i <- MALDIquant:::.which.closest(ms, x)
  m <- x[i]
  dim(m) <- dim(i)

  i[, colSums(abs(ms - m)/mm < tolerance) == nr, drop=FALSE]
}

#' .F
#'
#' Map mass to poisson mean/lambda.
#'
#' @param mass double, mass from experimental peak list
#' @return double suitable to pass to `dpois`
#' @references
#' K.L. Williams, and M.R. Wilkins.
#' Automatic poisson peak harvesting for high throughput protein identification.
#' Electrophoresis 21 (2000): 2243-2251.
#' @noRd
.F <- function(x)0.000594 * x + 0.03091

#' .P
#'
#' Model isotopic distribution by poisson distribution.
#'
#' @param mass double, mass from experimental peak list
#' @param isotopes integer, which isotopes
#' @return double, isotopic distribution
#' @references
#' K.L. Williams, and M.R. Wilkins.
#' Automatic poisson peak harvesting for high throughput protein identification.
#' Electrophoresis 21 (2000): 2243-2251.
#' @noRd
.P <- function(x, isotopes)dpois(isotopes, .F(x))

#' .Psum
#'
#' Model isotopic distribution by poisson distribution and sum to 1 (similar to
#' TIC).
#'
#' @param mass double, mass from experimental peak list
#' @param isotopes integer, which isotopes
#' @return double, isotopic distribution
#' @references
#' K.L. Williams, and M.R. Wilkins.
#' Automatic poisson peak harvesting for high throughput protein identification.
#' Electrophoresis 21 (2000): 2243-2251.
#' @noRd
.Psum <- function(x, isotopes) {
  ni <- length(isotopes)
  nx <- length(x)
  p <- .P(rep.int(x, rep.int(ni, nx)), isotopes)
  dim(p) <- c(ni, nx)
  t(t(p)/colSums(p))
}

#' .monoisotopic
#'
#' Model isotopic distribution by poisson distribution.
#'
#' @param x double, mass from experimental peak list
#' @param y double, intensity from experimental peak list
#' @param tolerance double, mass tolerance for .pseudoCluster
#' @param minCor double, minimal correlation between experimental and model
#' intensities
#' @param stepSize double, distance between isotopes (mass of a neutron; see
#' Park et al 2008)
#' @param size integer, cluster size (number of peaks for a possible cluster),
#' see .pseudoCluster
#' @return double, index of monoisotopic masses
#' @references
#' K.L. Williams, and M.R. Wilkins.
#' Automatic poisson peak harvesting for high throughput protein identification.
#' Electrophoresis 21 (2000): 2243-2251.
#' @noRd
.monoisotopic <- function(x, y, minCor=0.95, tolerance=1e-4, stepSize=1.00235,
                          size=3L) {
  pc <- .pseudoCluster(x, size=size, stepSize=stepSize, tolerance=tolerance)
  y <- y[pc]
  dim(y) <- dim(pc)
  y <- t(t(y)/colSums(y))
  p <- .Psum(x[pc[1L,]], isotopes=0L:(size-1L))
  cr <- .colCors(y, p)
  pc <- pc[, cr > minCor, drop=FALSE]
  pc[duplicated(as.vector(pc))] <- NA_real_
  pc[1L, colSums(is.na(pc)) == 0L]
}
