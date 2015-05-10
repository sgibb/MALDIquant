/* Copyright 2013 Sebastian Gibb
 * <mail@sebastiangibb.de>
 *
 * This file is part of MALDIquant for R and related languages.
 *
 * MALDIquant is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * MALDIquant is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with MALDIquant. If not, see <http://www.gnu.org/licenses/>
 */

#include "MALDIquant.h"

#include <R.h>
#include <Rinternals.h>

/* Dilation/Erosion is based on the vHGW Algorithm
 * (needs only 3 comparison per element):
 *
 * M. van Herk. "A Fast Algorithm for Local Minimum and Maximum Filters on
 * Rectangular and Octagonal Kernels."
 * Pattern Recognition Letters 13.7 (1992): 517-521.
 *
 * J. Y. Gil and M. Werman. "Computing 2-Dimensional Min, Median and Max
 * Filters." IEEE Transactions (1996): 504-507.
 *
 * Gil and Kimmel (GK) decrease the number of comparisons to nearly 1.5:
 *
 * J. Y. Gil and R. Kimmel. "Efficient Dilation, Erosion, Opening, and Closing
 * Algorithms." Pattern Analysis and Machine Intelligence,
 * IEEE Transactions on 24.12 (2002): 1606-1617.
 *
 * The implementation of the GK algorithm is much more complex and needs pointer
 * arithmetic, "goto" or multiple for-loops (increasing number of comparision).
 * The speed improvement is marginal and does not matter for our purposes.
 */

/* y = input vector
 * s = half window size (half filter length)
 */
SEXP C_dilation(SEXP y, SEXP s) {
  SEXP f, g, h, output;
  R_xlen_t n, fn,  k, q, i, r, j, gi, hi;

  PROTECT(y=coerceVector(y, REALSXP));
  n=XLENGTH(y);
  q=asInteger(s);
  k=2*q+1;

  /* add q (== halfWindowSize) values left/right
   * increase n to make n%windowSize == 0 */
  fn=n+2*q+(k-(n%k));
  PROTECT(f=allocVector(REALSXP, fn));
  PROTECT(g=allocVector(REALSXP, fn));
  PROTECT(h=allocVector(REALSXP, fn));
  PROTECT(output=allocVector(REALSXP, n));

  double* xy=REAL(y);
  double* xf=REAL(f);
  double* xg=REAL(g);
  double* xh=REAL(h);
  double* xo=REAL(output);

  memcpy(xf+q, xy, n*sizeof(double));

  /* init left extrema */
  for (i=0; i<q; ++i) {
    xf[i]=xf[q];
    xh[i]=xf[q];
  }
  /* init right extrema */
  r=q+n-1;
  for (i=q+n; i<fn; ++i) {
    xf[i]=xf[r];
    xg[i]=xf[r];
  }

  /* preprocessing */
  for (i=q, r=i+k-1; i<n+q; i+=k, r+=k) {

    /* init most left/right elements */
    xg[i]=xf[i];
    xh[r]=xf[r];

    for (j=1, gi=i+1, hi=r-1; j<k; ++j, ++gi, --hi) {
      if (xg[gi-1] < xf[gi]) {
        xg[gi]=xf[gi];
      } else {
        xg[gi]=xg[gi-1];
      }
      if (xh[hi+1] < xf[hi]) {
        xh[hi]=xf[hi];
      } else {
        xh[hi]=xh[hi+1];
      }
    }

  }

  /* merging */
  for (i=0, gi=k-1, hi=0; i<n; ++i, ++gi, ++hi) {
    if (xg[gi] < xh[hi]) {
      xo[i]=xh[hi];
    } else {
      xo[i]=xg[gi];
    }
  }

  UNPROTECT(5);
  return(output);
}

/* y = input vector
 * s = half window size (half filter length)
 */
SEXP C_erosion(SEXP y, SEXP s) {
  SEXP f, g, h, output;
  R_xlen_t n, fn,  k, q, i, r, j, gi, hi;

  PROTECT(y=coerceVector(y, REALSXP));
  n=XLENGTH(y);
  q=asInteger(s);
  k=2*q+1;

  /* add q (== halfWindowSize) values left/right
   * increase n to make n%windowSize == 0 */
  fn=n+2*q+(k-(n%k));
  PROTECT(f=allocVector(REALSXP, fn));
  PROTECT(g=allocVector(REALSXP, fn));
  PROTECT(h=allocVector(REALSXP, fn));
  PROTECT(output=allocVector(REALSXP, n));

  double* xy=REAL(y);
  double* xf=REAL(f);
  double* xg=REAL(g);
  double* xh=REAL(h);
  double* xo=REAL(output);

  memcpy(xf+q, xy, n*sizeof(double));

  /* init left extrema */
  for (i=0; i<q; ++i) {
    xf[i]=xf[q];
    xh[i]=xf[q];
  }
  /* init right extrema */
  r=q+n-1;
  for (i=q+n; i<fn; ++i) {
    xf[i]=xf[r];
    xg[i]=xf[r];
  }

  /* preprocessing */
  for (i=q, r=i+k-1; i<n+q; i+=k, r+=k) {

    /* init most left/right elements */
    xg[i]=xf[i];
    xh[r]=xf[r];

    for (j=1, gi=i+1, hi=r-1; j<k; ++j, ++gi, --hi) {
      if (xg[gi-1] > xf[gi]) {
        xg[gi]=xf[gi];
      } else {
        xg[gi]=xg[gi-1];
      }
      if (xh[hi+1] > xf[hi]) {
        xh[hi]=xf[hi];
      } else {
        xh[hi]=xh[hi+1];
      }
    }

  }

  /* merging */
  for (i=0, gi=k-1, hi=0; i<n; ++i, ++gi, ++hi) {
    if (xg[gi] > xh[hi]) {
      xo[i]=xh[hi];
    } else {
      xo[i]=xg[gi];
    }
  }

  UNPROTECT(5);
  return(output);
}
