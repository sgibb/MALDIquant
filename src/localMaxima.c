/* Copyright 2012-2013 Sebastian Gibb
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

/* TODO: replace int by R_xlen_t in R 3.0.0 */
int windowMaxIdx(double* x, int start, int end) {
  int i, m=start;
  for (i=start+1; i<=end; ++i) {
    if (x[m] < x[i]) {
      m=i;
    }
  }
  return(m);
}

/* y = array of double values
 * s = half window size
 */
SEXP C_localMaxima(SEXP y, SEXP s) {
  SEXP output;
  /* TODO: replace by R_xlen_t in R 3.0.0 */
  int n, q, m, windowSize, i, l, mid;

  PROTECT(y=coerceVector(y, REALSXP));
  n=LENGTH(y);

  PROTECT(output=allocVector(LGLSXP, n));

  double* xy=REAL(y);
  int* xo=LOGICAL(output);
  memset(xo, 0, n*sizeof(int));

  q=asInteger(s);
  windowSize=q*2;
  m=windowMaxIdx(xy, 0, windowSize);

  xo[m]=m==q;

  /* i == rhs; l == lhs; mid == middle pos */
  for (i=windowSize+1, l=i-windowSize, mid=(l+i)/2; i<n; ++i, ++mid, ++l) {
    /* maximum out of window, calculate new maximum in current window */
    if (m < l) {
      m=windowMaxIdx(xy, l, i);
    } else if (xy[i] > xy[m]) {
      m=i;
    }

    if (m == mid) {
      xo[m]=1;
    }
  }

  UNPROTECT(2);
  return(output);
}
