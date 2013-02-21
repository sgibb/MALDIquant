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

 * You should have received a copy of the GNU General Public License
 * along with MALDIquant. If not, see <http://www.gnu.org/licenses/>
 */

#include <R.h>

/* based on vHGW dilation Algorithm, see morphological_filters.h for details */

/* f = input vector
 * fn = length of f
 * n = number of values in f before padding of zeros left and right
 * k = filter length
 * q = floor(k/2)
 * output = output vector
 */
void localMaxima(double* f, int fn, int n, int k, int q, int* output) {
  /* allocate vector - error handling is done by R */
  double* g=(double*) Calloc((size_t) fn, double);
  double* h=(double*) Calloc((size_t) fn, double);

  unsigned int i, r, j, fi, gi, hi;

  /* init extrema */
  r=q+n-1;
  for (i=0, gi=q+n+q-1, hi=0;  i<q; ++i, --gi, ++hi) {
    g[gi]=f[r];
    h[hi]=f[q];
  }

  /* preprocessing */
  for (i=q, r=i+k-1; i<n+q; i+=k, r+=k) {

    /* init most left/right elements */
    g[i]=f[i];
    h[r]=f[r];

    for (j=1, gi=i+1, hi=r-1; j<k; ++j, ++gi, --hi) {
      if (g[gi-1] < f[gi]) {
        g[gi]=f[gi];
      } else {
        g[gi]=g[gi-1];
      }
      if (h[hi+1] < f[hi]) {
        h[hi]=f[hi];
      } else {
        h[hi]=h[hi+1];
      }
    }

  }

  /* merging */
  for (i=0, fi=q, gi=k-1, hi=0; i<n; ++i, ++fi, ++gi, ++hi) {
    if (g[gi] < h[hi]) {
      output[i]=h[hi]==f[fi];
    } else {
      output[i]=g[gi]==f[fi];
    }
  }

  Free(g);
  Free(h);
}

void R_localMaxima(double* f, int* fn, int* n, int* k,  int* q, int* output) {
  localMaxima(f, *fn, *n, *k, *q, output);
}

