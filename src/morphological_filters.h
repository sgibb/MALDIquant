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

#ifndef MORPHOLOGICAL_FILTERS_H
#define MORPHOLOGICAL_FILTERS_H

#include <R.h>

/* Dilation/Erosion is based on the vHGW Algorithm
 * (needs only 3 comparison per * element):
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

/* f = input vector
 * fn = length of f
 * n = number of values in f before padding of zeros left and right
 * k = filter length
 * q = floor(k/2)
 * output = output vector
 */
void dilation(double* f, int fn, int n, int k, int q, double* output) {
  /* allocate vector - error handling is done by R */
  double* g=(double*) Calloc((size_t) fn, double);
  double* h=(double*) Calloc((size_t) fn, double);

  unsigned int i, r, j, gi, hi;

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
  for (i=0, gi=k-1, hi=0; i<n; ++i, ++gi, ++hi) {
    if (g[gi] < h[hi]) {
      output[i]=h[hi];
    } else {
      output[i]=g[gi];
    }
  }

  Free(g);
  Free(h);
}

/* f = input vector
 * fn = length of f
 * n = number of values in f before padding of zeros left and right
 * k = filter length
 * q = floor(k/2)
 * output = output vector
 */
void erosion(double* f, int fn, int n, int k, int q, double* output) {
  /* allocate vector - error handling is done by R */
  double* g=(double*) Calloc((size_t) fn, double);
  double* h=(double*) Calloc((size_t) fn, double);

  unsigned int i, r, j, gi, hi;

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
      if (g[gi-1] > f[gi]) {
        g[gi]=f[gi];
      } else {
        g[gi]=g[gi-1];
      }
      if (h[hi+1] > f[hi]) {
        h[hi]=f[hi];
      } else {
        h[hi]=h[hi+1];
      }
    }

  }

  /* merging */
  for (i=0, gi=k-1, hi=0; i<n; ++i, ++gi, ++hi) {
    if (g[gi] > h[hi]) {
      output[i]=h[hi];
    } else {
      output[i]=g[gi];
    }
  }

  Free(g);
  Free(h);
}

#endif /* end of MORPHOLOGICAL_FILTERS_H */

