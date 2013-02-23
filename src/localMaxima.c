/* Copyright 2012 Sebastian Gibb
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

int windowMaxIdx(double* x, int start, int end) {
  int m=start;
  for (int i=start+1; i<=end; ++i) {
    if (x[m] < x[i]) {
      m=i;
    }
  }
  return(m);
}

int middle(int pos, int start, int end) {
  return(pos == (start + end)/2);
}

/* y = array of double values
 * n = length of y
 * span = half window size
 */
void R_localMaxima(double* y, int* n, int* span, int* output) {

    int windowSize=*span*2;

    int m=windowMaxIdx(y, 0, windowSize);

    output[m]=m==*span;

    unsigned int i, l, mid;

    /* i == rhs; l == lhs; mid == middle pos */
    for (i=windowSize+1, l=i-windowSize, mid=(l+i)/2; i<*n; ++i, ++mid, ++l) {
      /* maximum out of window, calculate new maximum in current window */
      if (m < l) {
        m=windowMaxIdx(y, l, i);
      } else if (y[i] > y[m]) {
        m=i;
      }

      if (m == mid) {
        output[m]=1;
      }
    }
}

