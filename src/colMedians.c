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
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with MALDIquant. If not, see <http://www.gnu.org/licenses/>
*/

#include "MALDIquant.h"

#include <R.h>
#include <Rinternals.h>

/* x = matrix of double values
* na_rm = remove NA?
* based on src/library/stats/R/median.R
*/
SEXP C_colMedians(SEXP x, SEXP na_rm) {
  SEXP dims, column, output;
  R_xlen_t ncol, nrow, ir, ic, icr, half;
  int narm, isEven;
  double v;

  PROTECT(x=coerceVector(x, REALSXP));
  PROTECT(dims=getAttrib(x, R_DimSymbol));

  /* TODO: Replace by R_xlen_t in R 3.0.0.
   * TODO: Maybe this will cause problems because we need Long Vector support.
   */
  nrow=INTEGER(dims)[0];
  ncol=INTEGER(dims)[1];

  narm=asInteger(na_rm);

  PROTECT(output=allocVector(REALSXP, ncol));
  PROTECT(column=allocVector(REALSXP, nrow));

  double* xx=REAL(x);
  double* xo=REAL(output);
  double* xc=REAL(column);

  for (ic=0; ic<ncol; ic++) {
    /* copy current column */
    for (icr=0, ir=0; ir<nrow; ir++) {
      v=xx[ir+ic*nrow];

      if (ISNAN(v)) {
        if (!narm) {
          icr=0;
          xo[ic]=R_NaReal;
          break;
        }
      } else {
        xc[icr]=v;
        icr++;
      }
    }

    /* calculate median */
    if (icr) {
      isEven=((icr) % 2 == 0);
      /* explicit cast to int (truncated towards zero) */
      half=(R_xlen_t)((icr)/2);

      /* use R's internal partial sort for REAL numbers:
       * http://cran.r-project.org/doc/manuals/R-exts.html#Utility-functions
       * src/main/sort.c
       * TODO: Maybe this will cause problems, because rPsort doesn't support
       * Long Vectors until now.
       */
      rPsort(xc, icr, half);
      xo[ic]=xc[half];

      if (isEven) {
        rPsort(xc, half, half-1);
        xo[ic]=(xo[ic]+xc[half-1])/2;
      }
    }
  }

  UNPROTECT(4);

  return(output);
}
