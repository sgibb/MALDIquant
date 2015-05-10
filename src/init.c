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
#include <R_ext/Rdynload.h>

static const R_CallMethodDef callMethods[] = {
    {"C_colMedians", (DL_FUNC) &C_colMedians, 2},
    {"C_snip", (DL_FUNC) &C_snip, 3},
    {"C_lowerConvexHull", (DL_FUNC) &C_lowerConvexHull, 2},
    {"C_dilation", (DL_FUNC) &C_dilation, 2},
    {"C_erosion", (DL_FUNC) &C_erosion, 2},
    {"C_localMaxima", (DL_FUNC) &C_localMaxima, 2},
    { NULL, NULL, 0 } /* mark last element */
};

void
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_MALDIquant(DllInfo *info)
{
    /* no .C, .Fortran, or .External routines => NULL */
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
