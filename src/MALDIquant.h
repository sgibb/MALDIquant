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

#ifndef MALDIQUANT_H
#define MALDIQUANT_H

#include <R.h>
#include <Rinternals.h>

SEXP C_colMedians(SEXP x, SEXP na_rm);
SEXP C_snip(SEXP y, SEXP iterations, SEXP decreasing);
SEXP C_lowerConvexHull(SEXP x, SEXP y);
SEXP C_dilation(SEXP y, SEXP s);
SEXP C_erosion(SEXP y, SEXP s);
SEXP C_localMaxima(SEXP y, SEXP s);

#endif /* end of MALDIQUANT_H */
