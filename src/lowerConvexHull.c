/* Copyright 2011 Sebastian Gibb
 * <mail@sebastiangibb.de>
 *
 * This file is part of PACKAGE for R and related languages.
 *
 * PACKAGE is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * PACKAGE is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with PACKAGE. If not, see <http://www.gnu.org/licenses/>
 */

/* Lower Convex Hull Algorithm based on:
 * A. M. Andrew, "Another Efficient Algorithm for Convex Hulls in Two
 * Dimensions", Info. Proc. Letters 9, 216-219 (1979).
 * only calculate lower hull (we don't need the upper one)
 */

#include <R.h>


/* build cross product of 2 vectors and compare to zero
 * returns true for P2(x2, y2) left of P0->P1
 */ 
double left(double x0, double y0, double x1, double y1, double x2, double y2) {
    return(((x1-x0)*(y2-y0) - (x2-x0)*(y1-y0)) > 0);
}

/* x = array of double values 
 * y = array of double values
 * length = length of y
 * output = array of double values (new y values)
 */
void R_lowerConvexHull(double* x, double* y, int* length, double* output) {
    
    int n=*length;

    int k=0;

    /* allocate vector - error handling is done by R */
    int* nodes=(int*) Calloc((size_t) n, int);

    /* find lower convex hull */
    for (int i=0; i<n; ++i) {
        while (k > 1 && !left(x[nodes[k-2]], y[nodes[k-2]], 
                              x[nodes[k-1]], y[nodes[k-1]], x[i], y[i])) {
            k=k-1;
        }
        nodes[k]=i;
        k=k+1;
    }

    /* build linear function y=mx+c to calculate values between nodes */
    for (int i=0; i<k; ++i) {
        double m=(y[nodes[i+1]]-y[nodes[i]])/(x[nodes[i+1]]-x[nodes[i]]);
        double c=y[nodes[i]]-m*x[nodes[i]];

        for (int j=nodes[i]; j<nodes[i+1]; ++j) {
            output[j]=m*x[j]+c;
        }
    }

    output[n-1]=y[n-1];

    Free(nodes);
}
