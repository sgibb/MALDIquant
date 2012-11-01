/* Copyright 2011 Sebastian Gibb
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

/* SNIP algorithm based on:
 * C.G. Ryan, E. Clayton, W.L. Griffin, S.H. Sie, and D.R. Cousens. 
 * "Snip, a statistics-sensitive background treatment for the quantitative
 *  analysis of pixe spectra in geoscience applications."
 * Nuclear Instruments and Methods in Physics Research Section B: 
 * Beam Interactions with Materials and Atoms, 34(3):396-402, 1988. 
 * ISSN 0168-583X. doi:10.1016/0168-583X(88)90063-8. 
 * URL http://www.sciencedirect.com/science/article/B6TJN-46YSYTJ-30/2/e0d015ceb8ea8a7bc0702a857a19750b
 */

#include <R.h>

/* y = array of double values
 * length = length of y
 * iterations = max iteration steps
 */
void R_snip(double* y, int* length, int* iterations, double* output) {
    
    int n=*length;

    /* allocate vector - error handling is done by R */
    double* tmp=(double*) Calloc((size_t) n, double);


    for (int i=1; i<=*iterations; ++i) {
        for (int j=i; j<n-i; ++j) {
            double a=y[j];
            double b=(y[j-i]+y[j+i])/2;
            if (b < a)
                a=b;
            tmp[j]=a;
        }

        for(int j=i; j<n-i; ++j) {
            y[j]=tmp[j];
        }
    }

    for(int i=0; i<n; ++i) {
        output[i]=y[i];
    }

    Free(tmp);
}
