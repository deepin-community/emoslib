/**
* Copyright 1981-2016 ECMWF.
*
* This software is licensed under the terms of the Apache Licence 
* Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
*
* In applying this licence, ECMWF does not waive the privileges and immunities 
* granted to it by virtue of its status as an intergovernmental organisation 
* nor does it submit to any jurisdiction.
*/

#include "common/fortint.h"

#ifdef FORTRAN_NO_UNDERSCORE
#define FFTCHK fftchk
#else
#define FFTCHK fftchk_
#endif

#ifdef REAL_8
fortint FFTCHK(fortint * trunc, double * longinc)
#else
fortint FFTCHK(fortint * trunc, float * longinc)
#endif
{
/*   Checks if the given truncation and longitude increment can be handled
     by the FFT routine used in the interpolation scheme.

     Returns 1 if it can, otherwise 0.

     For calculation purposes, the number of longitude points has to be
     greater than 2*(output truncation) to ensure that the fourier
     transform is exact. For more information see page 10 in:

      E.C.M.W.F. Research Department technical memorandum no. 56
                 "The forecast and analysis post-processing package"
                 May 1982. J.Haseler.

*/
long nlonpts = (long) ( (360.0/(*longinc)) + 0.5 );

/*  Set number of longitude points > 2*truncation   */
    while( nlonpts < 2*(*trunc) ) nlonpts *= 2;

/*  Look for allowed factors: 8, 6, 5, 4 ,3 , 2 */

/*  Check 6 first */
    while( nlonpts%6 == 0 ) nlonpts /= 6;

/*  8 only allowed once as a factor */
    if( nlonpts%8 == 0 ) nlonpts /= 8;

    while( nlonpts%5 == 0 ) nlonpts /= 5;
    while( nlonpts%4 == 0 ) nlonpts /= 4;
    while( nlonpts%3 == 0 ) nlonpts /= 3;
    while( nlonpts%2 == 0 ) nlonpts /= 2;

    if( nlonpts != 1) return (fortint) 0;

    return (fortint) 1;
}
