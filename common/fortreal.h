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
#ifndef FORTREAL_H
#define FORTREAL_H

#ifdef REAL_8
#define fortreal long double
#else
#define fortreal float
#endif

#define fortdouble double

#ifdef REAL_8
typedef double fortfloat;
#else
typedef float fortfloat; /* fortran single precision float */
#endif

#endif /* end of FORTREAL_H */
