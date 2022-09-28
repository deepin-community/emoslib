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
#ifndef GRIB_INT_T_H
#define GRIB_INT_T_H

#ifdef INTEGER_IS_INT
typedef int grib_int_t;
#else
typedef long grib_int_t;
#endif
typedef long long grib_int_64_t;

typedef double grib_real_t;

typedef unsigned char grib_octet_t;
typedef const char grib_string_t;

#endif /* end of GRIB_INT_T_H */
