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
#ifndef GDECODE1_H
#define GDECODE1_H

#ifdef FORTRAN_NO_UNDERSCORE
#define IG1NUMB ig1numb
#define RG1NUMB rg1numb
#define IG1TOTL ig1totl
#define RG1TOTL rg1totl
#else
#define IG1NUMB ig1numb_
#define RG1NUMB rg1numb_
#define IG1TOTL ig1totl_
#define RG1TOTL rg1totl_
#endif

fortint IG1NUMB(gribProduct **);
fortdouble RG1NUMB(gribProduct **);
fortint IG1TOTL(gribProduct **);
fortdouble RG1TOTL(gribProduct **);

#endif /* end of GDECODE1_H */
