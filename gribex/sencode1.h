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
#ifndef SENCODE1_H
#define SENCODE1_H

#ifdef FORTRAN_NO_UNDERSCORE
#define IS1NUMB is1numb
#define RS1NUMB rs1numb
#define IS1TOTL is1totl
#define RS1TOTL rs1totl
#else
#define IS1NUMB is1numb_
#define RS1NUMB rs1numb_
#define IS1TOTL is1totl_
#define RS1TOTL rs1totl_
#endif

fortint IS1NUMB(gribProduct **, fortint *);
fortint RS1NUMB(gribProduct **, fortdouble *);
fortint IS1TOTL(gribProduct **, fortint *);
fortint RS1TOTL(gribProduct **, fortdouble *);

#endif /* end of SENCODE1_H */
