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
#ifndef SENCODE2_H
#define SENCODE2_H

#ifdef FORTRAN_NO_UNDERSCORE
#define IS2NUMB is2numb
#define RS2NUMB rs2numb
#define IS2TOTL is2totl
#define RS2TOTL rs2totl
#define IS2METH is2meth
#define RS2METH rs2meth
#define IS2STAR is2star
#define RS2STAR rs2star
#define IS2END  is2end
#define RS2END  rs2end
#define IS2NLAT is2nlat
#define RS2NLAT rs2nlat
#define IS2WLON is2wlon
#define RS2WLON rs2wlon
#define IS2SLAT is2slat
#define RS2SLAT rs2slat
#define IS2ELON is2elon
#define RS2ELON rs2elon
#define IS2OPCL is2opcl
#define RS2OPCL rs2opcl
#define RS2CFCL rs2cfcl
#define IS2CFCL is2cfcl
#define RS2NUCL rs2nucl
#define IS2NUCL is2nucl
#define IS2LIST is2list
#else
#define IS2NUMB is2numb_
#define RS2NUMB rs2numb_
#define IS2TOTL is2totl_
#define RS2TOTL rs2totl_
#define IS2METH is2meth_
#define RS2METH rs2meth_
#define IS2STAR is2star_
#define RS2STAR rs2star_
#define IS2END  is2end_
#define RS2END  rs2end_
#define IS2NLAT is2nlat_
#define RS2NLAT rs2nlat_
#define IS2WLON is2wlon_
#define RS2WLON rs2wlon_
#define IS2SLAT is2slat_
#define RS2SLAT rs2slat_
#define IS2ELON is2elon_
#define RS2ELON rs2elon_
#define IS2OPCL is2opcl_
#define RS2OPCL rs2opcl_
#define RS2CFCL rs2cfcl_
#define IS2CFCL is2cfcl_
#define RS2NUCL rs2nucl_
#define IS2NUCL is2nucl_
#define IS2LIST is2list_
#endif

fortint IS2NUMB(gribProduct **, fortint *);
fortint RS2NUMB(gribProduct **, fortdouble *);
fortint IS2TOTL(gribProduct **, fortint *);
fortint RS2TOTL(gribProduct **, fortdouble *);
fortint IS2METH(gribProduct **, fortint *);
fortint RS2METH(gribProduct **, fortdouble *);
fortint IS2STAR(gribProduct **, fortint *);
fortint RS2STAR(gribProduct **, fortdouble *);
fortint IS2END(gribProduct **, fortint *);
fortint RS2END(gribProduct **, fortdouble *);
fortint IS2NLAT(gribProduct **, fortint *);
fortint RS2NLAT(gribProduct **, fortdouble *);
fortint IS2WLON(gribProduct **, fortint *);
fortint RS2WLON(gribProduct **, fortdouble *);
fortint IS2SLAT(gribProduct **, fortint *);
fortint RS2SLAT(gribProduct **, fortdouble *);
fortint IS2ELON(gribProduct **, fortint *);
fortint RS2ELON(gribProduct **, fortdouble *);
fortint IS2OPCL(gribProduct **, fortint *);
fortint RS2OPCL(gribProduct **, fortdouble *);
fortint IS2CFCL(gribProduct **, fortint *);
fortint RS2CFCL(gribProduct **, fortdouble *);
fortint IS2NUCL(gribProduct **, fortint *);
fortint RS2NUCL(gribProduct **, fortdouble *);
fortint IS2LIST(gribProduct**,fortint*,fortint*);

#endif /* end of SENCODE2_H */
