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
#ifndef GDECODE2_H
#define GDECODE2_H

#ifdef FORTRAN_NO_UNDERSCORE
#define IG2NUMB ig2numb
#define RG2NUMB rg2numb
#define IG2TOTL ig2totl
#define RG2TOTL rg2totl
#define IG2METH ig2meth
#define RG2METH rg2meth
#define IG2CFCL ig2cfcl
#define RG2CFCL rg2cfcl
#define IG2ELON ig2elon
#define RG2ELON rg2elon
#define IG2NLAT ig2nlat
#define RG2NLAT rg2nlat
#define IG2SLAT ig2slat
#define RG2SLAT rg2slat
#define IG2WLON ig2wlon
#define RG2WLON rg2wlon
#define IG2END  ig2end
#define RG2END  rg2end
#define IG2NUCL ig2nucl
#define RG2NUCL rg2nucl
#define IG2OPCL ig2opcl
#define RG2OPCL rg2opcl
#define IG2STAR ig2star
#define RG2STAR rg2star
#define IG2LIST ig2list
#else
#define IG2NUMB ig2numb_
#define RG2NUMB rg2numb_
#define IG2TOTL ig2totl_
#define RG2TOTL rg2totl_
#define IG2METH ig2meth_
#define RG2METH rg2meth_
#define IG2CFCL ig2cfcl_
#define RG2CFCL rg2cfcl_
#define IG2ELON ig2elon_
#define RG2ELON rg2elon_
#define IG2NLAT ig2nlat_
#define RG2NLAT rg2nlat_
#define IG2SLAT ig2slat_
#define RG2SLAT rg2slat_
#define IG2WLON ig2wlon_
#define RG2WLON rg2wlon_
#define IG2END  ig2end_
#define RG2END  rg2end_
#define IG2NUCL ig2nucl_
#define RG2NUCL rg2nucl_
#define IG2OPCL ig2opcl_
#define RG2OPCL rg2opcl_
#define IG2STAR ig2star_
#define RG2STAR rg2star_
#define IG2LIST ig2list_
#endif

fortint IG2NUMB(gribProduct **);
fortdouble RG2NUMB(gribProduct **);
fortint IG2TOTL(gribProduct **);
fortdouble RG2TOTL(gribProduct **);
fortint IG2METH(gribProduct **);
fortdouble RG2METH(gribProduct **);
fortint IG2CFCL(gribProduct **);
fortdouble RG2CFCL(gribProduct **);
fortint IG2ELON(gribProduct **);
fortdouble RG2ELON(gribProduct **);
fortint IG2NLAT(gribProduct **);
fortdouble RG2NLAT(gribProduct **);
fortint IG2SLAT(gribProduct **);
fortdouble RG2SLAT(gribProduct **);
fortint IG2WLON(gribProduct **);
fortdouble RG2WLON(gribProduct **);
fortint IG2END(gribProduct **);
fortdouble RG2END(gribProduct **);
fortint IG2NUCL(gribProduct **);
fortdouble RG2NUCL(gribProduct **);
fortint IG2OPCL(gribProduct **);
fortdouble RG2OPCL(gribProduct **);
fortint IG2STAR(gribProduct **);
fortdouble RG2STAR(gribProduct **);

#endif /* end of GDECODE2_H */
