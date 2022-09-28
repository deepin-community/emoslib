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

#include "gdecode.h"
#include "sencode.h"

#include "sencode1.h"

fortint IS1NUMB(gribProduct ** grib, fortint * number) {
gribProduct * g = *grib;

  if( !( ecmwfLocalDefinitionPresent(g) && (g1_definition(g)==1) ) )
    return (fortint) -1;

  MOVE1BYTE(((g->g1)->local.contents.def1.number),number);
  return 0;
}

fortint RS1NUMB(gribProduct ** grib, fortdouble *number) {
fortint Number = (fortint) *number;
  return IS1NUMB(grib,&Number);
}

fortint IS1TOTL(gribProduct ** grib, fortint * total) {
gribProduct * g = *grib;

  if( !( ecmwfLocalDefinitionPresent(g) && (g1_definition(g)==1) ) )
    return (fortint) -1;

  MOVE1BYTE(((g->g1)->local.contents.def1.total),total);
  return 0;
}

fortint RS1TOTL(gribProduct ** grib, fortdouble *total) {
fortint Total = (fortint) *total;
  return IS1TOTL(grib,&Total);
}
