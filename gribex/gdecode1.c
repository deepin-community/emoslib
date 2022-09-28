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
#include "gdecode1.h"

fortint IG1NUMB(gribProduct ** grib) {
  if( ecmwfLocalDefinition1Present((*grib)) )
    return g1_1_number( (*grib) );
  else
    return -999999;
}

fortdouble RG1NUMB(gribProduct ** grib) {
  return (fortdouble) IG1NUMB(grib);
}

fortint IG1TOTL(gribProduct ** grib) {
  if( ecmwfLocalDefinition1Present((*grib)) )
    return g1_1_total( (*grib) );
  else
    return -999999;
}

fortdouble RG1TOTL(gribProduct ** grib) {
  return (fortdouble) IG1TOTL(grib);
}

