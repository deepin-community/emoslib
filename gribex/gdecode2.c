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
#include "gdecode2.h"

fortint IG2NUMB(gribProduct ** grib) {
  if( ecmwfLocalDefinition2Present((*grib)) )
    return g1_2_number( (*grib) );
  else
    return -999999;
}

fortdouble RG2NUMB(gribProduct ** grib) {
  return (fortdouble) IG2NUMB(grib);
}

fortint IG2TOTL(gribProduct ** grib) {
  if( ecmwfLocalDefinition2Present((*grib)) )
    return g1_2_total( (*grib) );
  else
    return -999999;
}

fortdouble RG2TOTL(gribProduct ** grib) {
  return (fortdouble) IG2TOTL(grib);
}

fortint IG2METH(gribProduct ** grib) {
  if( ecmwfLocalDefinition2Present((*grib)) )
    return g1_2_method( (*grib) );
  else
    return -999999;
}

fortdouble RG2METH(gribProduct ** grib) {
  return (fortdouble) IG2METH(grib);
}

fortint IG2CFCL(gribProduct ** grib) {
  if( ecmwfLocalDefinition2Present((*grib)) )
    return g1_2_control( (*grib) );
  else
    return -999999;
}

fortdouble RG2CFCL(gribProduct ** grib) {
  return (fortdouble) IG2CFCL(grib);
}

fortint IG2ELON(gribProduct ** grib) {
fortint value;
  if( ecmwfLocalDefinition2Present((*grib)) ) {
    value = g1_2_domainE( (*grib) );
    if( value & 0x800000 ) value = - (value & 0x7fffff);
    return value;
  }
  else
    return -999999;
}

fortdouble RG2ELON(gribProduct ** grib) {
fortdouble value = (fortdouble) IG2ELON(grib);
  if( value != -999999.0 )
    return (value/1000.0);
  else
    return value;
}

fortint IG2NLAT(gribProduct ** grib) {
fortint value;
  if( ecmwfLocalDefinition2Present((*grib)) ) {
    value = g1_2_domainN( (*grib) );
    if( value & 0x800000 ) value = - (value & 0x7fffff);
    return value;
  }
  else
    return -999999;
}

fortdouble RG2NLAT(gribProduct ** grib) {
fortdouble value = (fortdouble) IG2NLAT(grib);
  if( value != -999999.0 )
    return (value/1000.0);
  else
    return value;
}

fortint IG2SLAT(gribProduct ** grib) {
fortint value;
  if( ecmwfLocalDefinition2Present((*grib)) ) {
    value = g1_2_domainS( (*grib) );
    if( value & 0x800000 ) value = - (value & 0x7fffff);
    return value;
  }
  else
    return -999999;
}

fortdouble RG2SLAT(gribProduct ** grib) {
fortdouble value = (fortdouble) IG2SLAT(grib);
  if( value != -999999.0 )
    return (value/1000.0);
  else
    return value;
}

fortint IG2WLON(gribProduct ** grib) {
fortint value;
  if( ecmwfLocalDefinition2Present((*grib)) ) {
    value = g1_2_domainW( (*grib) );
    if( value & 0x800000 ) value = - (value & 0x7fffff);
    return value;
  }
  else
    return -999999;
}

fortdouble RG2WLON(gribProduct ** grib) {
fortdouble value = (fortdouble) IG2WLON(grib);
  if( value != -999999.0 )
    return (value/1000.0);
  else
    return value;
}

fortint IG2END(gribProduct ** grib) {
  if( ecmwfLocalDefinition2Present((*grib)) )
    return g1_2_end( (*grib) );
  else
    return -999999;
}

fortdouble RG2END(gribProduct ** grib) {
  return (fortdouble) IG2END(grib);
}

fortint IG2NUCL(gribProduct ** grib) {
  if( ecmwfLocalDefinition2Present((*grib)) )
    return g1_2_count( (*grib) );
  else
    return -999999;
}

fortdouble RG2NUCL(gribProduct ** grib) {
  return (fortdouble) IG2NUCL(grib);
}

fortint IG2OPCL(gribProduct ** grib) {
  if( ecmwfLocalDefinition2Present((*grib)) )
    return g1_2_operational( (*grib) );
  else
    return -999999;
}

fortdouble RG2OPCL(gribProduct ** grib) {
  return (fortdouble) IG2OPCL(grib);
}

fortint IG2STAR(gribProduct ** grib) {
  if( ecmwfLocalDefinition2Present((*grib)) )
    return g1_2_start( (*grib) );
  else
    return -999999;
}

fortdouble RG2STAR(gribProduct ** grib) {
  return (fortdouble) IG2STAR(grib);
}

fortint IG2LIST(gribProduct ** grib, fortint * array, fortint * size) {
gribProduct * g = *grib;
fortint loop, listLength;
unsigned char * p;

  if( !ecmwfLocalDefinition2Present(g) ) return -1;

  listLength =  g1_2_count(g);
  if( listLength > *size ) return -1;

  p = (unsigned char *) ((g->g1)->local.contents.def2.forecastNumberList);
  for( loop = 0; loop < listLength; loop++ ) {
    *(array+loop) = ONEBYTEINT(p);
    p++;
  }

  return listLength;

}
