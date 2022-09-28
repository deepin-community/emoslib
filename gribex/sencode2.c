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

#include "sencode2.h"

fortint IS2NUMB(gribProduct ** grib, fortint * number) {
gribProduct * g = *grib;

  if( !( ecmwfLocalDefinitionPresent(g) && (g1_definition(g)==2) ) )
    return (fortint) -1;

  MOVE1BYTE(((g->g1)->local.contents.def2.number),number);
  return 0;
}

fortint RS2NUMB(gribProduct ** grib, fortdouble *number) {
fortint Number = (fortint) *number;
  return IS2NUMB(grib,&Number);
}

fortint IS2TOTL(gribProduct ** grib, fortint * total) {
gribProduct * g = *grib;

  if( !( ecmwfLocalDefinitionPresent(g) && (g1_definition(g)==2) ) )
    return (fortint) -1;

  MOVE1BYTE(((g->g1)->local.contents.def2.total),total);
  return 0;
}

fortint RS2TOTL(gribProduct ** grib, fortdouble *total) {
fortint Total = (fortint) *total;
  return IS2TOTL(grib,&Total);
}

fortint IS2METH(gribProduct ** grib, fortint * method) {
gribProduct * g = *grib;

  if( !( ecmwfLocalDefinitionPresent(g) && (g1_definition(g)==2) ) )
    return (fortint) -1;

  MOVE1BYTE(((g->g1)->local.contents.def2.method),method);
  return 0;
}

fortint RS2METH(gribProduct ** grib, fortdouble *method) {
fortint Method = (fortint) *method;
  return IS2METH(grib,&Method);
}

fortint IS2STAR(gribProduct ** grib, fortint * startTimestep) {
gribProduct * g = *grib;

  if( !( ecmwfLocalDefinitionPresent(g) && (g1_definition(g)==2) ) )
    return (fortint) -1;

  MOVE2BYTES(((g->g1)->local.contents.def2.startTimestep),startTimestep);
  return 0;
}

fortint RS2STAR(gribProduct ** grib, fortdouble *startTimestep) {
fortint StartTimestep = (fortint) *startTimestep;
  return IS2STAR(grib,&StartTimestep);
}

fortint IS2END(gribProduct ** grib, fortint * endTimestep) {
gribProduct * g = *grib;

  if( !( ecmwfLocalDefinitionPresent(g) && (g1_definition(g)==2) ) )
    return (fortint) -1;

  MOVE2BYTES(((g->g1)->local.contents.def2.endTimestep),endTimestep);
  return 0;
}

fortint RS2END(gribProduct ** grib, fortdouble *endTimestep) {
fortint EndTimestep = (fortint) *endTimestep;
  return IS2END(grib,&EndTimestep);
}

fortint IS2NLAT(gribProduct ** grib, fortint * domainNorthLatitude) {
gribProduct * g = *grib;
fortint value = *domainNorthLatitude;

  if( !( ecmwfLocalDefinitionPresent(g) && (g1_definition(g)==2) ) )
    return (fortint) -1;

  if( value < 0 ) value = 0x800000 | (-value);
  MOVE3BYTES(((g->g1)->local.contents.def2.domainNorthLatitude),&value);
  return 0;
}

fortint RS2NLAT(gribProduct ** grib, fortdouble *domainNorthLatitude) {
fortint DomainNorthLatitude = (fortint) (*domainNorthLatitude*1000.0);
  return IS2NLAT(grib,&DomainNorthLatitude);
}

fortint IS2WLON(gribProduct ** grib, fortint * domainWestLongitude) {
gribProduct * g = *grib;
fortint value = *domainWestLongitude;

  if( !( ecmwfLocalDefinitionPresent(g) && (g1_definition(g)==2) ) )
    return (fortint) -1;

  if( value < 0 ) value = 0x800000 | (-value);
  MOVE3BYTES(((g->g1)->local.contents.def2.domainWestLongitude),&value);
  return 0;
}

fortint RS2WLON(gribProduct ** grib, fortdouble *domainWestLongitude) {
fortint DomainWestLongitude = (fortint) (*domainWestLongitude*1000.0);
  return IS2WLON(grib,&DomainWestLongitude);
}

fortint IS2SLAT(gribProduct ** grib, fortint * domainSouthLatitude) {
gribProduct * g = *grib;
fortint value = *domainSouthLatitude;

  if( !( ecmwfLocalDefinitionPresent(g) && (g1_definition(g)==2) ) )
    return (fortint) -1;

  if( value < 0 ) value = 0x800000 | (-value);
  MOVE3BYTES(((g->g1)->local.contents.def2.domainSouthLatitude),&value);
  return 0;
}

fortint RS2SLAT(gribProduct ** grib, fortdouble *domainSouthLatitude) {
fortint DomainSouthLatitude = (fortint) (*domainSouthLatitude*1000.0);
  return IS2SLAT(grib,&DomainSouthLatitude);
}

fortint IS2ELON(gribProduct ** grib, fortint * domainEastLongitude) {
gribProduct * g = *grib;
fortint value = *domainEastLongitude;

  if( !( ecmwfLocalDefinitionPresent(g) && (g1_definition(g)==2) ) )
    return (fortint) -1;

  if( value < 0 ) value = 0x800000 | (-value);
  MOVE3BYTES(((g->g1)->local.contents.def2.domainEastLongitude),&value);
  return 0;
}

fortint RS2ELON(gribProduct ** grib, fortdouble *domainEastLongitude) {
fortint DomainEastLongitude = (fortint) (*domainEastLongitude*1000.0);
  return IS2ELON(grib,&DomainEastLongitude);
}

fortint IS2OPCL(gribProduct ** grib, fortint * operationalForecastCluster) {
gribProduct * g = *grib;

  if( !( ecmwfLocalDefinitionPresent(g) && (g1_definition(g)==2) ) )
    return (fortint) -1;

  MOVE1BYTE(((g->g1)->local.contents.def2.operationalForecastCluster),operationalForecastCluster);
    return 0;
}

fortint RS2OPCL(gribProduct ** grib, fortdouble *operationalForecastCluster) {
fortint OperationalForecastCluster = (fortint) *operationalForecastCluster;
  return IS2OPCL(grib,&OperationalForecastCluster);
}

fortint IS2CFCL(gribProduct ** grib, fortint * controlForecastCluster) {
gribProduct * g = *grib;

  if( !( ecmwfLocalDefinitionPresent(g) && (g1_definition(g)==2) ) )
    return (fortint) -1;

  MOVE1BYTE(((g->g1)->local.contents.def2.controlForecastCluster),controlForecastCluster);
    return 0;
}

fortint RS2CFCL(gribProduct ** grib, fortdouble *controlForecastCluster) {
fortint ControlForecastCluster = (fortint) *controlForecastCluster;
  return IS2CFCL(grib,&ControlForecastCluster);
}

fortint IS2NUCL(gribProduct ** grib, fortint * numberInCluster) {
gribProduct * g = *grib;

  if( !( ecmwfLocalDefinitionPresent(g) && (g1_definition(g)==2) ) )
    return (fortint) -1;

  MOVE1BYTE(((g->g1)->local.contents.def2.numberInCluster),numberInCluster);
    return 0;
}

fortint RS2NUCL(gribProduct ** grib, fortdouble *numberInCluster) {
fortint NumberInCluster = (fortint) *numberInCluster;
  return IS2NUCL(grib,&NumberInCluster);
}

fortint IS2LIST(gribProduct ** grib, fortint * array, fortint * size) {
gribProduct * g = *grib;
fortint loop, listLength;
unsigned char * p;

  if( !ecmwfLocalDefinition2Present(g) ) return -1;

  listLength =  g1_2_count(g);
  if( listLength > *size ) return -1;

  p = (unsigned char *) ((g->g1)->local.contents.def2.forecastNumberList);
  for( loop = 0; loop < listLength; loop++ ) {
    MOVE1BYTE(p,(array+loop));
    p++;
  }

  return listLength;

}
