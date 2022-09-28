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

#include <stdio.h>
#include <string.h>
#include "getValues.h"

#ifdef FORTRAN_NO_UNDERSCORE
#define GETINT  getint
#define GETREAL getreal
#else
#define GETINT  getint_
#define GETREAL getreal_
#endif

fortint getIntegerValue(
  gribProduct** grib,
  unsigned char* request ){

  return GETINT(grib,request,strlen(request));
}

void copyName(
  unsigned char* copy,
  unsigned char* original,
  int copyLength,
  int originalLength) {
int length;
char *p;

  length = (originalLength>copyLength)?copyLength:originalLength;
  memcpy(copy,original,length);
  copy[length] = '\0';

  p = copy + length - 1;
  while( *p == ' ' ) *(p--) = '\0';

  return;
}

fortint GETINT(
  gribProduct** grib,
  unsigned char* request,
  long requestLength ) { 
int index;
int listLength = sizeof(despatchInteger)/sizeof(despatchI);
unsigned char Request[80];

  copyName(Request,request,(int)sizeof(Request),(int)requestLength);

  index = binaryChopI(despatchInteger,listLength,Request);
  if( index < 0 )
    return -999999;
  else {
    return despatchInteger[index].get(grib);
  }
}

fortdouble getRealValue(
  gribProduct** grib,
  unsigned char* request ) {

  return GETREAL(grib,request,strlen(request));
}

fortdouble GETREAL(
  gribProduct** grib,
  unsigned char* request,
  long requestLength ) { 
int index;
int listLength = sizeof(despatchReal)/sizeof(despatchR);
char Request[80];

  copyName(Request,request,(int)sizeof(Request),(int)requestLength);

  index = binaryChopR(despatchReal,listLength,Request);
  if( index < 0 )
    return -999999.0;
  else {
    return despatchReal[index].get(grib);
  }
}

fortint IGNWLAT(gribProduct** grib) {
fortdouble value = GNWLAT(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGNWLON(gribProduct** grib) {
fortdouble value = GNWLON(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGSELAT(gribProduct** grib) {
fortdouble value = GSELAT(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGSELON(gribProduct** grib) {
fortdouble value = GSELON(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGDI(gribProduct** grib) {
fortdouble value = GDI(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGDJ(gribProduct** grib) {
fortdouble value = GDJ(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGIP(gribProduct** grib) {
fortdouble value = GIP(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGLATRPL(gribProduct** grib) {
fortdouble value = GLATRPL(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGLONRPL(gribProduct** grib) {
fortdouble value = GLONRPL(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGROTATN(gribProduct** grib) {
fortdouble value = GROTATN(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGLATSPL(gribProduct** grib) {
fortdouble value = GLATSPL(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGLONSPL(gribProduct** grib) {
fortdouble value = GLONSPL(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGSFACTR(gribProduct** grib) {
fortdouble value = GSFACTR(grib);

  if( value != -999999.0 )
    return (fortint)(1000000.0 * value);
  else
    return -999999;
}

fortint IGREFVAL(gribProduct** grib) {
  return (fortint)(1000000.0 * GREFVAL(grib));
}

fortint IGDSCALE(gribProduct** grib) {
  return (fortint)(1000000.0 * GDSCALE(grib));
}

fortdouble RGLENGTH(gribProduct** grib) {
  return (fortdouble) GLENGTH(grib);
}

fortdouble RGTABLE(gribProduct** grib) {
  return (fortdouble) GTABLE(grib);
}

fortdouble RGCENTRE(gribProduct** grib) {
  return (fortdouble) GCENTRE(grib);
}

fortdouble RGPARAM(gribProduct** grib) {
  return (fortdouble) GPARAM(grib);
}

fortdouble RGLEVEL(gribProduct** grib) {
  return (fortdouble) GLEVEL(grib);
}

fortdouble RGLEVEL1(gribProduct** grib) {
  return (fortdouble) GLEVEL1(grib);
}

fortdouble RGLEVEL2(gribProduct** grib) {
  return (fortdouble) GLEVEL2(grib);
}

fortdouble RGDATE(gribProduct** grib) {
  return (fortdouble) GDATE(grib);
}

fortdouble RGTIME(gribProduct** grib) {
  return (fortdouble) GTIME(grib);
}

fortdouble RGSTEP(gribProduct** grib) {
  return (fortdouble) GSTEP(grib);
}

fortdouble RGDEFIN(gribProduct** grib) {
  return (fortdouble) GDEFIN(grib);
}

fortdouble RGCLASS(gribProduct** grib) {
  return (fortdouble) GCLASS(grib);
}

fortdouble RGTYPE(gribProduct** grib) {
  return (fortdouble) GTYPE(grib);
}

fortdouble RGSTREAM(gribProduct** grib) {
  return (fortdouble) GSTREAM(grib);
}

fortdouble RGEXPVER(gribProduct** grib) {
  return (fortdouble) GEXPVER(grib);
}

fortdouble RGNUMPV(gribProduct** grib) {
  return (fortdouble) GNUMPV(grib);
}

fortdouble RGREPRES(gribProduct** grib) {
  return (fortdouble) GREPRES(grib);
}

fortdouble RGNI(gribProduct** grib) {
  return (fortdouble) GNI(grib);
}

fortdouble RGNJ(gribProduct** grib) {
  return (fortdouble) GNJ(grib);
}

fortdouble RGRESCOM(gribProduct** grib) {
  return (fortdouble) GRESCOM(grib);
}

fortdouble RGNUMBER(gribProduct** grib) {
  return (fortdouble) GNUMBER(grib);
}

fortdouble RGSCAN(gribProduct** grib) {
  return (fortdouble) GSCAN(grib);
}

fortdouble RGJ(gribProduct** grib) {
  return (fortdouble) GJ(grib);
}

fortdouble RGK(gribProduct** grib) {
  return (fortdouble) GK(grib);
}

fortdouble RGM(gribProduct** grib) {
  return (fortdouble) GM(grib);
}

fortdouble RGREPMOD(gribProduct** grib) {
  return (fortdouble) GREPMOD(grib);
}

fortdouble RGTJ(gribProduct** grib) {
  return (fortdouble) GTJ(grib);
}

fortdouble RGTK(gribProduct** grib) {
  return (fortdouble) GTK(grib);
}

fortdouble RGTM(gribProduct** grib) {
  return (fortdouble) GTM(grib);
}

fortdouble RGBITSPV(gribProduct** grib) {
  return (fortdouble) GBITSPV(grib);
}

fortdouble RGUNUSED(gribProduct** grib) {
  return (fortdouble) GUNUSED(grib);
}

fortdouble RGNVALUE(gribProduct** grib) {
  return (fortdouble) GNVALUE(grib);
}

int binaryChopI(despatchI * list, int listLength, unsigned char * test) {
int current, oldCurrent = -1;
int direction, startRange = 0, endRange = listLength;

  do {
    current = (startRange + endRange)/2;

    if( current == oldCurrent) break;

    direction = strcmp(test,(*(list+current)).name);

    if( direction == 0 ) return current;

    if( direction < 0 )
      endRange = current;
    else
      startRange = current;

    oldCurrent = current;
  } while( startRange <= endRange );

  return -1;
}

int binaryChopR(despatchR * list, int listLength, unsigned char * test) {
int current, oldCurrent = -1;
int direction, startRange = 0, endRange = listLength;

  do {
    current = (startRange + endRange)/2;

    if( current == oldCurrent) break;

    direction = strcmp(test,(*(list+current)).name);

    if( direction == 0 ) return current;

    if( direction < 0 )
      endRange = current;
    else
      startRange = current;

    oldCurrent = current;
  } while( startRange <= endRange );

  return -1;
}
