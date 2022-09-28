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

#include "getsetValues.h"

void copyNameLoc(
  unsigned char* copy,
  unsigned char* original,
  int copyLength,
  int originalLength) {
int length;
unsigned char *p;

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

  copyNameLoc(Request,request,(int)sizeof(Request),(int)requestLength);

  index = binaryChopI(despatchInteger,listLength,Request);
  if( index < 0 )
    return -999999;
  else {
    return despatchInteger[index].get(grib);
  }
}

fortint getIntegerValue(
  gribProduct** grib,
  unsigned char* request ){

  return GETINT(grib,request,(long)strlen((char*)request));
}

fortdouble GETREAL(
  gribProduct** grib,
  unsigned char* request,
  long requestLength ) { 
int index;
int listLength = sizeof(despatchReal)/sizeof(despatchR);
unsigned char Request[80];

  copyNameLoc(Request,request,(int)sizeof(Request),(int)requestLength);

  index = binaryChopR(despatchReal,listLength,Request);
  if( index < 0 )
    return -999999.0;
  else {
    return despatchReal[index].get(grib);
  }
}

fortdouble getRealValue(
  gribProduct** grib,
  unsigned char* request ) {

  return GETREAL(grib,request,(long)strlen((char*)request));
}

fortint IGNWLAT(gribProduct** grib) {
fortdouble value = RGNWLAT(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGNWLON(gribProduct** grib) {
fortdouble value = RGNWLON(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGSELAT(gribProduct** grib) {
fortdouble value = RGSELAT(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGSELON(gribProduct** grib) {
fortdouble value = RGSELON(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGDI(gribProduct** grib) {
fortdouble value = RGDI(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGDJ(gribProduct** grib) {
fortdouble value = RGDJ(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGIP(gribProduct** grib) {
fortdouble value = RGIP(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGLATRP(gribProduct** grib) {
fortdouble value = RGLATRP(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGLONRP(gribProduct** grib) {
fortdouble value = RGLONRP(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGROTAT(gribProduct** grib) {
fortdouble value = RGROTAT(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGLATSP(gribProduct** grib) {
fortdouble value = RGLATSP(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGLONSP(gribProduct** grib) {
fortdouble value = RGLONSP(grib);

  if( value != -999999.0 )
    return (fortint)(1000.0 * value);
  else
    return -999999;
}

fortint IGSFACT(gribProduct** grib) {
fortdouble value = RGSFACT(grib);

  if( value != -999999.0 )
    return (fortint)(1000000.0 * value);
  else
    return -999999;
}

fortint IGREFVL(gribProduct** grib) {
  return (fortint)(1000000.0 * RGREFVL(grib));
}

fortint IGDSCAL(gribProduct** grib) {
  return (fortint)(1000000.0 * RGDSCAL(grib));
}

fortdouble RGLNGTH(gribProduct** grib) {
  return (fortdouble) IGLNGTH(grib);
}

fortdouble RGTABLE(gribProduct** grib) {
  return (fortdouble) IGTABLE(grib);
}

fortdouble RGCENTR(gribProduct** grib) {
  return (fortdouble) IGCENTR(grib);
}

fortdouble RGPARAM(gribProduct** grib) {
  return (fortdouble) IGPARAM(grib);
}

fortdouble RGLEVEL(gribProduct** grib) {
  return (fortdouble) IGLEVEL(grib);
}

fortdouble RGLEVL1(gribProduct** grib) {
  return (fortdouble) IGLEVL1(grib);
}

fortdouble RGLEVL2(gribProduct** grib) {
  return (fortdouble) IGLEVL2(grib);
}

fortdouble RGDATE(gribProduct** grib) {
  return (fortdouble) IGDATE(grib);
}

fortdouble RGTIME(gribProduct** grib) {
  return (fortdouble) IGTIME(grib);
}

fortdouble RGSTEP(gribProduct** grib) {
  return (fortdouble) IGSTEP(grib);
}

fortdouble RGSTEP1(gribProduct ** grib) {
  return (fortdouble) IGSTEP1(grib);
}

fortdouble RGSTEP2(gribProduct ** grib) {
  return (fortdouble) IGSTEP2(grib);
}

fortdouble RGDEFIN(gribProduct** grib) {
  return (fortdouble) IGDEFIN(grib);
}

fortdouble RGCLASS(gribProduct** grib) {
  return (fortdouble) IGCLASS(grib);
}

fortdouble RGTYPE(gribProduct** grib) {
  return (fortdouble) IGTYPE(grib);
}

fortdouble RGSTREM(gribProduct** grib) {
  return (fortdouble) IGSTREM(grib);
}

fortdouble RGEXPVR(gribProduct** grib) {
  return (fortdouble) IGEXPVR(grib);
}

fortdouble RGNUMPV(gribProduct** grib) {
  return (fortdouble) IGNUMPV(grib);
}

fortdouble RGREPRS(gribProduct** grib) {
  return (fortdouble) IGREPRS(grib);
}

fortdouble RGNI(gribProduct** grib) {
  return (fortdouble) IGNI(grib);
}

fortdouble RGNJ(gribProduct** grib) {
  return (fortdouble) IGNJ(grib);
}

fortdouble RGRESCO(gribProduct** grib) {
  return (fortdouble) IGRESCO(grib);
}

fortdouble RGGAUSS(gribProduct** grib) {
  return (fortdouble) IGGAUSS(grib);
}

fortdouble RGSCANM(gribProduct** grib) {
  return (fortdouble) IGSCANM(grib);
}

fortdouble RGJ(gribProduct** grib) {
  return (fortdouble) IGJ(grib);
}

fortdouble RGK(gribProduct** grib) {
  return (fortdouble) IGK(grib);
}

fortdouble RGM(gribProduct** grib) {
  return (fortdouble) IGM(grib);
}

fortdouble RGREPMO(gribProduct** grib) {
  return (fortdouble) IGREPMO(grib);
}

fortdouble RGTJ(gribProduct** grib) {
  return (fortdouble) IGTJ(grib);
}

fortdouble RGTK(gribProduct** grib) {
  return (fortdouble) IGTK(grib);
}

fortdouble RGTM(gribProduct** grib) {
  return (fortdouble) IGTM(grib);
}

fortdouble RGBTSPV(gribProduct** grib) {
  return (fortdouble) IGBTSPV(grib);
}

fortdouble RGUNUSD(gribProduct** grib) {
  return (fortdouble) IGUNUSD(grib);
}

fortdouble RGNVALU(gribProduct** grib) {
  return (fortdouble) IGNVALU(grib);
}

int binaryChopI(despatchI * list, int listLength, unsigned char * test) {
int current, oldCurrent = -1;
int direction, startRange = 0, endRange = listLength;

  do {
    current = (startRange + endRange)/2;

    if( current == oldCurrent) break;

    direction = strcmp((char*)test,(char*)(*(list+current)).name);

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

    direction = strcmp((char*)test,(char*)(*(list+current)).name);

    if( direction == 0 ) return current;

    if( direction < 0 )
      endRange = current;
    else
      startRange = current;

    oldCurrent = current;
  } while( startRange <= endRange );

  return -1;
}

fortint setIntegerValue(
  gribProduct** grib,
  unsigned char* request,
  fortint * value )
{
  return SETINT(grib,request,value,(long)strlen((char*)request));
}

fortint SETINT(
  gribProduct** grib,
  unsigned char* request,
  fortint * value,
  long requestLength )
{ 
int index;
int listLength = sizeof(despatchInteger)/sizeof(despatchI);
unsigned char Request[80];

  copyNameLoc(Request,request,(int)sizeof(Request),(int)requestLength);

  index = binaryChopI(despatchInteger,listLength,Request);
  if( index < 0 )
    return -999999;
  else {
    despatchInteger[index].set(grib,value);
    return 0;
  }
}

fortint setRealValue(
  gribProduct** grib,
  unsigned char* request,
  fortdouble * value ) {

  return SETREAL(grib,request,value,strlen((char*)request));
}

fortint SETREAL(
  gribProduct** grib,
  unsigned char* request,
  fortdouble * value,
  long requestLength ) { 
int index;
int listLength = sizeof(despatchReal)/sizeof(despatchR);
unsigned char Request[80];

  copyNameLoc(Request,request,(int)sizeof(Request),(int)requestLength);

  index = binaryChopR(despatchReal,listLength,Request);
  if( index < 0 )
    return -999999.0;
  else {
    despatchReal[index].set(grib,value);
    return 0;
  }
}
