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
#ifndef ECMWFDEFNS_H
#define ECMWFDEFNS_H

#include "common/fortint.h"

void encodeMarsPart(fortint * inputArray, unsigned char * gribSection1);

void decodeMarsPart(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_1(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_1(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_2(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_2(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_3(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_3(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_4(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint d_def_4(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint e_def_5(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_5(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_6(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint d_def_6(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint e_def_7(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_7(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_8(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_8(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_9(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_9(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_10(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint d_def_10(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint e_def_11(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_11(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_13(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint d_def_13(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint e_def_14(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_14(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_15(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_15(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_16(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_16(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_17(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint d_def_17(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint e_def_18(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_18(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_19(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_19(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_20(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_20(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_21(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_21(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_22(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_22(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_23(
fortint * inputArray, unsigned char * gribSection1, fortint * bitPointer);

fortint d_def_23(
fortint * outputArray, unsigned char * gribSection1, fortint * bitPointer);

fortint e_def_50(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_50(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_190(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * collectedLocalDefinitionBytes,
  fortint * bitPointer);

fortint d_def_190(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * collectedLocalDefinitionBytes,
  fortint * bitPointer);

fortint e_def_191(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint d_def_191(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint e_def_x(
  fortint * definitionNumber,
  fortint * outputArray,
  unsigned char * gribSection1 );

fortint d_def_x(
  fortint * definitionNumber,
  fortint * outputArray,
  unsigned char * gribSection1 );

fortint jmemove(
  fortint * destination,
  fortint * destinationOffset,
  fortint * source,
  fortint * sourceOffset,
  fortint * length );

fortint e_def_1_(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_1_(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_2_(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_2_(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_3_(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_3_(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_4_(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint d_def_4_(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint e_def_5_(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_5_(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_6_(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint d_def_6_(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint e_def_7_(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_7_(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_8_(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_8_(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_9_(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_9_(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_10_(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint d_def_10_(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint e_def_11_(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_11_(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_13_(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint d_def_13_(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint e_def_14_(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_14_(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_15_(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_15_(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_16_(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_16_(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_17_(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint d_def_17_(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint e_def_18_(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_18_(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_19_(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_19_(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_20_(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_20_(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_21_(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_21_(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_50_(fortint * inputArray, unsigned char * gribSection1);

fortint d_def_50_(fortint * outputArray, unsigned char * gribSection1);

fortint e_def_190_(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * collectedLocalDefinitionBytes,
  fortint * bitPointer);

fortint d_def_190_(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * collectedLocalDefinitionBytes,
  fortint * bitPointer);

fortint e_def_191_(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint d_def_191_(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer);

fortint e_def_x(
  fortint * definitionNumber,
  fortint * outputArray,
  unsigned char * gribSection1 );

fortint d_def_x(
  fortint * definitionNumber,
  fortint * outputArray,
  unsigned char * gribSection1 );

fortint jmemove_(
  fortint * destination,
  fortint * destinationOffset,
  fortint * source,
  fortint * sourceOffset,
  fortint * length );

#define ONEBYTEINT(a)   (fortint) ( *(a) )
#define TWOBYTEINT(a)   (fortint) ( (*(a))<<8 | (*((a)+1))<<0 )
#define THREEBYTEINT(a) (fortint) (TWOBYTEINT((a))<<8 | (*((a)+2))<<0 )
#define FOURBYTEINT(a)  (fortint) (THREEBYTEINT((a))<<8 | (*((a)+3))<<0 )

#define ONEBYTE(n)      ((*(n)>> 0) & 0xFF)
#define MOVE1BYTE(p,n)  ( *(p)     = ((*(n)>> 0) & 0xFF) )
#define MOVE2BYTES(p,n) ( *(p)     = ((*(n)>> 8) & 0xFF) ) , \
                        ( *((p)+1) = ((*(n)>> 0) & 0xFF) )
#define MOVE3BYTES(p,n) ( *(p)     = ((*(n)>>16) & 0xFF) ) , \
                        ( *((p)+1) = ((*(n)>> 8) & 0xFF) ) , \
                        ( *((p)+2) = ((*(n)>> 0) & 0xFF) )
#define MOVE4BYTES(p,n) ( *(p)     = ((*(n)>>24) & 0xFF) ) , \
                        ( *((p)+1) = ((*(n)>>16) & 0xFF) ) , \
                        ( *((p)+2) = ((*(n)>> 8) & 0xFF) ) , \
                        ( *((p)+3) = ((*(n)>> 0) & 0xFF) )

#endif /* End of ECMWFDEFNS_H */
