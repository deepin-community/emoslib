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
#include <memory.h>

#include "ECMWFdefinitions.h"

void encodeMarsPart(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes fixed part of ECMWF local definition 1 (from byte 41 onwards)
// into gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition1.html
*/
fortint * n = inputArray;
unsigned char * p = gribSection1;

  *p++ = ONEBYTE(n);n++;          /* DEFINITION */
  *p++ = ONEBYTE(n);n++;          /* CLASS */
  *p++ = ONEBYTE(n);n++;          /* TYPE */

  MOVE2BYTES(p,n);                /* STREAM */
  p+=2;
  n++;
#ifdef INTEGER_8
  memcpy(p,((char *)n+4),4);      /* EXPVER */
#else
  memcpy(p,(char *)n,4);          /* EXPVER */
#endif
  p += 4;
  n++;

  return;
}

void decodeMarsPart(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes fixed part of ECMWF local definition 1 (from byte 41 onwards)
// from gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition1.html
*/
fortint * n = outputArray;
unsigned char * p = gribSection1;

  *n++ = ONEBYTEINT(p); p++;      /* DEFINITION */
  *n++ = ONEBYTEINT(p); p++;      /* CLASS */
  *n++ = ONEBYTEINT(p); p++;      /* TYPE */
  *n++ = TWOBYTEINT(p); p += 2;   /* STREAM */
#ifdef INTEGER_8
  memcpy(((char *)n+4),p,4);      /* EXPVER */
#else
  memcpy((char *)n,p,4);          /* EXPVER */
#endif
  n++; p+= 4;

  return;
}


fortint e_def_1_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 1 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition1.html
//
// Returns the number of bytes (octets) in local definition 1 after
// byte 40.
*/
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = ONEBYTE(n);n++;          /* NUMBER */
  *p++ = ONEBYTE(n);n++;          /* TOTAL */
  *p++ = (unsigned char) 0;       /* UNUSED */

/*
// Definition 1 has a fixed length section 1 (52 bytes)
*/
  return (fortint) (52-40);
}

fortint e_def_1(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_1_(inputArray,gribSection1);
}

fortint d_def_1_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 1 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition1.html
//
// Returns the number of array elements inserted in outputArray
*/
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = ONEBYTEINT(p);p++;      /* NUMBER */
  *n++ = ONEBYTEINT(p);p++;      /* TOTAL */

  return (fortint) (n - outputArray);
}

fortint d_def_1(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_1_(outputArray,gribSection1);
}

fortint e_def_2_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 2 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition2.html
//
// Returns the number of bytes (octets) in local definition 2 after
// byte 40.
*/
int loop, N;
fortint value;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = ONEBYTE(n);n++;          /* NUMBER */
  *p++ = ONEBYTE(n);n++;          /* TOTAL */
  *p++ = (unsigned char) 0;       /* UNUSED */
  *p++ = ONEBYTE(n);n++;          /* METHOD */

  MOVE2BYTES(p,n);                /* STARTSTEP */
  p += 2;
  n++;
  MOVE2BYTES(p,n);                /* ENDSTEP */
  p += 2;
  n++;
/*
// When coding values, set sign bit to 1, if value is negative.
*/
  value = *n++;                   /* NORTH */
  if( value < 0 ) value = 0x800000 | (-value);
  MOVE3BYTES(p,&value);
  p += 3;

  value = *n++;                   /* WEST */
  if( value < 0 ) value = 0x800000 | (-value);
  MOVE3BYTES(p,&value);
  p += 3;

  value = *n++;                   /* SOUTH */
  if( value < 0 ) value = 0x800000 | (-value);
  MOVE3BYTES(p,&value);
  p += 3;

  value = *n++;                   /* EAST */
  if( value < 0 ) value = 0x800000 | (-value);
  MOVE3BYTES(p,&value);
  p += 3;

  *p++ = ONEBYTE(n);n++;          /* FORECAST */
  *p++ = ONEBYTE(n);n++;          /* CONTROL */

  N = *n;                         /* NUMBERINCLUSTER */
  *p++ = ONEBYTE(n++);

  for( loop = 0; loop < N; loop++)   /* LIST */
    *p++ = ONEBYTE(n++);
/*
// Definition 2 has a fixed length section 1 (328 bytes)
*/
  for( loop = N; loop < (328-72); loop++)
    *p++ = (unsigned char) 0;

  return (fortint) (328-40);
}

fortint e_def_2(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_2_(inputArray,gribSection1);
}

fortint d_def_2_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 2 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition2.html
//
// Returns the number of array elements inserted in outputArray
*/
int loop, N;
fortint value;
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = ONEBYTEINT(p);p++;      /* NUMBER */
  *n++ = ONEBYTEINT(p);p++;      /* TOTAL */
  p++;                           /* UNUSED */
  *n++ = ONEBYTEINT(p);p++;      /* METHOD */

  *n++ = TWOBYTEINT(p);          /* STARTSTEP */
  p += 2;
  *n++ = TWOBYTEINT(p);          /* ENDSTEP */
  p += 2;
/*
// When decoding values, if sign bit is 1, value is negative.
*/
  value = THREEBYTEINT(p);       /* NORTH */
  p += 3;
  if( value & 0x800000 ) value = - (value & 0x7fffff);
  *n++ = value;

  value = THREEBYTEINT(p);       /* WEST */
  p += 3;
  if( value & 0x800000 ) value = - (value & 0x7fffff);
  *n++ = value;

  value = THREEBYTEINT(p);       /* SOUTH */
  p += 3;
  if( value & 0x800000 ) value = - (value & 0x7fffff);
  *n++ = value;

  value = THREEBYTEINT(p);       /* EAST */
  p += 3;
  if( value & 0x800000 ) value = - (value & 0x7fffff);
  *n++ = value;

  *n++ = ONEBYTEINT(p);p++;      /* FORECAST */
  *n++ = ONEBYTEINT(p);p++;      /* CONTROL */

  N = ONEBYTEINT(p);
  *n++ = ONEBYTEINT(p);p++;      /* NUMBERINCLUSTER */

  for( loop = 0; loop < N; loop++) /* LIST */
    *n++ = ONEBYTEINT(p++);

  return (fortint) (n - outputArray);
}

fortint d_def_2(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_2_(outputArray,gribSection1);
}

fortint e_def_3_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 3 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition3.html
//
// Returns the number of bytes (octets) in local definition 3 after
// byte 40.
*/
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = ONEBYTE(n);n++;          /* BAND */
  *p++ = ONEBYTE(n);n++;          /* FUNCTION */
  *p++ = (unsigned char) 0;       /* RESERVED */

  return (fortint) (p-gribSection1);
}

fortint e_def_3(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_3_(inputArray,gribSection1);
}

fortint d_def_3_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 3 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition3.html
//
// Returns the number of array elements inserted in outputArray
*/
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = ONEBYTEINT(p);p++;      /* BAND */
  *n++ = ONEBYTEINT(p);p++;      /* FUNCTION */

  return (fortint) (n - outputArray);
}

fortint d_def_3(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_3_(outputArray,gribSection1);
}

void encodeNonMarsPartOfDefinition4(
  fortint** nPointer,
  unsigned char** pPointer) {
fortint* n = *nPointer;
unsigned char* p = *pPointer;
int loop;
int N, M, I, J, K;
int zero = 0;
fortint value;

/*
//  Coordinate structure definition
*/
  *p++ = ONEBYTE(n);n++;          /* SPACEUNITFLAG */
  *p++ = ONEBYTE(n);n++;          /* VERTCOORDDEFN */
  *p++ = ONEBYTE(n);n++;          /* HORIZCOORDDEFN */
  *p++ = ONEBYTE(n);n++;          /* TIMEUNITFLAG */
  *p++ = ONEBYTE(n);n++;          /* TIMECOORDDEFN */
/*
// Mixed coordinates
// For positions, set sign bit to 1, if value is negative
*/
  *p++ = ONEBYTE(n);n++;          /* MIXEDCOORDFLAG */
  *p++ = ONEBYTE(n);n++;          /* COORD1FLAG */
  *p++ = ONEBYTE(n);n++;          /* AVERAGINGFLAG1 */

  value = *n++;                   /* POSITION1LEVEL1 */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  value = *n++;                   /* POSITION1LEVEL2 */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  *p++ = ONEBYTE(n);n++;          /* COORD2FLAG */
  *p++ = ONEBYTE(n);n++;          /* AVERAGINGFLAG2 */

  value = *n++;                   /* POSITION2LEVEL1 */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  value = *n++;                   /* POSITION2LEVEL2 */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;
/*
// Data grid definitions
// For coordinates and increments, set sign bit to 1, if value is negative
*/
  *p++ = ONEBYTE(n);n++;          /* COORD3FLAG */
  *p++ = ONEBYTE(n);n++;          /* COORD4FLAG */

  value = *n++;                   /* COORD4FIRSTPT */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  value = *n++;                   /* COORD3FIRSTPT */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  value = *n++;                   /* COORD4LASTPT */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  value = *n++;                   /* COORD3LASTPT */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  value = *n++;                   /* I_INCREMENT */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  value = *n++;                   /* J_INCREMENT */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  *p++ = ONEBYTE(n);n++;          /* FLAGIRREGGRIDLIST */
  *p++ = ONEBYTE(n);n++;          /* FLAGOTHERGRIDLIST */
/*
// Auxiliary information
*/
  *p++ = ONEBYTE(n);n++;          /* FLAGOTHERINFO */

  N = *n;                         /* NUMBERHORIZCOORDS */
  *p++ = ONEBYTE(n++);

  M = *n;                         /* NUMBERMIXEDCOORDS */
  MOVE2BYTES(p,n);
  p += 2;
  n++;

  I = *n;                         /* NUMBERGRIDCOORDS */
  MOVE2BYTES(p,n);
  p += 2;
  n++;

  J = *n;                         /* NUMBERAUXILIARY */
  MOVE2BYTES(p,n);
  p += 2;
  n++;
/*
// Horizontal coordinate definition 
// For coordinates, set sign bit to 1, if value is negative
*/
  for( loop = 0; loop < N; loop++) {     /* AUXILIARY ARRAY */
    value = *n++;  
    if( value < 0 ) value = 0x80000000 | (-value);
    MOVE4BYTES(p,&value);
    p += 4;
  }
/*
// Mixed coordinate definition
// For coordinates, set sign bit to 1, if value is negative
*/
  for( loop = 0; loop < M; loop++) {
    value = *n++;  
    if( value < 0 ) value = 0x80000000 | (-value);
    MOVE4BYTES(p,&value);
    p += 4;
  }
/*
// Grid coordinate list
// For coordinates, set sign bit to 1, if value is negative
*/
  for( loop = 0; loop < I; loop++) {
    value = *n++;
    if( value < 0 ) value = 0x80000000 | (-value);
    MOVE4BYTES(p,&value);
    p += 4;
  }
/*
// Auxiliary array
*/
  for( loop = 0; loop < J; loop++) {
    MOVE4BYTES(p,n);
    p += 4;
    n++;
  }
/*
// Post-auxiliary array
*/
  K = *n;
  MOVE4BYTES(p,n);
  p += 4;
  n++;
  for( loop = 1; loop < K; loop++) {
    MOVE4BYTES(p,n);
    p += 4;
    n++;
  }

  *nPointer = n;
  *pPointer = p;
  return;
}

fortint e_def_4_(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {
/*
// Encodes ECMWF local definition 4 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition4.html
//
// If *bitPointer is not equal to zero, the length of the GRIB section
// 1 is stored in 3 bytes starting at (gribSection1-40).
// *bitPointer is updated by the number of bits in local definition 4.
//
// Returns the number of bytes (octets) in local definition 4 after
// byte 40.
*/
fortint byteLength, streamNumber, zero = 0;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  streamNumber = *(n-2);
  if( streamNumber == 1090 ) {
    MOVE2BYTES(p,n);              /* NUMBER in 2 bytes for stream 1090 */
    p += 2;
    n +=2;                        /* ZERO */
  }
  else {
    *p++ = ONEBYTE(n);n++;        /* NUMBER */
    *p++ = zero; n++;             /* ZERO */
  }
  *p++ = (unsigned char) 1;       /* POST-AUXILIARY FLAG */
  *p++ = ONEBYTE(n);n++;          /* SYSTEM */
  *p++ = ONEBYTE(n);n++;          /* METHOD */

  encodeNonMarsPartOfDefinition4(&n, &p);

  byteLength = 40 + (fortint) (p-gribSection1);
  if( *bitPointer ) {
    MOVE3BYTES(gribSection1-40,&byteLength);
    *bitPointer += (byteLength-40)*8;
  }

  return (fortint) byteLength;
}

fortint e_def_4( fortint * inputArray,
                 unsigned char * gribSection1,
                 fortint * bitPointer) {
  return e_def_4_(inputArray,gribSection1,bitPointer);
}

void decodeNonMarsPartOfDefinition4(
  fortint postAuxiliaryArrayFlag,
  fortint** nPointer,
  unsigned char** pPointer) {
fortint* n = *nPointer;
unsigned char* p = *pPointer;
int loop;
int N, M, I, J, K;
fortint value;

/*
//  Coordinate structure definition
*/
  *n++ = ONEBYTEINT(p);p++;       /* SPACEUNITFLAG */
  *n++ = ONEBYTEINT(p);p++;       /* VERTCOORDDEFN */
  *n++ = ONEBYTEINT(p);p++;       /* HORIZCOORDDEFN */
  *n++ = ONEBYTEINT(p);p++;       /* TIMEUNITFLAG */
  *n++ = ONEBYTEINT(p);p++;       /* TIMECOORDDEFN */
/*
// Mixed Coordinates
// For positions, if sign bit is 1, value is negative
*/
  *n++ = ONEBYTEINT(p);p++;       /* MIXEDCOORDFLAG */
  *n++ = ONEBYTEINT(p);p++;       /* COORD1FLAG */
  *n++ = ONEBYTEINT(p);p++;       /* AVERAGINGFLAG1 */

  value = FOURBYTEINT(p);         /* POSITION1LEVEL1 */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  value = FOURBYTEINT(p);         /* POSITION1LEVEL2 */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  *n++ = ONEBYTEINT(p);p++;       /* COORD2FLAG */
  *n++ = ONEBYTEINT(p);p++;       /* AVERAGINGFLAG2 */

  value = FOURBYTEINT(p);         /* POSITION2LEVEL1 */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  value = FOURBYTEINT(p);         /* POSITION2LEVEL2 */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;
/*
//  Data grid definitions
// For coordinates and increments, if sign bit is 1, value is negative
*/
  *n++ = ONEBYTEINT(p);p++;       /* COORD3FLAG */
  *n++ = ONEBYTEINT(p);p++;       /* COORD4FLAG */

  value = FOURBYTEINT(p);         /* COORD4FIRSTPT */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  value = FOURBYTEINT(p);         /* COORD3FIRSTPT */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  value = FOURBYTEINT(p);         /* COORD4LASTPT */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  value = FOURBYTEINT(p);         /* COORD3LASTPT */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  value = FOURBYTEINT(p);         /* I_INCREMENT */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  value = FOURBYTEINT(p);         /* J_INCREMENT */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  *n++ = ONEBYTEINT(p);p++;       /* FLAGIRREGGRIDLIST */
  *n++ = ONEBYTEINT(p);p++;       /* FLAGOTHERGRIDLIST */
/*
// Auxiliary information
*/
  *n++ = ONEBYTEINT(p);p++;       /* FLAGOTHERINFO */

  *n++ = ONEBYTEINT(p);p++;       /* NUMBERHORIZCOORDS */
  N = *(n-1);

  *n++ = TWOBYTEINT(p);           /* NUMBERMIXEDCOORDS */
  M = *(n-1);
  p += 2;

  *n++ = TWOBYTEINT(p);           /* NUMBERGRIDCOORDS */
  I = *(n-1);
  p += 2;

  *n++ = TWOBYTEINT(p);           /* NUMBERAUXILIARY */
  J = *(n-1);
  p += 2;
/*
// Horizontal coordinate definition 
// For coordinates, if sign bit is 1, value is negative
*/
  for( loop = 0; loop < N; loop++) {
    value = FOURBYTEINT(p); p += 4;
    if( value & 0x80000000 ) value = - (value & 0x7fffffff);
    *n++ = value;
  }
/*
// Mixed coordinate definition
// For coordinates, if sign bit is 1, value is negative
*/
  for( loop = 0; loop < M; loop++) {
    value = FOURBYTEINT(p); p += 4;
    if( value & 0x80000000 ) value = - (value & 0x7fffffff);
    *n++ = value;
  }
/*
// Grid coordinate list
// For coordinates, set sign bit to 1, if value is negative
*/
  for( loop = 0; loop < I; loop++) {
    value = FOURBYTEINT(p); p += 4;
    if( value & 0x80000000 ) value = - (value & 0x7fffffff);
    *n++ = value;
  }
/*
// Auxiliary array
*/
  for( loop = 0; loop < J; loop++) {
    *n++ = FOURBYTEINT(p);
    p += 4;
  }
/*
// Post-auxiliary array KSEC1 element 
*/
  if( postAuxiliaryArrayFlag ) {
    *n++ = FOURBYTEINT(p);
    p += 4;
    K = *(n-1);
    for( loop = 1; loop < K; loop++) {
      *n++ = FOURBYTEINT(p);
       p += 4;
    }
  }

  *nPointer = n;
  *pPointer = p;
  return;
}


fortint d_def_4_(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {
/*
// Decodes ECMWF local definition 4 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition4.html
//
// *bitPointer is updated by the number of bits in local definition 4.
//
// Returns the number of array elements inserted in outputArray
*/
fortint byteLength, streamNumber, postAuxiliaryArrayFlag;
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  streamNumber = *(n-2);
  if( streamNumber == 1090 ) {
    *n++ = TWOBYTEINT(p);         /* NUMBER in 2 bytes for stream 1090 */
    p +=2;
    *n++ = 0;                     /* ZERO */
  }
  else {
    *n++ = ONEBYTEINT(p);p++;     /* NUMBER */
    *n++ = 0; p++;                /* ZERO */
  }
  postAuxiliaryArrayFlag = ONEBYTEINT(p);p++; /* FLAG */

  *n++ = ONEBYTEINT(p);p++;       /* SYSTEM */
  *n++ = ONEBYTEINT(p);p++;       /* METHOD */

  decodeNonMarsPartOfDefinition4(postAuxiliaryArrayFlag, &n, &p);

  byteLength = 40 + (fortint) (p - gribSection1);

  if( *bitPointer ) *bitPointer += (byteLength-40)*8;

  return (fortint) (n - outputArray);
}

fortint d_def_4( fortint * outputArray,
                 unsigned char * gribSection1,
                 fortint * bitPointer) {
  return d_def_4_(outputArray,gribSection1,bitPointer);
}

fortint e_def_5_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 5 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition5.html
//
// Returns the number of bytes (octets) in local definition 5 after
// byte 40.
*/
fortint value;
fortint * n = inputArray;
unsigned char * p = gribSection1;
fortint indicator;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = ONEBYTE(n);n++;          /* NUMBER */
  *p++ = ONEBYTE(n);n++;          /* TOTAL */
/*
// When decoding scale, if sign bit is 1, value is negative.
*/
  value = *n++;                   /* SCALE */
  if( value < 0 ) value = 0x80 | (-value);
  *p++ = ONEBYTE(&value);

  indicator = *n;
  *p++ = ONEBYTE(n);n++;          /* INDICATOR */
/*
// When decoding upper/lower thresholds, if stored value is not 0xffff,
// if sign bit is 1, value is negative.
*/
  value = *n++;                   /* LOWER */
  if( indicator != 2 )  {
    if( value < 0 ) value = 0x8000 | (-value);
  }
  else
    value = 0xffff;
  MOVE2BYTES(p,&value);
  p += 2;

  value = *n++;                   /* UPPER */
  if( indicator != 1 )  {
    if( value < 0 ) value = 0x8000 | (-value);
  }
  else
    value = 0xffff;
  MOVE2BYTES(p,&value);
  p += 2;

  *p++ = (unsigned char) 0;       /* SPARE */
/*
// Definition 5 has a fixed length section 1 (58 bytes)
*/
  return (fortint) (58-40);
}

fortint e_def_5(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_5_(inputArray,gribSection1);
}

fortint d_def_5_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 5 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition5.html
//
// Returns the number of array elements inserted in outputArray
*/
fortint value;
fortint * n = outputArray;
unsigned char * p = gribSection1;
fortint indicator;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = ONEBYTEINT(p);p++;      /* NUMBER */
  *n++ = ONEBYTEINT(p);p++;      /* TOTAL */
/*
// When decoding scale, if sign bit is 1, value is negative.
*/
  value = ONEBYTEINT(p);p++;     /* SCALE */
  if( value & 0x80 ) value = - (value & 0x7f);
  *n++ = value;

  *n++ = ONEBYTEINT(p);p++;      /* INDICATOR */
  indicator = *(n-1);
/*
// When decoding upper/lower thresholds, if stored value is not 0xffff,
// if sign bit is 1, value is negative.
*/
  value = TWOBYTEINT(p);         /* LOWER */
  p += 2;
  if( indicator != 2 ) {
    if( value & 0x8000 ) value = - (value & 0x7fff);
  }
  else
    value = 0xffff;
  *n++ = value;

  value = TWOBYTEINT(p);         /* UPPER */
  p += 2;
  if( indicator != 1 ) {
    if( value & 0x8000 ) value = - (value & 0x7fff);
  }
  else
    value = 0xffff;
  *n++ = value;

  return (fortint) (n - outputArray);
}

fortint d_def_5(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_5_(outputArray,gribSection1);
}

fortint e_def_6_(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {
/*
// Encodes ECMWF local definition 6 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition6.html
//
// If *bitPointer is not equal to zero, the length of the GRIB section
// 1 is stored in 3 bytes starting at (gribSection1-40).
// *bitPointer is updated by the number of bits in local definition 6.
//
// Returns the number of bytes (octets) in local definition 6 after
// byte 40.
*/
int loop, N;
fortint byteLength;
fortint * n = inputArray;
unsigned char * p = gribSection1;
fortint date;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = (unsigned char) 0;       /* ZERO1 */
  n++;
  *p++ = (unsigned char) 0;       /* ZERO2 */
  n++;

  date = *n++;                    /* DATEOFSST */
  if( date > 19000000 ) date -= 19000000;
  MOVE3BYTES(p,&date);
  p += 3;

  *p++ = ONEBYTE(n);n++;          /* TYPEOFSST */

  N = *n;                         /* COUNTOFICE */
  *p++ = ONEBYTE(n++);

  for( loop = 0; loop < N; loop++) {
    date = *n++;                  /* ICEDATE */
    if( date > 19000000 ) date -= 19000000;
    MOVE3BYTES(p,&date);
    p += 3;
    *p++ = ONEBYTE(n);n++;        /* SATELLITE */
  }
 
  byteLength = 40 + (fortint) (p-gribSection1);
  if( *bitPointer ) {
    MOVE3BYTES(gribSection1-40,&byteLength);
    *bitPointer += (byteLength-40)*8;
  }

  return (fortint) byteLength;
}

fortint e_def_6( fortint * inputArray,
                 unsigned char * gribSection1,
                 fortint * bitPointer) {
  return e_def_6_(inputArray,gribSection1,bitPointer);
}

fortint d_def_6_(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {
/*
// Decodes ECMWF local definition 6 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition6.html
//
// *bitPointer is updated by the number of bits in local definition 6.
//
// Returns the number of array elements inserted in outputArray
*/
int loop, N, byteLength;
fortint * n = outputArray;
unsigned char * p = gribSection1;
fortint date;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = (fortint) 0;  p++;       /* ZERO1 */
  *n++ = (fortint) 0;  p++;       /* ZERO2 */

  date = THREEBYTEINT(p);         /* Date of SST field used */
  p += 3;
  if( (date < 19000000) && (date > 100 ) ) date += 19000000;
  *n++ = date;

  *n++ = ONEBYTEINT(p);p++;       /* TYPEOFSST */
  *n++ = ONEBYTEINT(p);p++;       /* COUNTOFICE */

  N = *(n-1);
  for( loop = 0; loop < N; loop++) {
    date = THREEBYTEINT(p);       /* ICEDATE */
    p += 3;
    if( (date < 19000000) && (date > 100 ) ) date += 19000000;
    *n++ = date;
    *n++ = ONEBYTEINT(p);p++;     /* SATELLITE */
  }

  byteLength = 40 + (fortint) (p - gribSection1);

  if( *bitPointer ) *bitPointer += (byteLength-40)*8;

  return (fortint) (n - outputArray);
}

fortint d_def_6( fortint * outputArray,
                 unsigned char * gribSection1,
                 fortint * bitPointer) {
  return d_def_6_(outputArray,gribSection1,bitPointer);
}

fortint e_def_7_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 7 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition7.html
//
// Returns the number of bytes (octets) in local definition 7 after
// byte 40.
*/
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = ONEBYTE(n);n++;          /* NUMBER */
  *p++ = ONEBYTE(n);n++;          /* TOTAL */
  *p++ = ONEBYTE(n);n++;          /* DOMAIN */
  *p++ = ONEBYTE(n);n++;          /* DIAGNOSTICNUMBER */
  *p++ = (unsigned char) 0;       /* SPARE */
/*
// Definition 7 has a fixed length section 1 (54 bytes)
*/
  return (fortint) (54-40);
}

fortint e_def_7(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_7_(inputArray,gribSection1);
}

fortint d_def_7_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 7 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition7.html
//
// Returns the number of array elements inserted in outputArray
*/
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = ONEBYTEINT(p);p++;      /* NUMBER */
  *n++ = ONEBYTEINT(p);p++;      /* TOTAL */
  *n++ = ONEBYTEINT(p);p++;      /* DOMAIN */
  *n++ = ONEBYTEINT(p);p++;      /* DIAGNOSTICNUMBER */

  return (fortint) (n - outputArray);
}

fortint d_def_7(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_7_(outputArray,gribSection1);
}

fortint e_def_8_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 8 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition8.html
//
// Returns the number of bytes (octets) in local definition 8 after
// byte 40.
*/
int loop;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = ONEBYTE(n);n++;          /* INTERVAL */

  for( loop = 0; loop < 12; loop++) {
    *p++ = ONEBYTE(n);            /* UNSIGNED INTEGER */
    n++;
  }
/*
// Definition 8 has a fixed length section 1 (62 bytes)
*/
  return (fortint) (62-40);
}

fortint e_def_8(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_8_(inputArray,gribSection1);
}

fortint d_def_8_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 8 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition8.html
//
// Returns the number of array elements inserted in outputArray
*/
int loop;
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = ONEBYTEINT(p);p++;      /* INTERVAL */

  for( loop = 0; loop < 12; loop++) {
     *n++ = ONEBYTEINT(p);       /* UNSIGNED INTEGER */
     p++;
  }

  return (fortint) (n - outputArray);
}

fortint d_def_8(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_8_(outputArray,gribSection1);
}

fortint e_def_9_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 9 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition9.html
//
// Returns the number of bytes (octets) in local definition 9 after
// byte 40.
*/
fortint value;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  MOVE2BYTES(p,n);                /* NUMBER */
  p += 2;
  n++;
  MOVE2BYTES(p,n);                /* ITERATIONCOUNT */
  p += 2;
  n++;
  MOVE2BYTES(p,n);                /* VECTORCOUNT */
  p += 2;
  n++;

  *p++ = ONEBYTE(n);n++;          /* INITIALNORM */
  *p++ = ONEBYTE(n);n++;          /* FINALNORM */

  MOVE4BYTES(p,n);                /* VECTORCOUNT */
  p += 4;
  n++;
/*
// When coding values, set sign bit to 1, if value is negative.
*/
  value = *n++;                   /* NORTHWESTLAT */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  value = *n++;                   /* NORTHWESTLON */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  value = *n++;                   /* SOUTHEASTLAT */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  value = *n++;                   /* SOUTHEASTLON */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  MOVE4BYTES(p,n);                /* ACCURACY */
  p += 4;
  n++;

  MOVE2BYTES(p,n);                /* NUMBEREVOLVED */
  p += 2;
  n++;

  value = *n++;                   /* RITZNUMBER1 */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  value = *n++;                   /* RITZNUMBER2 */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  *p++ = (unsigned char) 0;       /* SPARE */
/*
// Definition 9 has a fixed length section 1 (92 bytes)
*/

  return (fortint) (92-40);
}

fortint e_def_9(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_9_(inputArray,gribSection1);
}

fortint d_def_9_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 9 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition9.html
//
// Returns the number of array elements inserted in outputArray
*/
fortint value;
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = TWOBYTEINT(p);           /* NUMBER */
  p += 2;
  *n++ = TWOBYTEINT(p);           /* ITERATIONCOUNT */
  p += 2;
  *n++ = TWOBYTEINT(p);           /* VECTORCOUNT */
  p += 2;

  *n++ = ONEBYTEINT(p);p++;       /* INITIALNORM */
  *n++ = ONEBYTEINT(p);p++;       /* FINALNORM */

  *n++ = FOURBYTEINT(p);          /* FACTOR */
  p += 4;

/*
// When decoding values, if sign bit is 1, value is negative.
*/
  value = FOURBYTEINT(p);         /* NORTHWESTLAT */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  value = FOURBYTEINT(p);         /* NORTHWESTLON */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  value = FOURBYTEINT(p);         /* SOUTHEASTLAT */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  value = FOURBYTEINT(p);         /* SOUTHEASTLON */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  *n++ = FOURBYTEINT(p);          /* ACCURACY */
  p += 4;

  *n++ = TWOBYTEINT(p);           /* NUMBEREVOLVED */
  p += 2;

  value = FOURBYTEINT(p);         /* RITZNUMBER1 */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  value = FOURBYTEINT(p);         /* RITZNUMBER2 */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  return (fortint) (n - outputArray);
}

fortint d_def_9(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_9_(outputArray,gribSection1);
}

fortint e_def_10_(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {
/*
// Encodes ECMWF local definition 10 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition10.html
//
// If *bitPointer is not equal to zero, the length of the GRIB section
// 1 is stored in 3 bytes starting at (gribSection1-40).
// *bitPointer is updated by the number of bits in local definition 10.
//
// Returns the number of bytes (octets) in local definition 10 after
// byte 40.
*/
int loop, N;
fortint value, byteLength;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = ONEBYTE(n);n++;          /* NUMBER */
  *p++ = ONEBYTE(n);n++;          /* TOTAL */
  *p++ = ONEBYTE(n);n++;          /* CENTRALDEFINITION */
  *p++ = ONEBYTE(n);n++;          /* PARAMETER */
  *p++ = ONEBYTE(n);n++;          /* LEVELTYPE */

/*
// When coding values, set sign bit to 1, if value is negative.
*/
  value = *n++;                   /* NORTH */
  if( value < 0 ) value = 0x800000 | (-value);
  MOVE3BYTES(p,&value);
  p += 3;

  value = *n++;                   /* WEST */
  if( value < 0 ) value = 0x800000 | (-value);
  MOVE3BYTES(p,&value);
  p += 3;

  value = *n++;                   /* SOUTH */
  if( value < 0 ) value = 0x800000 | (-value);
  MOVE3BYTES(p,&value);
  p += 3;

  value = *n++;                   /* EAST */
  if( value < 0 ) value = 0x800000 | (-value);
  MOVE3BYTES(p,&value);
  p += 3;

  *p++ = ONEBYTE(n);n++;          /* OPERFORECASTTUBE */
  *p++ = ONEBYTE(n);n++;          /* CONTROLFORECASTTUBE */

  MOVE2BYTES(p,n);                /* HEIGHT */
  p += 2;
  n++;
  MOVE2BYTES(p,n);                /* STEP */
  p += 2;
  n++;
  MOVE2BYTES(p,n);                /* RADIUS */
  p += 2;
  n++;
  MOVE2BYTES(p,n);                /* ENSEMBLEDEVIATION */
  p += 2;
  n++;
  MOVE2BYTES(p,n);                /* DISTANCEEXTREME */
  p += 2;
  n++;

  N = *n;                         /* NUMBERINTUBE */
  *p++ = ONEBYTE(n++);

  for( loop = 0; loop < N; loop++)  /* ENSEMBLE FORECAST NUMBERS */
    *p++ = ONEBYTE(n++);
/*
// Definition 10 has a fixed length section 1 (334 bytes)
*/
  for( loop = N; loop < (334-39); loop++)
    *p++ = (unsigned char) 0;

  byteLength = 334;
  if( *bitPointer ) {
    MOVE3BYTES(gribSection1-40,&byteLength);
    *bitPointer += (byteLength-40)*8;
  }

  return (fortint) byteLength;
}

fortint e_def_10( fortint * inputArray,
                  unsigned char * gribSection1,
                  fortint * bitPointer) {
  return e_def_10_(inputArray,gribSection1,bitPointer);
}

fortint d_def_10_(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {
/*
// Decodes ECMWF local definition 10 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition10.html
//
// *bitPointer is updated by the number of bits in local definition 10.
//
// Returns the number of array elements inserted in outputArray
*/
int loop, N, byteLength;
fortint value;
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = ONEBYTEINT(p);p++;       /* NUMBER */
  *n++ = ONEBYTEINT(p);p++;       /* TOTAL */
  *n++ = ONEBYTEINT(p);p++;       /* CENTRALDEFINITION */
  *n++ = ONEBYTEINT(p);p++;       /* PARAMETER */
  *n++ = ONEBYTEINT(p);p++;       /* LEVELTYPE */
/*
// When decoding values, if sign bit is 1, value is negative.
*/
  value = THREEBYTEINT(p);        /* NORTH */
  p += 3;
  if( value & 0x800000 ) value = - (value & 0x7fffff);
  *n++ = value;

  value = THREEBYTEINT(p);        /* WEST */
  p += 3;
  if( value & 0x800000 ) value = - (value & 0x7fffff);
  *n++ = value;

  value = THREEBYTEINT(p);        /* SOUTH */
  p += 3;
  if( value & 0x800000 ) value = - (value & 0x7fffff);
  *n++ = value;

  value = THREEBYTEINT(p);        /* EAST */
  p += 3;
  if( value & 0x800000 ) value = - (value & 0x7fffff);
  *n++ = value;

  *n++ = ONEBYTEINT(p);p++;       /* OPERFORECASTTUBE */
  *n++ = ONEBYTEINT(p);p++;       /* CONTROLFORECASTTUBE */

  *n++ = TWOBYTEINT(p);           /* HEIGHT */
  p += 2;
  *n++ = TWOBYTEINT(p);           /* STEP */
  p += 2;
  *n++ = TWOBYTEINT(p);           /* RADIUS */
  p += 2;
  *n++ = TWOBYTEINT(p);           /* ENSEMBLEDEVIATION */
  p += 2;
  *n++ = TWOBYTEINT(p);           /* DISTANCEEXTREME */
  p += 2;

  *n++ = ONEBYTEINT(p);p++;       /* NUMBERINTUBE */
  N = *(n-1);

  for( loop = 0; loop < N; loop++)    /* ENSEMBLE FORECAST NUMBERS */
    *n++ = ONEBYTEINT(p++);
/*
// Definition 10 has a fixed length section 1 (334 bytes)
*/
  byteLength = 334;

  if( *bitPointer ) *bitPointer += (byteLength-40)*8;

  return (fortint) (n - outputArray);
}

fortint d_def_10( fortint * outputArray,
                  unsigned char * gribSection1,
                  fortint * bitPointer) {
  return d_def_10_(outputArray,gribSection1,bitPointer);
}

fortint e_def_11_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 11 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition11.html
//
// Returns the number of bytes (octets) in local definition 11 after
// byte 40.
*/
int loop;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = ONEBYTE(n);n++;          /* ANALYSISCLASS */
  *p++ = ONEBYTE(n);n++;          /* ANALYSISTYPE */

  MOVE2BYTES(p,n);                /* ANALYSISSTREAM */
  p += 2;
  n++;

  MOVE4BYTES(p,n);                /* ANALYSISEXPVER */
  p += 4;
  n++;

  *p++ = ONEBYTE(n);n++;          /* ANALYSIS_YY */
  *p++ = ONEBYTE(n);n++;          /* ANALYSIS_MM */
  *p++ = ONEBYTE(n);n++;          /* ANALYSIS_DD */
  *p++ = ONEBYTE(n);n++;          /* ANALYSIS_HH */
  *p++ = ONEBYTE(n);n++;          /* ANALYSIS_MIN */
  *p++ = ONEBYTE(n);n++;          /* ANALYSISCENTURY */
  *p++ = ONEBYTE(n);n++;          /* ANALYSISORIGIN */
  *p++ = ONEBYTE(n);n++;          /* ANALYSISSUBCENTRE */


  for( loop = 0; loop < 7; loop++)   /* SPARE */
    *p++ = (unsigned char) 0;
/*
// Definition 11 has a fixed length section 1 (72 bytes)
*/
  return (fortint) (72-40);
}

fortint e_def_11(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_11_(inputArray,gribSection1);
}

fortint d_def_11_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 11 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition11.html
//
// Returns the number of array elements inserted in outputArray
*/
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = ONEBYTEINT(p);p++;      /* ANALYSISCLASS */
  *n++ = ONEBYTEINT(p);p++;      /* ANALYSISTYPE */

  *n++ = TWOBYTEINT(p);          /* ANALYSISSTREAM */
  p += 2;
  *n++ = FOURBYTEINT(p);         /* ANALYSISEXPVER */
  p += 4;

  *n++ = ONEBYTEINT(p);p++;      /* ANALYSIS_YY */
  *n++ = ONEBYTEINT(p);p++;      /* ANALYSIS_MM */
  *n++ = ONEBYTEINT(p);p++;      /* ANALYSIS_DD */
  *n++ = ONEBYTEINT(p);p++;      /* ANALYSIS_HH */
  *n++ = ONEBYTEINT(p);p++;      /* ANALYSIS_MIN */
  *n++ = ONEBYTEINT(p);p++;      /* ANALYSISCENTURY */
  *n++ = ONEBYTEINT(p);p++;      /* ANALYSISORIGIN */
  *n++ = ONEBYTEINT(p);p++;      /* ANALYSISSUBCENTRE */

  return (fortint) (n - outputArray);
}

fortint d_def_11(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_11_(outputArray,gribSection1);
}

fortint e_def_13_(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {
/*
// Encodes ECMWF local definition 13 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition13.html
//
// If *bitPointer is not equal to zero, the length of the GRIB section
// 1 is stored in 3 bytes starting at (gribSection1-40).
// *bitPointer is updated by the number of bits in local definition 13.
//
// Returns the number of bytes (octets) in local definition 13 after
// byte 40.
*/
int loop, ND, NF;
fortint byteLength;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = ONEBYTE(n);n++;          /* NUMBER */
  *p++ = ONEBYTE(n);n++;          /* TOTAL */
  *p++ = ONEBYTE(n);n++;          /* DIRECTION */
  *p++ = ONEBYTE(n);n++;          /* FREQUENCY */

  ND = *n;                        /* NUMBERDIRECTIONS */
  *p++ = ONEBYTE(n++);

  NF = *n;                        /* NUMBERFREQUENCIES */
  *p++ = ONEBYTE(n++);

  MOVE4BYTES(p,n);                /* SCALEDIRECTIONS */
  p += 4;
  n++;
  MOVE4BYTES(p,n);                /* SCALEFREQUENCIES */
  p += 4;
  n++;
/*
// Set flag for new style coding and include system and method
// Note the offsets for system/method; n is not updated till later!
// Modified flag=2 to add:
//   reference date
//   climate date from
//   climate date to.
*/
  *p++ = (unsigned char) 2;       /* FLAG */

  MOVE2BYTES(p,(n+ND+NF));        /* SYSTEM */
  p += 2;

  MOVE2BYTES(p,(n+ND+NF+1));      /* METHOD */
  p += 2;

  MOVE4BYTES(p,(n+ND+NF+2));      /* REFERENCE DATE */
  p += 4;

  MOVE4BYTES(p,(n+ND+NF+3));      /* CLIMATE DATE FROM */
  p += 4;

  MOVE4BYTES(p,(n+ND+NF+4));      /* CLIMATE DATE TO */
  p += 4;

  for( loop = 0; loop < 20; loop++)     /* SPARE */
    *p++ = (unsigned char) 0;

  for( loop = 0; loop < ND; loop++) {   /* DIRECTIONS */
    MOVE4BYTES(p,n);
    p += 4;
    n++;
  }
 
  for( loop = 0; loop < NF; loop++) {   /* FREQUENCIES */
    MOVE4BYTES(p,n);
    p += 4;
    n++;
  }

  n += 5;                        /* for system, method, reference date and
                                    two climate dates */
 
  byteLength = 100 + ND*4 + NF*4;
  if( *bitPointer ) {
    MOVE3BYTES(gribSection1-40,&byteLength);
    *bitPointer += (byteLength-40)*8;
  }

  return (fortint) byteLength;
}

fortint e_def_13( fortint * inputArray,
                  unsigned char * gribSection1,
                  fortint * bitPointer) {
  return e_def_13_(inputArray,gribSection1,bitPointer);
}

fortint d_def_13_(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {
/*
// Decodes ECMWF local definition 13 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition13.html
//
// *bitPointer is updated by the number of bits in local definition 13.
//
// Returns the number of array elements inserted in outputArray
*/
int loop, ND, NF, byteLength, flag;
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = ONEBYTEINT(p);p++;       /* NUMBER */
  *n++ = ONEBYTEINT(p);p++;       /* TOTAL */
  *n++ = ONEBYTEINT(p);p++;       /* DIRECTION */
  *n++ = ONEBYTEINT(p);p++;       /* FREQUENCY */

  *n++ = ONEBYTEINT(p);p++;       /* NUMBERDIRECTIONS */
  ND = *(n-1);
  *n++ = ONEBYTEINT(p);p++;       /* NUMBERFREQUENCIES */
  NF = *(n-1);

  *n++ = FOURBYTEINT(p);          /* SCALEDIRECTIONS */
  p += 4;
  *n++ = FOURBYTEINT(p);          /* SCALEFREQUENCIES */
  p += 4;
/*
// Handle system and method if flag = 1 or 2
// Modified flag=2 added:
//   reference date
//   climate date from
//   climate date to.
*/
  flag = ONEBYTEINT(p++);          /* FLAG */
  if( flag == 1 ) {
    *(n+ND+NF) = TWOBYTEINT(p);    /* SYSTEM */
    p += 2;
    *(n+ND+NF+1) = TWOBYTEINT(p);  /* METHOD */

    p += 34;                       /* SPARE */
  }
  else if( flag == 2 ) {
    *(n+ND+NF) = TWOBYTEINT(p);    /* SYSTEM */
    p += 2;
    *(n+ND+NF+1) = TWOBYTEINT(p);  /* METHOD */
    p += 2;
    *(n+ND+NF+2) = FOURBYTEINT(p); /* REFERENCE DATE */
    p += 4;
    *(n+ND+NF+3) = FOURBYTEINT(p); /* CLIMATE DATE FROM */
    p += 4;
    *(n+ND+NF+4) = FOURBYTEINT(p); /* CLIMATE DATE TO */

    p += 24;                       /* SPARE */
  }
  else
    p += 36;                       /* SPARE */

  for( loop = 0; loop < ND; loop++) { /* DIRECTIONS */
    *n++ = FOURBYTEINT(p);
    p += 4;
  }

  for( loop = 0; loop < NF; loop++) { /* FREQUENCIES */
    *n++ = FOURBYTEINT(p);
    p += 4;
  }
/*
// Adjust for system and method if 'new-style' flag is set
*/
  if( flag == 1 ) n += 2;
  if( flag == 2 ) n += 5;

  byteLength = 100 + ND*4 + NF*4;

  if( *bitPointer ) *bitPointer += (byteLength-40)*8;

  return (fortint) (n - outputArray);
}

fortint d_def_13( fortint * outputArray,
                  unsigned char * gribSection1,
                  fortint * bitPointer) {
  return d_def_13_(outputArray,gribSection1,bitPointer);
}

fortint e_def_14_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 14 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition14.html
//
// Returns the number of bytes (octets) in local definition 14 after
// byte 40.
*/
int loop, N;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = ONEBYTE(n);n++;          /* NUMBER */
  *p++ = ONEBYTE(n);n++;          /* TOTAL */
  *p++ = ONEBYTE(n);n++;          /* CHANNELNUMBER */

  MOVE4BYTES(p,n);                /* SCALEFACTOR */
  p += 4;
  n++;

  *p++ = ONEBYTE(n);n++;          /* NUMBERFREQUENCIES */
  N = *(n-1);

  for( loop = 0; loop < 3; loop++)  /* SPARE */
    *p++ = (unsigned char) 0;

  for( loop = 0; loop < N; loop++) {   /* FREQUENCIES */
    MOVE4BYTES(p,n);
    p += 4;
    n++;
  }
/*
// Definition 14 has a fixed length section 1 (1080 bytes)
*/
  for( loop = 0; loop < (1080-60-N*4); loop++)
    *p++ = (unsigned char) 0;

  return (fortint) (1080-40);
}

fortint e_def_14(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_14_(inputArray,gribSection1);
}

fortint d_def_14_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 14 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition14.html
//
// Returns the number of array elements inserted in outputArray
*/
int loop, N;
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = ONEBYTEINT(p);p++;      /* NUMBER */
  *n++ = ONEBYTEINT(p);p++;      /* TOTAL */
  *n++ = ONEBYTEINT(p);p++;      /* CHANNELNUMBER */

  *n++ = FOURBYTEINT(p);         /* SCALEFACTOR */
  p += 4;

  *n++ = ONEBYTEINT(p);p++;      /* NUMBERFREQUENCIES */
  N = *(n-1);

  p += 3;                        /* SPARE */

  for( loop = 0; loop < N; loop++) {   /* FREQUENCIES */
    *n++ = FOURBYTEINT(p);
    p += 4;
  }

  return (fortint) (n - outputArray);
}

fortint d_def_14(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_14_(outputArray,gribSection1);
}

fortint e_def_15_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 15 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition15.html
//
// Returns the number of bytes (octets) in local definition 15 after
// byte 40.
*/
int loop;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  MOVE2BYTES(p,n);                /* NUMBER */
  p += 2;
  n++;

  MOVE2BYTES((p+4),n);            /* TOTAL: bytes are out-of-sequence */
  n++;

  MOVE2BYTES(p,n);                /* SYSTEM */
  p += 2;
  n++;
  MOVE2BYTES(p,n);                /* METHOD */
  p += 4;                         /* to skip TOTAL as well as METHOD */
  n++;
/*
// Definition 15 has a fixed length section 1 (60 bytes)
*/
  for( loop = 0; loop < 3; loop++)   /* SPARE */
    *p++ = (unsigned char) 0;

  return (fortint) (60-40);
}

fortint e_def_15(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_15_(inputArray,gribSection1);
}

fortint d_def_15_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 15 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition15.html
//
// Returns the number of array elements inserted in outputArray
*/
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = TWOBYTEINT(p);          /* NUMBER */
  p += 2;

  *n++ = TWOBYTEINT((p+4));      /* TOTAL: bytes are out-of-sequence */

  *n++ = TWOBYTEINT(p);          /* SYSTEM */
  p += 2;
  *n++ = TWOBYTEINT(p);          /* METHOD */
  p += 2;

  return (fortint) (n - outputArray);
}

fortint d_def_15(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_15_(outputArray,gribSection1);
}

fortint e_def_16_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 16 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition16.html
//
// Returns the number of bytes (octets) in local definition 16 after
// byte 40.
*/
int loop;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  MOVE2BYTES(p,n);                /* NUMBER */
  p += 2;
  n++;

  n++;                            /* ZERO */

  MOVE2BYTES(p,n);                /* SYSTEM */
  p += 2;
  n++;
  MOVE2BYTES(p,n);                /* METHOD */
  p += 2;
  n++;
  MOVE4BYTES(p,n);                /* MONTH */
  p += 4;
  n++;

  *p++ = ONEBYTE(n);n++;          /* PERIOD */

  MOVE2BYTES(p,n);                /* Forecast month */
  p += 2;
  n++;

  for( loop = 63; loop <= 80; loop++ )
    *p++ = (unsigned char) 0;

  return (fortint) (80);
}

fortint e_def_16(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_16_(inputArray,gribSection1);
}

fortint d_def_16_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 16 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition16.html
//
// Returns the number of array elements inserted in outputArray
*/
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = TWOBYTEINT(p);          /* NUMBER */
  p += 2;

  *n++ = 0;                      /* ZERO */

  *n++ = TWOBYTEINT(p);          /* SYSTEM */
  p += 2;
  *n++ = TWOBYTEINT(p);          /* METHOD */
  p += 2;
  *n++ = FOURBYTEINT(p);         /* MONTH */
  p += 4;
  *n++ = ONEBYTEINT(p);          /* PERIOD */
  p++;
  *n++ = TWOBYTEINT(p);          /* Forecast month */
  p += 2;

  return (fortint) (n - outputArray);
}

fortint d_def_16(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_16_(outputArray,gribSection1);
}

fortint e_def_17_(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {
/*
// Encodes ECMWF local definition 17 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition17.html
//
// If *bitPointer is not equal to zero, the length of the GRIB section
// 1 is stored in 3 bytes starting at (gribSection1-40).
// *bitPointer is updated by the number of bits in local definition 17.
//
// Returns the number of bytes (octets) in local definition 17 after
// byte 40.
*/
int loop, N, M;
fortint byteLength;
fortint zero = 0;
fortint date;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = (unsigned char) 0;       /* ZERO1 */
  n++;
  *p++ = (unsigned char) 0;       /* ZERO2 */
  n++;

  date = *n++;                    /* DATEOFSST */
  if( date > 19000000 ) date -= 19000000;
  MOVE3BYTES(p,&date);
  p += 3;

  *p++ = ONEBYTE(n);n++;          /* TYPEOFSST */
  *p++ = ONEBYTE(n);n++;          /* COUNTOFICE */
  N = *(n-1);

  for( loop = 0; loop < N; loop++) {   /* ICE data */
    date = *n++;                  /* Date of ICE field */
    if( date > 19000000 ) date -= 19000000;
    MOVE3BYTES(p,&date);
    p += 3;
    *p++ = ONEBYTE(n);n++;             /* Satellite number */
  }
/*
// The ICE data entries are padded with zeroes to make upto a multiple
// of 10 slots
*/
  M = ((N+9)/10)*10;
  if( M == 0 ) M = 10;
  for( loop = N; loop < M; loop++) {
    MOVE4BYTES(p,&zero);
    p += 4;
  }
 
  byteLength = 40 + 16 + M*4;
  if( *bitPointer ) {
    MOVE3BYTES(gribSection1-40,&byteLength);
    *bitPointer += (byteLength-40)*8;
  }

  return (fortint) byteLength;
}

fortint e_def_17( fortint * inputArray,
                  unsigned char * gribSection1,
                  fortint * bitPointer) {
  return e_def_17_(inputArray,gribSection1,bitPointer);
}

fortint d_def_17_(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {
/*
// Decodes ECMWF local definition 17 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition17.html
//
// *bitPointer is updated by the number of bits in local definition 17.
//
// Returns the number of array elements inserted in outputArray
*/
int loop, N, M, byteLength;
fortint date;
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = (fortint) 0;  p++;       /* ZERO1 */
  *n++ = (fortint) 0;  p++;       /* ZERO2 */

  date = THREEBYTEINT(p);         /* DATEOFSST */
  p += 3;
  if( (date < 19000000) && (date > 100 ) ) date += 19000000;
  *n++ = date;

  *n++ = ONEBYTEINT(p);p++;       /* TYPEOFSST */
  *n++ = ONEBYTEINT(p);p++;       /* COUNTOFICE */
  N = *(n-1);

  for( loop = 0; loop < N; loop++) {    /* ICE data */
    date = THREEBYTEINT(p);             /* Date of ICE field */
    p += 3;
    if( (date < 19000000) && (date > 100 ) ) date += 19000000;
    *n++ = date;

    *n++ = ONEBYTEINT(p);p++;           /* Satellite number */
  }

  M = ((N+9)/10)*10;
  if( M == 0 ) M = 10;
  byteLength = 40 + 16 + M*4;

  if( *bitPointer ) *bitPointer += (byteLength-40)*8;

  return (fortint) (n - outputArray);
}

fortint d_def_17( fortint * outputArray,
                  unsigned char * gribSection1,
                  fortint * bitPointer) {
  return d_def_17_(outputArray,gribSection1,bitPointer);
}

fortint e_def_18_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 18 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition18.html
//
// Returns the number of bytes (octets) in local definition 18 after
// byte 40.
*/
int loop, N;
fortint zero = 0, fourBlanks = 0x20202020;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = ONEBYTE(n);n++;          /* NUMBER */
  *p++ = ONEBYTE(n);n++;          /* TOTAL */
  *p++ = ONEBYTE(n);n++;          /* ORIGIN */

  MOVE4BYTES(p,n);                /* MODEL */
  p += 4;
  n++;

  *p++ = ONEBYTE(n);n++;          /* CONSENSUS_COUNT */
  N = *(n-1);

  MOVE3BYTES(p,&zero);            /* SPARE */
  p += 3;

  for( loop = 0; loop < N; loop++) {   /* WMO identifiers */
    MOVE4BYTES(p,n);
    p += 4;
    n++;
  }
/*
// Definition 2 has a fixed length section 1 (120 bytes)
*/
  for( loop = 0; loop < (15-N); loop++) {   /* PADDING */
    MOVE4BYTES(p,&fourBlanks);
    p += 4;
  }

  return (fortint) (120 - 40);
}

fortint e_def_18(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_18_(inputArray,gribSection1);
}

fortint d_def_18_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 18 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition18.html
//
// Returns the number of array elements inserted in outputArray
*/
int loop, N;
unsigned char fourBlanks[5] = "    ";
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = ONEBYTEINT(p);p++;       /* NUMBER */
  *n++ = ONEBYTEINT(p);p++;       /* TOTAL */
  *n++ = ONEBYTEINT(p);p++;       /* ORIGIN */

  *n++ = FOURBYTEINT(p);          /* MODEL */
  p += 4;

  *n++ = ONEBYTEINT(p);p++;       /* CONSENSUS_COUNT */
  N = *(n-1);

  p += 3;                         /* SPARE */

  for( loop = 0; loop < N; loop++) {    /* WMO identifiers */
    *n++ = FOURBYTEINT(p);
    p += 4;
  }
/*
// Definition 2 has a fixed length section 1 (120 bytes)
*/
  for( loop = 0; loop < (15-N); loop++)
    *n++ = FOURBYTEINT(fourBlanks);       /* PADDING */

  return (fortint) (n - outputArray);
}

fortint d_def_18(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_18_(outputArray,gribSection1);
}

fortint e_def_19_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 19 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition19.html
//
// Returns the number of bytes (octets) in local definition 19 after
// byte 40.
*/
int loop;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = (unsigned char) 0;n++;   /* ZERO */
  *p++ = ONEBYTE(n);n++;          /* ENSEMBLESIZE */
  *p++ = ONEBYTE(n);n++;          /* POWEROFTEN */

  MOVE4BYTES(p,n);                /* WEIGHT */
  p += 4;
  n++;
  MOVE3BYTES(p,n);                /* FIRSTMONTH1 */
  p += 3;
  n++;
  MOVE3BYTES(p,n);                /* LASTMONTH1 */
  p += 3;
  n++;
  MOVE3BYTES(p,n);                /* FIRSTMONTH2 */
  p += 3;
  n++;
  MOVE3BYTES(p,n);                /* LASTMONTH2 */
  p += 3;
  n++;
  *p++ = ONEBYTE(n);n++;          /* EFI order */
/*
// Definition 19 has a fixed length section 1 (80 bytes)
*/
   for( loop = 0; loop < 11; loop++)
     *p++ = (unsigned char) 0;         /* PADDING */

  return (fortint) (80-40);
}

fortint e_def_19(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_19_(inputArray,gribSection1);
}

fortint d_def_19_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 19 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition19.html
//
// Returns the number of array elements inserted in outputArray
*/
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = (fortint) 0;  p++;       /* ZERO */
  *n++ = ONEBYTEINT(p);p++;       /* ENSEMBLESIZE */
  *n++ = ONEBYTEINT(p);p++;       /* POWEROFTEN */

  *n++ = FOURBYTEINT(p);          /* WEIGHT */
  p += 4;
  *n++ = THREEBYTEINT(p);         /* FIRSTMONTH1 */
  p += 3;
  *n++ = THREEBYTEINT(p);         /* LASTMONTH1 */
  p += 3;
  *n++ = THREEBYTEINT(p);         /* FIRSTMONTH2 */
  p += 3;
  *n++ = THREEBYTEINT(p);         /* LASTMONTH2 */
  p += 3;
  *n++ = ONEBYTEINT(p);p++;       /* EFI order */

  return (fortint) (n - outputArray);
}

fortint d_def_19(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_19_(outputArray,gribSection1);
}

fortint e_def_20_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 20 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition20.html
//
// Returns the number of bytes (octets) in local definition 20 after
// byte 40.
*/
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = ONEBYTE(n);n++;          /* ITERATION */
  *p++ = ONEBYTE(n);n++;          /* TOTAL NUMBER OF ITERATIONS */
  *p++ = (unsigned char) 0;       /* UNUSED */

/*
// Definition 20 has a fixed length section 1 (52 bytes)
*/
  return (fortint) (52-40);
}

fortint e_def_20(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_20_(inputArray,gribSection1);
}

fortint d_def_20_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 20 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition20.html
//
// Returns the number of array elements inserted in outputArray
*/
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = ONEBYTEINT(p);p++;      /* ITERATION */
  *n++ = ONEBYTEINT(p);p++;      /* TOTAL NUMBER OF ITERATIONS */

  return (fortint) (n - outputArray);
}

fortint d_def_20(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_20_(outputArray,gribSection1);
}

fortint e_def_21_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 21 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition21.
html
//
// Returns the number of bytes (octets) in local definition 21 after
// byte 40.
*/
fortint value;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  MOVE2BYTES(p,n);                /* Forecast or singular vector number */
  p += 2;
  n++;
  MOVE2BYTES(p,n);                /* Number of iterations */
  p += 2;
  n++;
  MOVE2BYTES(p,n);                /* Number of singular vectors computed */
  p += 2;
  n++;
  *p++ = ONEBYTE(n);n++;          /* Norm used at initial time */
  *p++ = ONEBYTE(n);n++;          /* Norm used at final time */

  MOVE4BYTES(p,n);                /* Lat-long multiplication factor */
  p += 4;
  n++;
/*
// When coding values, set sign bit to 1, if value is negative.
*/
  value = *n++;                   /* North-west latitude */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  value = *n++;                   /* North-west longitude */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  value = *n++;                   /* South-east latitude */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  value = *n++;                   /* South-east longitude */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  MOVE4BYTES(p,n);                /* Accuracy */
  p += 4;
  n++;

  MOVE2BYTES(p,n);                /* Number of singular vectors evolved */
  p += 2;
  n++;

  value = *n++;                   /* Ritz number 1 */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  value = *n++;                   /* Ritz number 2 */
  if( value < 0 ) value = 0x80000000 | (-value);
  MOVE4BYTES(p,&value);
  p += 4;

  *p++ = ONEBYTE(n);n++;          /* Observation offset period */
  *p++ = ONEBYTE(n);n++;          /* Forecast lead time */
  *p++ = ONEBYTE(n);n++;          /* WMO id of data originating centre */

  MOVE2BYTES(p,n);                /* Method number */
  p += 2;
  n++;
  MOVE2BYTES(p,n);                /* Total number of forecasts in ensemble */
  p += 2;
  n++;

  *p++ = ONEBYTE(n);n++;          /* Shape of verification region */
  *p++ = (unsigned char) 0;       /* Reserved. Set to zero */
  n++;


/*
// Definition 21 has a fixed length section 1 (100 bytes)
*/
  return (fortint) (100-40);
} 

fortint e_def_21(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_21_(inputArray,gribSection1);
}

fortint d_def_21_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 21 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition21.
html
//
// Returns the number of array elements inserted in outputArray
*/
fortint value;
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = TWOBYTEINT(p);          /* Forecast or singular vector number */
  p += 2;
  *n++ = TWOBYTEINT(p);          /* Number of iterations */
  p += 2;
  *n++ = TWOBYTEINT(p);          /* Number of singular vectors computed */
  p += 2;
  *n++ = ONEBYTEINT(p);p++;      /* Norm used at initial time */
  *n++ = ONEBYTEINT(p);p++;      /* Norm used at final time */

  *n++ = FOURBYTEINT(p);         /* Lat-long multiplication factor */
  p += 4;

  value = FOURBYTEINT(p);        /* North-west latitude */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  value = FOURBYTEINT(p);        /* North-west longitude */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  value = FOURBYTEINT(p);        /* South-east latitude */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  value = FOURBYTEINT(p);        /* South-east longitude */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  *n++ = FOURBYTEINT(p);         /* Accuracy */
  p += 4;
  *n++ = TWOBYTEINT(p);          /* Number of singular vectors evolved */
  p += 2;

  value = FOURBYTEINT(p);        /* Ritz number 1 */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  value = FOURBYTEINT(p);        /* Ritz number 2 */
  p += 4;
  if( value & 0x80000000 ) value = - (value & 0x7fffffff);
  *n++ = value;

  *n++ = ONEBYTEINT(p);p++;      /* Observation offset period */
  *n++ = ONEBYTEINT(p);p++;      /* Forecast lead time */
  *n++ = ONEBYTEINT(p);p++;      /* WMO id of data originating centre */

  *n++ = TWOBYTEINT(p);          /* Method number */
  p += 2;
  *n++ = TWOBYTEINT(p);          /* Total number of forecasts in ensemble */
  p += 2;

  *n++ = ONEBYTEINT(p);p++;      /* Shape of verification region */

  p++;                           /* Reserved. Set to zero */

  return (fortint) (n - outputArray);
}

fortint d_def_21(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_21_(outputArray,gribSection1);
}

fortint e_def_22_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 22 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition22.html
//
// Returns the number of bytes (octets) in local definition 22 after
// byte 40.
*/
int loop;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  MOVE2BYTES(p,n);                /* NUMBER */
  p += 2;
  n++;

  MOVE2BYTES(p,n);                /* TOTAL */
  p += 2;
  n++;

  MOVE2BYTES(p,n);                /* SYSTEM */
  p += 2;
  n++;

  MOVE2BYTES(p,n);                /* METHOD */
  p += 2;
  n++;

  MOVE4BYTES(p,n);                /* REFERENCE DATE */
  p += 4;
  n++;

  MOVE4BYTES(p,n);                /* CLIMATE DATE (FROM) */
  p += 4;
  n++;

  MOVE4BYTES(p,n);                /* CLIMATE DATE (TO) */
  p += 4;
  n++;

  return (fortint) (69);
}

fortint e_def_22(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_22_(inputArray,gribSection1);
}

fortint d_def_22_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 22 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition22.html
//
// Returns the number of array elements inserted in outputArray
*/
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = TWOBYTEINT(p);          /* NUMBER */
  p += 2;
  *n++ = TWOBYTEINT(p);          /* TOTAL */
  p += 2;
  *n++ = TWOBYTEINT(p);          /* SYSTEM */
  p += 2;
  *n++ = TWOBYTEINT(p);          /* METHOD */
  p += 2;
  *n++ = FOURBYTEINT(p);         /* REFERENCE DATE */
  p += 4;
  *n++ = FOURBYTEINT(p);         /* CLIMATE DATE (FROM) */
  p += 4;
  *n++ = FOURBYTEINT(p);         /* CLIMATE DATE (TO) */
  p += 4;

  return (fortint) (n - outputArray);
}

fortint d_def_22(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_22_(outputArray,gribSection1);
}

fortint e_def_23_(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {
/*
// Encodes ECMWF local definition 23 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition23.html
//
// If *bitPointer is not equal to zero, the length of the GRIB section
// 1 is stored in 3 bytes starting at (gribSection1-40).
// *bitPointer is updated by the number of bits in local definition 4.
//
// Returns the number of bytes (octets) in local definition 23 after
// byte 40.
*/
fortint byteLength;
int loop;
fortint * n = inputArray;
unsigned char * p = gribSection1;
fortint zero = 0;
unsigned char* total;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  MOVE2BYTES(p,n);                /* NUMBER */
  p += 2;
  n++;

  total = p + 29;
  MOVE2BYTES(total,n);            /* TOTAL */
  n++;

  MOVE2BYTES(p,n);                /* SYSTEM */
  p += 2;
  n++;
  MOVE2BYTES(p,n);                /* METHOD */
  p += 2;
  n++;
  MOVE4BYTES(p,n);                /* MONTH */
  p += 4;
  n++;

  *p++ = ONEBYTE(n);n++;          /* PERIOD */

  MOVE2BYTES(p,n);                /* FORECAST MONTH */
  p += 2;
  n++;

  MOVE4BYTES(p,n);                /* REFERENCE DATE */
  p += 4;
  n++;

  MOVE4BYTES(p,n);                /* CLIMATE DATE (FROM) */
  p += 4;
  n++;

  MOVE4BYTES(p,n);                /* CLIMATE DATE (TO) */
  p += 4;
  n++;

  *p++ = ONEBYTE(n);n++;          /* THRESHOLD UNITS */

  *p++ = ONEBYTE(n);n++;          /* THRESHOLD INDICATOR */

  MOVE2BYTES(p,n);                /* LOWER THRESHOLD VALUE */
  p += 2;
  n++;
  MOVE2BYTES(p,n);                /* UPPER THRESHOLD VALUE */
  p += 2;
  n++;

/*
// Skip the total (already packed above)
*/
  p +=2;

  MOVE2BYTES(p,&zero);             /* SPARE (SET TO ZERO) */
  p += 2;
  n += 4;

  byteLength = 40 + (fortint) (p-gribSection1);
  if( *bitPointer ) {
    MOVE3BYTES(gribSection1-40,&byteLength);
    *bitPointer += (byteLength-40)*8;
  }

  return (fortint) byteLength;
}

fortint e_def_23(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {

  return e_def_23_(inputArray,gribSection1,bitPointer);
}

fortint d_def_23_(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {
/*
// Decodes ECMWF local definition 23 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition23.html
//
// *bitPointer is updated by the number of bits in local definition 4.
//
// Returns the number of array elements inserted in outputArray
*/
fortint byteLength;
fortint * n = outputArray;
unsigned char * p = gribSection1;
unsigned char* total;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = TWOBYTEINT(p);          /* NUMBER */
  p += 2;

  total = p + 29;
  *n++ = TWOBYTEINT(total);      /* TOTAL */

  *n++ = TWOBYTEINT(p);          /* SYSTEM */
  p += 2;
  *n++ = TWOBYTEINT(p);          /* METHOD */
  p += 2;
  *n++ = FOURBYTEINT(p);         /* MONTH */
  p += 4;
  *n++ = ONEBYTEINT(p);          /* PERIOD */
  p++;
  *n++ = TWOBYTEINT(p);          /* FORECAST MONTH */
  p += 2;
  *n++ = FOURBYTEINT(p);         /* REFERENCE DATE */
  p += 4;
  *n++ = FOURBYTEINT(p);         /* CLIMATE DATE (FROM) */
  p += 4;
  *n++ = FOURBYTEINT(p);         /* CLIMATE DATE (TO) */
  p += 4;
  *n++ = ONEBYTEINT(p);          /* THRESHOLD UNITS */
  p++;
  *n++ = ONEBYTEINT(p);          /* THRESHOLD INDICATOR */
  p++;
  *n++ = TWOBYTEINT(p);          /* LOWER THRESHOLD */
  p += 2;
  *n++ = TWOBYTEINT(p);          /* UPPER THRESHOLD */
  p += 2;

  *n++ = 0;                      /* SPARE (SET TO ZERO) */
  *n++ = 0;
  *n++ = 0;
  *n++ = 0;
  p += 4;

  byteLength = 40 + (fortint) (p - gribSection1);

  if( *bitPointer ) *bitPointer += (byteLength-40)*8;

  return (fortint) (n - outputArray);
}

fortint d_def_23(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {

  return d_def_23_(outputArray,gribSection1,bitPointer);
}

fortint e_def_24_(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {
/*
// Encodes ECMWF local definition 24 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition24.html
//
// If *bitPointer is not equal to zero, the length of the GRIB section
// 1 is stored in 3 bytes starting at (gribSection1-40).
// *bitPointer is updated by the number of bits in local definition 4.
//
// Returns the number of bytes (octets) in local definition 24 after
// byte 40.
*/
fortint byteLength;
int loop;
fortint * n = inputArray;
unsigned char * p = gribSection1;
fortint zero = 0;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  MOVE2BYTES(p,n);                /* Satellite identifier */
  p += 2;
  n++;

  MOVE2BYTES(p,n);                /* Instrument identifier */
  p += 2;
  n++;

  MOVE2BYTES(p,n);                /* Channel number */
  p += 2;
  n++;

  *p++ = ONEBYTE(n);n++;          /* Function code */

  byteLength = 40 + (fortint) (p-gribSection1);
  if( *bitPointer ) {
    MOVE3BYTES(gribSection1-40,&byteLength);
    *bitPointer += (byteLength-40)*8;
  }

  return (fortint) byteLength;
}

fortint e_def_24(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {

  return e_def_24_(inputArray,gribSection1,bitPointer);
}

fortint d_def_24_(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {
/*
// Decodes ECMWF local definition 24 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition24.html
//
// *bitPointer is updated by the number of bits in local definition 4.
//
// Returns the number of array elements inserted in outputArray
*/
fortint byteLength;
fortint * n = outputArray;
unsigned char * p = gribSection1;
unsigned char* total;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = TWOBYTEINT(p);          /* Satellite identifier */
  p += 2;

  *n++ = TWOBYTEINT(p);          /* Instrument identifier */
  p += 2;

  *n++ = TWOBYTEINT(p);          /* Channel number */
  p += 2;

  *n++ = ONEBYTEINT(p);          /* Function code */
  p++;

  byteLength = 40 + (fortint) (p - gribSection1);

  if( *bitPointer ) *bitPointer += (byteLength-40)*8;

  return (fortint) (n - outputArray);
}

fortint d_def_24(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {

  return d_def_24_(outputArray,gribSection1,bitPointer);
}

fortint e_def_25_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 25 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition25.html
//
// Returns the number of bytes (octets) in local definition 25 after
// byte 40.
*/
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = ONEBYTE(n);n++;          /* component index */
  *p++ = ONEBYTE(n);n++;          /* number of components */
  *p++ = ONEBYTE(n);n++;          /* model error type */

  return (fortint) (p-gribSection1);
}

fortint e_def_25(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_25_(inputArray,gribSection1);
}

fortint d_def_25_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 25 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition25.html
//
// Returns the number of array elements inserted in outputArray
*/
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = ONEBYTEINT(p);p++;      /* component index */
  *n++ = ONEBYTEINT(p);p++;      /* number of components */
  *n++ = ONEBYTEINT(p);p++;      /* model error type */

  return (fortint) (n - outputArray);
}

fortint d_def_25(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_25_(outputArray,gribSection1);
}

fortint e_def_50_(fortint * inputArray, unsigned char * gribSection1) {
/*
// Encodes ECMWF local definition 50 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition50.html
//
// Returns the number of bytes (octets) in local definition 50 after
// byte 40.
*/
int loop;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = ONEBYTE(n);n++;          /* NUMBER */
  *p++ = ONEBYTE(n);n++;          /* TOTAL */
  *p++ = ONEBYTE(n);n++;          /* MODEL */

  MOVE4BYTES(p,n);                /* NORTHWESTLAT */
  p += 4;
  n++;

  MOVE4BYTES(p,n);                /* NORTHWESTLON */
  p += 4;
  n++;

  MOVE4BYTES(p,n);                /* SOUTHWESTLAT */
  p += 4;
  n++;

  MOVE4BYTES(p,n);                /* SOUTHWESTLON */
  p += 4;
  n++;
/*
// ECMWF additions
*/
  *p++ = ONEBYTE(n);n++;          /* ORIGINALPARAM */
  *p++ = ONEBYTE(n);n++;          /* ORIGINALTABLE */

  for( loop = 70; loop < 116; loop++ )
    *p++ = (unsigned char) 0;     /* PADDING */
  n += 10;
/*
// Optional data
*/
  for( loop = 0; loop < 46; loop++) {   /* Optional data */
    MOVE4BYTES(p,n);
    p += 4;
    n++;
  }
/*
// Definition 50 has a fixed length section 1 (300 bytes)
*/
  return (fortint) (300-40);
}

fortint e_def_50(fortint * inputArray, unsigned char * gribSection1) {
  return e_def_50_(inputArray,gribSection1);
}

fortint d_def_50_(fortint * outputArray, unsigned char * gribSection1) {
/*
// Decodes ECMWF local definition 50 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition50.html
//
// Returns the number of array elements inserted in outputArray
*/
int loop;
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = ONEBYTEINT(p);p++;       /* NUMBER */
  *n++ = ONEBYTEINT(p);p++;       /* TOTAL */
  *n++ = ONEBYTEINT(p);p++;       /* MODEL */

  *n++ = FOURBYTEINT(p);          /* NORTHWESTLAT */
  p += 4;
  *n++ = FOURBYTEINT(p);          /* NORTHWESTLON */
  p += 4;
  *n++ = FOURBYTEINT(p);          /* SOUTHWESTLAT */
  p += 4;
  *n++ = FOURBYTEINT(p);          /* SOUTHWESTLON */
  p += 4;
/*
// ECMWF additions
*/
  *n++ = ONEBYTEINT(p);p++;       /* ORIGINALPARAM */
  *n++ = ONEBYTEINT(p);p++;       /* ORIGINALTABLE */

  for( loop = 0; loop < 10; loop++ )
    *n++ = 0;

  p += 46;
/*
// Optional data
*/
  for( loop = 0; loop < 46; loop++) {
    *n++ = FOURBYTEINT(p);
    p += 4;
  }

  return (fortint) (n - outputArray);
}

fortint d_def_50(fortint * outputArray, unsigned char * gribSection1) {
  return d_def_50_(outputArray,gribSection1);
}

fortint e_def_190_(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * collectedLocalDefinitionBytes,
  fortint * bitPointer) {
/*
// Encodes ECMWF local definition 190 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition190.html
//
// collectedLocalDefinitionBytes is an array containing the ready-prepared
// collected local definition bytes of the multiple ECMWF local definitions.
//
// If *bitPointer is not equal to zero, the length of the GRIB section
// 1 is stored in 3 bytes starting at (gribSection1-40).
// *bitPointer is updated by the number of bits in local definition 190.
//
// Returns the number of bytes (octets) in local definition 190 after
// byte 40.
*/
int loop, N;
fortint byteLength;
fortint totalBytes = 0;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = (unsigned char) 0;       /* ZERO1 */
  n++;
  *p++ = (unsigned char) 0;       /* ZERO2 */
  n++;
  *p++ = ONEBYTE(n);n++;          /* NUMBEROFDEFS */
  N = *(n-1);

  for( loop = 0; loop < N; loop++) {
    *p++ = ONEBYTE(n);n++;          /* ECMWF local definition number */
    totalBytes += *n;
    MOVE2BYTES(p,n);                /* Count of bytes */
    p += 2;
    n++;
  }
/*
// Transfer the ready-prepared collected local definition bytes
*/
  memcpy(p,collectedLocalDefinitionBytes,totalBytes);
 
  byteLength = 40 + 12 + N*3 + totalBytes;
  if( *bitPointer ) {
    MOVE3BYTES(gribSection1-40,&byteLength);
    *bitPointer += (byteLength-40)*8;
  }

  return (fortint) byteLength;
}

fortint e_def_190( fortint * inputArray,
                   unsigned char * gribSection1,
                   fortint * collectedLocalDefinitionBytes,
                   fortint * bitPointer) {
  return e_def_190_(inputArray,gribSection1,collectedLocalDefinitionBytes,
                    bitPointer);
}

fortint d_def_190_(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * collectedLocalDefinitionBytes,
  fortint * bitPointer) {
/*
// Decodes ECMWF local definition 190 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition190.html
//
// *bitPointer is updated by the number of bits in local definition 190.
//
// Returns the number of array elements inserted in outputArray
*/
int loop, N, byteLength;
fortint totalBytes = 0;
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = (fortint) 0;  p++;       /* ZERO1 */
  *n++ = (fortint) 0;  p++;       /* ZERO2 */
  *n++ = ONEBYTEINT(p);p++;       /* NUMBEROFDEFS */
  N = *(n-1);

  for( loop = 0; loop < N; loop++) {
    *n++ = ONEBYTEINT(p);p++;       /* ECMWF local definition number */
    *n++ = TWOBYTEINT(p);           /* Count of bytes */
    p += 2;
    totalBytes += *(n-1);
  }
/*
// Transfer the collected local definition bytes
*/
  memcpy(collectedLocalDefinitionBytes,p,totalBytes);

  byteLength = 40 + 12 + N*3 + totalBytes;

  if( *bitPointer ) *bitPointer += (byteLength-40)*8;

  return (fortint) ((totalBytes+sizeof(fortint)-1)/sizeof(fortint) +
                     n - outputArray);
}

fortint d_def_190( fortint * outputArray,
                   unsigned char * gribSection1,
                   fortint * collectedLocalDefinitionBytes,
                   fortint * bitPointer) {
  return d_def_190_(outputArray,gribSection1,collectedLocalDefinitionBytes,
                    bitPointer);
}

fortint e_def_191_(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {
/*
// Encodes ECMWF local definition 191 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition191.html
//
// If *bitPointer is not equal to zero, the length of the GRIB section
// 1 is stored in 3 bytes starting at (gribSection1-40).
// *bitPointer is updated by the number of bits in local definition 191.
//
// Returns the number of bytes (octets) in local definition 191 after
// byte 40.
*/
int loop, i, I, N, M, size = sizeof(fortint);
fortint byteLength;
fortint * n = inputArray;
unsigned char * p = gribSection1;

  encodeMarsPart(n,p);
  p += 9;
  n += 5;

  *p++ = (unsigned char) 0;       /* ZERO1 */
  n++;
  *p++ = (unsigned char) 0;       /* ZERO2 */
  n++;
  *p++ = ONEBYTE(n);n++;          /* MAJOR */
  *p++ = ONEBYTE(n);n++;          /* MINOR */
  *p++ = ONEBYTE(n);n++;          /* ORIGINALSUBCENTRE */

  for( loop = 0; loop < 4; loop++)
    *p++ = (unsigned char) 0;

  n += 4;

  MOVE2BYTES(p,n);                /* NUMBEROFBYTES */
  p += 2;
  n++;
  N = *(n-1);

  I = (N+size-1)/size;
  for( loop = 0; loop < I; loop++) {     /* Data descriptor bytes */
    unsigned char * np = (unsigned char *) n;
    for( i = 0; i < size; i++ ) {
      *p++ = *np++;
    }
    n++;
  }
/*
// The data entries are padded with zeroes to make upto a multiple of (60+80*M)
*/
  M = 60;
  do { M += 80; } while( M < (60 + N) );

  for( loop = N; loop < M; loop++) *p++ = (unsigned char) 0;
 
  byteLength = M;
  if( *bitPointer ) {
    MOVE3BYTES(gribSection1-40,&byteLength);
    *bitPointer += (byteLength-40)*8;
  }

  return (fortint) byteLength;
}

fortint e_def_191( fortint * inputArray,
                   unsigned char * gribSection1,
                   fortint * bitPointer) {
  return e_def_191_(inputArray,gribSection1,bitPointer);
}

fortint d_def_191_(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * bitPointer) {
/*
// Decodes ECMWF local definition 191 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition191.html
//
// *bitPointer is updated by the number of bits in local definition 191.
//
// Returns the number of array elements inserted in outputArray
*/
int loop, i, I, N, M, byteLength, size = sizeof(fortint);
fortint * n = outputArray;
unsigned char * p = gribSection1;

  decodeMarsPart(n,p);
  p += 9;
  n += 5;

  *n++ = (fortint) 0;  p++;       /* ZERO */
  *n++ = (fortint) 0;  p++;       /* ZERO */
  *n++ = ONEBYTEINT(p);p++;       /* MAJOR */
  *n++ = ONEBYTEINT(p);p++;       /* MINOR */
  *n++ = ONEBYTEINT(p);p++;       /* ORIGINALSUBCENTRE */

  for( loop = 0; loop < 4; loop++ ) {
    *n++ = (fortint) 0;
    p++;                          /* ZEROES */
  }

  *n++ = TWOBYTEINT(p);           /* NUMBEROFBYTES */
  N = *(n-1);
  p += 2;

  I = (N+size-1)/size;
  for( loop = 0; loop < I; loop++) {     /* Data descriptor bytes */
    unsigned char * np = (unsigned char *) n;
    for( i = 0; i < size; i++ ) {
      *np++ = *p++;
    }
    n++;
  }
/*
// The data entries are padded with zeroes to make upto a multiple of (60+80*M)
*/
  M = 60;
  do { M += 80; } while( M < (60 + N) );

  byteLength = M;

  if( *bitPointer ) *bitPointer += (byteLength-40)*8;

  return (fortint) (n - outputArray);
}

fortint d_def_191( fortint * outputArray,
                   unsigned char * gribSection1,
                   fortint * bitPointer) {
  return d_def_191_(outputArray,gribSection1,bitPointer);
}

fortint e_def_x_(
  fortint * definitionNumber,
  fortint * inputArray,
  unsigned char * gribSection1 ) {
/*
// Calls the encoding routine for the ECMWF local definition.
*/
int number = (int) *definitionNumber;
fortint zero = 0;

  switch( number ) {

    case 1:
      return e_def_1_(inputArray,gribSection1);

    case 2:
      return e_def_2_(inputArray,gribSection1);

    case 3:
      return e_def_3_(inputArray,gribSection1);

    case 4:
      return e_def_4_(inputArray,gribSection1,&zero) - 40;

    case 5:
      return e_def_5_(inputArray,gribSection1);

    case 6:
      return e_def_6_(inputArray,gribSection1,&zero) - 40;

    case 7:
      return e_def_7_(inputArray,gribSection1);

    case 8:
      return e_def_8_(inputArray,gribSection1);

    case 9:
      return e_def_9_(inputArray,gribSection1);

    case 10:
      return e_def_10_(inputArray,gribSection1,&zero) - 40;

    case 11:
      return e_def_11_(inputArray,gribSection1);

    case 13:
      return e_def_13_(inputArray,gribSection1,&zero) - 40;

    case 14:
      return e_def_14_(inputArray,gribSection1);

    case 15:
      return e_def_15_(inputArray,gribSection1);

    case 16:
      return e_def_16_(inputArray,gribSection1);

    case 17:
      return e_def_17_(inputArray,gribSection1,&zero) - 40;

    case 18:
      return e_def_18_(inputArray,gribSection1);

    case 19:
      return e_def_19_(inputArray,gribSection1);

    case 20:
      return e_def_20_(inputArray,gribSection1);

    case 21:
      return e_def_21_(inputArray,gribSection1);

    case 22:
      return e_def_22_(inputArray,gribSection1);

    case 23:
      return e_def_23_(inputArray,gribSection1,&zero) - 40;

    case 24:
      return e_def_24_(inputArray,gribSection1,&zero) - 40;

    case 50:
      return e_def_50_(inputArray,gribSection1);

    case 191:
      return e_def_191_(inputArray,gribSection1,&zero);

    default:
      return (fortint) -1;
  }
}

fortint e_def_x( fortint * definitionNumber,
                 fortint * outputArray,
                 unsigned char * gribSection1 ) {
  return e_def_x_(definitionNumber,outputArray,gribSection1);
}

fortint d_def_x_(
  fortint * definitionNumber,
  fortint * outputArray,
  unsigned char * gribSection1 ) {
/*
// Calls the encoding routine for the ECMWF local definition.
*/
int number = (int) *definitionNumber;
fortint zero = 0;

  switch( number ) {

    case 1:
      return d_def_1_(outputArray,gribSection1);

    case 2:
      return d_def_2_(outputArray,gribSection1);

    case 3:
      return d_def_3_(outputArray,gribSection1);

    case 4:
      return d_def_4_(outputArray,gribSection1,&zero);

    case 5:
      return d_def_5_(outputArray,gribSection1);

    case 6:
      return d_def_6_(outputArray,gribSection1,&zero);

    case 7:
      return d_def_7_(outputArray,gribSection1);

    case 8:
      return d_def_8_(outputArray,gribSection1);

    case 9:
      return d_def_9_(outputArray,gribSection1);

    case 10:
      return d_def_10_(outputArray,gribSection1,&zero);

    case 11:
      return d_def_11_(outputArray,gribSection1);

    case 13:
      return d_def_13_(outputArray,gribSection1,&zero);

    case 14:
      return d_def_14_(outputArray,gribSection1);

    case 15:
      return d_def_15_(outputArray,gribSection1);

    case 16:
      return d_def_16_(outputArray,gribSection1);

    case 17:
      return d_def_17_(outputArray,gribSection1,&zero);

    case 18:
      return d_def_18_(outputArray,gribSection1);

    case 19:
      return d_def_19_(outputArray,gribSection1);

    case 20:
      return d_def_20_(outputArray,gribSection1);

    case 21:
      return d_def_21_(outputArray,gribSection1);

    case 22:
      return d_def_22_(outputArray,gribSection1);

    case 23:
      return d_def_23_(outputArray,gribSection1,&zero);

    case 24:
      return d_def_24_(outputArray,gribSection1,&zero);

    case 50:
      return d_def_50_(outputArray,gribSection1);

    case 191:
      return d_def_191_(outputArray,gribSection1,&zero);

    default:
      return (fortint) -1;
  }
}

fortint d_def_x( fortint * definitionNumber,
                 fortint * outputArray,
                 unsigned char * gribSection1 ) {
  return d_def_x_(definitionNumber,outputArray,gribSection1);
}

fortint e_190_x_(
  fortint * inputArray,
  unsigned char * gribSection1,
  fortint * collectedLocalDefinitionBytes) {
/*
// Encodes ECMWF local definition 190 (from byte 41 onwards) into
// gribSection1 using integer array values in inputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition190.html
//
// collectedLocalDefinitionBytes is an array containing the ready-prepared
// collected local definition bytes of the multiple ECMWF local definitions.
//
// Returns the number of bytes (octets) in local definition 190 after
// byte 40.
*/
fortint zero = 0;

  return e_def_190_(inputArray,gribSection1,collectedLocalDefinitionBytes,&zero);

}

fortint e_190_x( fortint * inputArray,
                 unsigned char * gribSection1,
                 fortint * collectedLocalDefinitionBytes) {
  return e_190_x_(inputArray,gribSection1,collectedLocalDefinitionBytes);
}

fortint d_190_x_(
  fortint * outputArray,
  unsigned char * gribSection1,
  fortint * collectedLocalDefinitionBytes) {
/*
// Decodes ECMWF local definition 190 (from byte 41 onwards) from
// gribSection1 into integer array values in outputArray.
//
// inputArray[0] corresponds to KSEC1(37) in the description at
// http://www.ecmwf.int/publications/manuals/libraries/gribex/localDefinition190.html
//
// Returns the number of array elements inserted in outputArray
*/
fortint zero = 0;

 return d_def_190_(outputArray,gribSection1,collectedLocalDefinitionBytes,&zero);

}

fortint d_190_x( fortint * outputArray,
                 unsigned char * gribSection1,
                 fortint * collectedLocalDefinitionBytes) {
  return d_190_x_(outputArray,gribSection1,collectedLocalDefinitionBytes);
}

fortint jmemove_(
  fortint * destination,
  fortint * destinationOffset,
  fortint * source,
  fortint * sourceOffset,
  fortint * length ) {
unsigned char* Destination = (unsigned char*)destination + (*destinationOffset);
unsigned char* Source = (unsigned char*)source + (*sourceOffset);
size_t n = (size_t) (*length);
void * result;

  result = memmove(Destination, Source, n);
  if( result == NULL )
    return (fortint) -1;
  else
    return (fortint) ((*destinationOffset)+ (*length));
}

fortint jmemove( fortint * destination,
                 fortint * destinationOffset,
                 fortint * source,
                 fortint * sourceOffset,
                 fortint * length ) {
  return jmemove_(destination,destinationOffset,source,sourceOffset,length);
}
