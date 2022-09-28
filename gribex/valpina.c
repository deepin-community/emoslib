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
#include "common/fortint.h"

#define CHARSIZE (long) (sizeof(char)*8)
#define LEASTSIGBIT 0x01

long bitmapValue(unsigned char * , long );
long bitmapValueTotal(unsigned char * , long, long );
fortint valpina(unsigned char * , fortint *, fortint * );
fortint valpina_(unsigned char * , fortint *, fortint * );
long separationBetweenValues(unsigned char * , long , long );
fortint numvals(unsigned char *, fortint *, fortint *);
fortint numvals_(unsigned char *, fortint *, fortint *);
fortint onebits(unsigned char *, fortint *);
fortint onebits_(unsigned char *, fortint *);

fortint numvals_(unsigned char * grib, fortint* istart, fortint* ifinish) {
/*
// Returns a count of the number of 1s in a GRIB between positions
// 'start' and 'finish'.
// If start = 0, the static values in the function are initialised.
*/
long start = (long) (*istart);
long finish = (long) (*ifinish);
static long oldTotal;
static long oldStart, oldFinish;
unsigned char * bitmap = grib;
static unsigned char * oldBitmap = 0;

  if( !start ) {
    oldBitmap = 0;
    oldTotal = 0;
    oldStart = oldFinish = 1;
    return (fortint) oldTotal;
  }

  if( oldBitmap != bitmap ) {
    oldBitmap = bitmap;
    oldStart = oldFinish = 1;
    oldTotal = 0;
  }

  if( start == finish ) {
    oldStart = oldFinish = finish;
    oldTotal = 0;
    return (fortint) oldTotal;
  }

  if( oldStart != start ) {
    oldTotal = bitmapValueTotal(bitmap, start+1, finish);
  }
  else {
    if( oldFinish < finish )
      oldTotal += bitmapValueTotal(bitmap, oldFinish+1, finish);
    else if( oldFinish > finish )
      oldTotal -= bitmapValueTotal(bitmap, finish+1, oldFinish);
  }

  oldStart = start;
  oldFinish = finish;

  return (fortint) oldTotal;

}

fortint numvals(unsigned char * grib, fortint* istart, fortint* ifinish) {
  return numvals_(grib,istart,ifinish);
}

fortint onebits_(unsigned char * grib, fortint* isection_3_offset) {
/*
// Returns a count of the number of 1s in a GRIB section 3 bitmap.
*/
long section_3_offset = (long) (*isection_3_offset);
unsigned char * bitmap = grib + section_3_offset;
long length, unused;
long number_of_bits, total;

  length = (*bitmap)<<16 | (*(bitmap+1)<<8) | *(bitmap+2);
  unused = *(bitmap+3);
  number_of_bits = ((length-6)*CHARSIZE) - unused;

  total = bitmapValueTotal((bitmap+6),1,number_of_bits);
  return ( (fortint) total );
}

fortint onebits(unsigned char * grib, fortint* isection_3_offset) {
  return onebits_(grib,isection_3_offset);
}

long bitmapValueTotal(unsigned char * bitmap, long start, long finish) {
/*
// Returns the count of 1 bits between start and finish in a bitmap.
*/
long total = 0;
unsigned char * first, * last, * next;
/*
  Lookup table to count number of 1s in a char
*/
static const char lookup[256] = {0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,
                                 1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
                                 1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
                                 2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
                                 1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
                                 2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
                                 2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
                                 3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
                                 1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
                                 2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
                                 2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
                                 3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
                                 2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
                                 3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
                                 3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
                                 4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8};
/*
  Masks to remove bits from left- and right-hand end of a char
*/
static const unsigned char bottomMask[8] =
  {0xff,0x7f,0x3f,0x1f,0x0f,0x07,0x03,0x01};
static const unsigned char topMask[8]    =
  {0xff,0xfe,0xfc,0xf8,0xf0,0xe0,0xc0,0x80};
int bitsToAdd, bitsToSubtract;

  first = bitmap + (start-1)/CHARSIZE;
  last  = bitmap + (finish-1)/CHARSIZE;


  bitsToAdd = (start-1)%CHARSIZE;
  total = lookup[(*first & bottomMask[bitsToAdd])];

  for( next = (first+1); next < last ; next++ )
    total += lookup[*next];

  if( last > first ) total += lookup[*last];

  bitsToSubtract = CHARSIZE - 1 - (finish-1)%CHARSIZE;
  total -= lookup[(*last & (~topMask[bitsToSubtract]))];

  return total;
}

long bitmapValue(unsigned char * bitmap, long index) {
/*
// Returns the value (0,1) of the bit at position 'index' in a bitmap.
*/
unsigned char * next;
int bitShift;

  next = bitmap + (index-1)/CHARSIZE;
  bitShift = CHARSIZE - 1 - ((index-1)%CHARSIZE);

  return  (((*next) >> bitShift) & LEASTSIGBIT);

}

fortint valpina_(unsigned char * grib, fortint* ioffset, fortint* iindex) {
/*
//  A GRIB product starts at 'grib' and contains missing/non-missing values
//  as described by a bitmap which is at position 'offset' in the GRIB.
//
//  'index' is the position of a point (missing/non-missing) in the field.
//  If index = 0, the static values in the function are initialised.
//
//  Examines the bitmap and returns:
//
//  - the actual index of a non-missing value
//
//  - 0 for a missing value
*/
long offset = (long) (*ioffset);
long index = (long) (*iindex);
unsigned char * bitmap = (grib + offset);
static unsigned char * oldBitmap;
static long count = 0;
static long oldIndex = 0;
long value;

  if( !(index) ) {
    oldBitmap = 0;
    count = 0;
    oldIndex = 0;
    return (fortint) 0;
  }

  if( oldBitmap != bitmap ) {
    oldBitmap = bitmap;
    count = 0;
    oldIndex = 0;
  }

  value = bitmapValue(bitmap, index);

  if( value ) {
    if( (index) != oldIndex ) {
      count += separationBetweenValues(bitmap, oldIndex, index);
      oldIndex = index;
    }
    return (fortint) count;
  }
  else
    return (fortint) 0;
}

fortint valpina(unsigned char * grib, fortint* ioffset, fortint* iindex) {
  return valpina_(grib,ioffset,iindex);
}

long separationBetweenValues(unsigned char * bitmap,long oldIndex,long index) {
/*
//  Counts the number of actual (non-missing) values between two locations
//  in the bitmap given by 'index' and 'oldIndex'.
//
//  The returned count can be positive or negative depending on whether
//  index is after or before oldIndex.
*/
long start, finish, sign = 1, total = 0;

  if( index > oldIndex ) {
    start = oldIndex;
    finish = index;
  }
  else {
    start = index;
    finish = oldIndex;
    sign = -1;
  }

  total = bitmapValueTotal(bitmap, (start+1), finish);

  return (sign*total);
}
