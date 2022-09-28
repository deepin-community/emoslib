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

#include "bitmap.h"
#include "common/fortint.h"

fortint GMAPBIT(
  char ** bitmap,
  fortint * columnCount,
  fortint * row,
  fortint * column) {
/*
// Returns the bit at position (row,column) of bitmap.
//
// Called from FORTRAN:
//
// VALUE = GMAPBIT(BITMAP,COLCNT,N,M)
//
// where:
//
// BITMAP is an array containing the bitmap.
// COLCNT is the number of values per row.
// N is the row number (starting from 1)
// M is the column number (starting from 1)
//
*/
int bitNumber, byte, bit;
char value;

  bitNumber = ((*row)-1)*(*columnCount) + ((*column)-1);
  byte = bitNumber / 8;
  bit  = bitNumber % 8;
  value = ((*bitmap)[byte] >> (7-bit)) & 0x01;

  return (fortint) value;
}
