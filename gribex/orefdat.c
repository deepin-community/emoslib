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

#include "common/fortint.h"

fortint orefdat_(fortint* ksec1) {
/*
// Fortran-callable:
//
//      INTEGER OREFDAT
//      EXTERNAL OREFDAT
//
//      INDEX = OREFDAT(KSEC1)
//
// where KSEC1 contains GRIB section 1 after unpacking by GRIBEX.
//
// Returns the index of the reference date in the post-auxiliary array in
// ECMWF local definition 4 (ocean data).
//
// Returns -1 if
//  -  local definition 4 is not in use, or
//  -  the post-auxiliary array is empty, or
//  -  the post-auxiliary array does not contain a reference date.
*/
fortint offset;

  if( ksec1[36] != 4 )  return -1;

  offset = 74 + ksec1[70] + ksec1[71] + ksec1[72] + ksec1[73];
  if( offset == 0 ) return -1;

  if( ksec1[offset] < 5 )
    return -1;
  else
    return (offset+5);
}

fortint orefdat(fortint* ksec1) {
  return orefdat_(ksec1);
}

fortint d13flag(fortint* kgrib) {
/*
// Fortran-callable:
//
//      INTEGER D13FLAG
//      EXTERNAL D13FLAG
//
//      FLAG = D13FLAG(KGRIB)
//
// where KGRIB contains GRIB 1 before unpacking by GRIBEX.
//
// Returns the Flag to show inclusions in the header in ECMWF GRIB section
// 1 local extension 13, or -1 if definition 13 is not present.
*/
unsigned char* p = ((unsigned char*) kgrib) + 7;

  if( *(p + 41) == 13 )
    return (fortint) *(p + 64);
  else
    return -1;
}

fortint d13flag_(fortint* kgrib) {
  return d13flag(kgrib);
}
