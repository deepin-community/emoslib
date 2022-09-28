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

#ifdef FORTRAN_NO_UNDERSCORE
#define JINDEX jindex
#else
#define JINDEX jindex_
#endif

fortint JINDEX(char * name, long len) {
/*
// Called from Fortran:
//
//   LEN = JINDEX(NAME)
//
//   Returns the length of the character string (including embedded
//   blanks).
*/
fortint offset = len;
char next;

  do {
    offset--;
    next = *(name+offset);
    if( next != ' ' ) {
      return (offset+1);
    }
  } while( offset >= 0 );

  return len;

}
