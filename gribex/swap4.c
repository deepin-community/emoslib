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

#include <memory.h>

void swap4_(char * version) {
/*
// Swaps the order of characters in experiment version (for little-endian)
//
// Called from FORTRAN:
//
//   CALL SWAP4(KSEC1(41))
*/
char copy[4];

  memcpy(copy,version,4);

  version[0] = copy[3];
  version[1] = copy[2];
  version[2] = copy[1];
  version[3] = copy[0];

  return;
}

void swap4(char * version) {
  swap4_(version);
}
