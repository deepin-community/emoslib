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
#include <stdlib.h>
#include <memory.h>

#include "common/fortint.h"

#ifdef FORTRAN_NO_UNDERSCORE
#define GRPRS1B grprs1b
#else
#define GRPRS1B grprs1b_
#endif

void GRPRS1B(fortint*);
fortint dldefs_(fortint*,fortint*,fortint*,fortint*,fortint*);
fortint d_def_x(fortint*,fortint*,unsigned char*);

void grpr190(fortint* ksec1) {
/*
// Decodes the local definition bytes for the multiple local definitions
// in definition 190 and displays them via calls to Fortran routine GRPRS1B.
*/
unsigned char* bytes;
fortint numberOfDefinitions = ksec1[43];
unsigned char* startByte = (unsigned char*) &ksec1[44+2*ksec1[43]];
fortint numberOfBytes = ksec1[45];
fortint offset = 36*sizeof(fortint);
fortint definitionNumber = ksec1[44];
fortint loop, status, numberOfUnpackedIntegers, numberOfPackedBytes;
static int first = 1;
fortint* pBytes;

  if( first ) {
    setbuf(stdout,NULL);
    first = 0;
  }

  for( loop = 0; loop < numberOfDefinitions; loop++ ) {
    bytes = (unsigned char*) malloc(numberOfBytes*sizeof(fortint)+offset);
/*
//  Decode the bytes into INTEGERS
*/
/*
//  Code for libemos version 000264 ..
//
//  status = dldefs(&definitionNumber,startByte,(bytes+offset),
//                  &numberOfUnpackedIntegers,&numberOfPackedBytes);
*/
    pBytes = (fortint*) (bytes+offset);
    status = d_def_x(&definitionNumber,pBytes,startByte);
/*
//  Display the values.
*/
    printf("\n ECMWF local definition number = %d\n",definitionNumber);
    GRPRS1B((fortint*)bytes);

    free(bytes);
    startByte += numberOfBytes;
    numberOfBytes = ksec1[47+loop*2];
    definitionNumber = ksec1[46+loop*2];
  }

  return;
}

void grpr190_(fortint* ksec1) { grpr190(ksec1); return; }
