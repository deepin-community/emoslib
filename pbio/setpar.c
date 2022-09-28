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

/*
//  setpar.c
//
//  This is a replacement for the SETPAR.f FORTRAN-callable subroutine.
//  It sets the number of bits per word (kbit),
//  and the largest negative number (kneg).
//  The diagnostic print option is no longer supported (kpar).
//
*/
#include <limits.h>
#include "common/fortint.h"

void setpar_(fortint * kbit, fortint * kneg, fortint * kpar)
{
    *kbit = sizeof(fortint)*8 ;
#ifdef INTEGER_IS_INT
    *kneg = INT_MIN ;
#else
    *kneg = LONG_MIN ;
#endif
    return;
}

void setpar(fortint * kbit, fortint * kneg, fortint * kpar) {

  setpar_(kbit,kneg,kpar);
}
