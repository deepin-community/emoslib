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
#ifndef SENCODE_H
#define SENCODE_H

#include <stdio.h>
#include <stdlib.h>
#include "common/fortint.h"
/*sinisa #include "getsetValues.h" */

#ifdef FORTRAN_NO_UNDERSCORE
#define SENCODE sencode
#define SENPACK senpack
#define RSTIME  rstime
#define ISTIME  istime
#define RSDATE  rsdate
#define ISDATE  isdate
#define SVALUES svalues
#define SPV     spv
#define CSECT4  csect4
#define GRSMKP  grsmkp
#define REF2GRB ref2grb
#define INSCAL  inscal
#define INXBIT  inxbit
#define JGETGG  jgetgg
#else
#define SENCODE sencode_
#define SENPACK senpack_
#define RSTIME  rstime_
#define ISTIME  istime_
#define RSDATE  rsdate_
#define ISDATE  isdate_
#define SVALUES svalues_
#define SPV     spv_
#define CSECT4  csect4_
#define GRSMKP  grsmkp_
#define REF2GRB ref2grb_
#define INSCAL  inscal_
#define INXBIT  inxbit_
#define JGETGG  jgetgg_
#endif

#endif /* End of SENCODE_H */

fortint SVALUES(gribProduct**,fortdouble*,fortint*,fortdouble*);
fortint CSECT4(fortdouble*,fortint*,fortint*,fortint*,fortint*,fortint*,
               fortint*,fortint*,fortint*);
void GRSMKP(fortint*);
fortint REF2GRB(fortdouble*,fortint*,fortint*,fortint*);
void INSCAL(fortdouble*,fortint*,fortint*,fortdouble*,fortdouble*,fortint*);
void INXBIT(fortint*,fortint*,fortint*,fortint*,fortint*,fortint*,fortint*,
            char*,fortint*,long);
void JGETGG(fortint*,unsigned char*,fortdouble*,fortint*,fortint*,long);

void setBitMap(unsigned char*,fortint);
