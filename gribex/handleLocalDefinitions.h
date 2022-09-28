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
#include "grib_int_t.h"

#ifndef UNPACK_H
#define UNPACK_H

#define ONE_BYTE_INT(a)   (int) ( *(a) & 0xff )
#define TWO_BYTE_INT(a)   (int) ( ONE_BYTE_INT(a)<<8 | ONE_BYTE_INT(a+1))
#define THREE_BYTE_INT(a) (int) (TWO_BYTE_INT((a))<<8 | ONE_BYTE_INT(a+2))
#define FOUR_BYTE_INT(a)  (int) (THREE_BYTE_INT((a))<<8 | ONE_BYTE_INT(a+3))

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


#define NUMBER(x) (sizeof(x)/sizeof(x[0]))
#define OCTETSTART 41
#define KSEC1START 37

#define NEQUAL(a,b) (strcmp((a),(b)) != 0)
#define EQUAL(a,b)  (strcmp((a),(b)) == 0)

typedef struct action action;

typedef struct buffer {
grib_int_t    totalPackedBytes;
grib_int_t    totalUnPackedValues;
grib_int_t    totalUnPackedBytes;
grib_int_t*   values;
grib_int_t*   startKsec1;
grib_octet_t* packed;
grib_octet_t* startOctets;
} buffer;

char* findLocalDefinitionFile(grib_int_t);

void encode(action*,buffer*);
void decode(action*,buffer*);

typedef action* (*pack_proc)(action*,buffer*);
typedef action* (*unpack_proc)(action*,buffer*);
typedef int (*test_proc)(grib_int_t,grib_int_t);

struct action {
 grib_string_t    *description;
 grib_string_t    *octet;
 grib_string_t    *code;
 grib_string_t    *ksec1;
 grib_string_t    *count;
struct action* next;
pack_proc      pack;
unpack_proc    unpack;

grib_int_t           value;

test_proc      test;
grib_int_t           sum;

struct action* reference;
struct action* end;

} ;

/*==========================================================*/

typedef void (*init_proc)(action*,action*,char*,char*,char*,char*,char*);

typedef struct op_code {
        grib_string_t *description;
        init_proc   init;
        pack_proc   pack;
        unpack_proc unpack;
        test_proc   test;
} op_code;

/*==========================================================*/

action* createLocalDefinitionHandler(grib_int_t);

int encodeLocalDefinitionValues(action*,grib_int_t*,grib_octet_t*,grib_int_t*,grib_int_t*);

int decodeLocalDefinitionValues(action*,grib_octet_t*,grib_int_t*,grib_int_t*,grib_int_t*);

void releaseAction(action*);

#endif /* End of UNPACK_H */
