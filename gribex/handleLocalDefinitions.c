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
#include <string.h>
#include <ctype.h>

#include "handleLocalDefinitions.h"
#include "grib_int_t.h"

#define FREE_NOT_NULL(a) if((a)!=NULL) free((a))

void releaseAction(action* a) {
action* current = a;
action* previous;

  while (current != NULL) {
    FREE_NOT_NULL((char*)current->description);
    FREE_NOT_NULL((char*)current->octet);
    FREE_NOT_NULL((char*)current->code);
    FREE_NOT_NULL((char*)current->ksec1);
    FREE_NOT_NULL((char*)current->count);
    previous = current;
    current = current->next;
    FREE_NOT_NULL(previous);
  }

  return;
}

void init(
  action* first,
  action* a,
  char* description,
  char* octet,
  char* code,
  char* ksec1,
  char* count) {
/*
// Copies the text fields describing the action
*/
  memset(a,0,sizeof(*a));
  a->description = strdup(description);
  if( (a->description) == NULL ) {
    perror("init: strdup failed");
    exit(1);
  }
  a->octet = strdup(octet);
  if( (a->octet) == NULL ) {
    perror("init: strdup failed");
    exit(1);
  }
  a->code = strdup(code);
  if( (a->code) == NULL ) {
    perror("init: strdup failed");
    exit(1);
  }
  a->ksec1 = strdup(ksec1);
  if( (a->ksec1) == NULL ) {
    perror("init: strdup failed");
    exit(1);
  }
  a->count = strdup(count);
  if( (a->count) == NULL ) {
    perror("init: strdup failed");
    exit(1);
  }
  return;
}

action* packNOOP(action* a,buffer *b) { return a->next; }

action* unpackNOOP(action* a,buffer *b) { return a->next; }

grib_int_t stringIsNotANumber(const char * count) {
grib_int_t loop;

  for( loop = 0; loop < strlen(count); loop++ )
    if( ! isdigit(*(count+loop)) ) return 1;

  return 0;
}

/*==========================================================*/

void updateOffsets(action* a,buffer* b,grib_int_t* numberOfValues) {
/*
// Updates current values of packed byte offset and unpacked
// integer value offset and count when the local definition
// description has a value in the appropriate column.
*/
unsigned char* octetValue;
grib_int_t* ksec1Value;

  if( ! stringIsNotANumber(a->count) )
    *numberOfValues = atol(a->count);

  if( ! stringIsNotANumber(a->octet) ) {
    octetValue = (b->startOctets)+atol(a->octet)-OCTETSTART;
    (b->packed) = octetValue;
  }

  if( ! stringIsNotANumber(a->ksec1) ) {
    ksec1Value = (b->startKsec1)+atol(a->ksec1)-KSEC1START;
    (b->values) = ksec1Value;
  }

  return;
}

action* packPADMULT(action* a,buffer *b) {
/*
// Pad buffer with zeroes upto a multiple of 'count' bytes
*/
grib_int_t count, loop;
grib_int_t multiple = atol(a->count);
grib_int_t startOctet = atol(a->octet);

  count = (int)((b->packed)-(b->startOctets)) -(startOctet-OCTETSTART);
  count = ((count + multiple - 1)/multiple)*multiple - count;
  if( count == 0 ) count = multiple;

  for( loop = 0; loop < count; loop++ ) *(b->packed)++ = 0;

  b->totalPackedBytes += count;
  b->packed += count;
  b->totalUnPackedBytes += count;
  return a->next;
}

action* unpackPADMULT(action* a,buffer *b) {
grib_int_t count, loop;
grib_int_t multiple = atol(a->count);
grib_int_t startOctet = atol(a->octet);

  count = (int)((b->packed)-(b->startOctets)) -(startOctet-OCTETSTART);
  count = ((count + multiple - 1)/multiple)*multiple - count;
  if( count == 0 ) count = multiple;

  b->totalUnPackedBytes += count;
  b->packed += count;

  return a->next;
}

action* packPADFROM(action* a,buffer *b) {
/*
// Pad buffer with zeroes upto a multiple of 'count' bytes
// starting from byte 'octet'
*/
grib_int_t count, loop;
grib_int_t multiple = atol(a->count);
grib_int_t startByte = atol(a->octet);

  count = (int)((b->packed)-(b->startOctets)+OCTETSTART-1-startByte);
  count = ((count + multiple - 1)/multiple)*multiple - count;

  for( loop = 0; loop < count; loop++ ) *(b->packed)++ = 0;

  b->totalPackedBytes += count;
  return a->next;
}

action* unpackPADFROM(action* a,buffer *b) {
/*
// Skip padding in buffer upto a multiple of 'count' bytes
// starting from byte 'octet'
*/
grib_int_t count;
grib_int_t multiple = atol(a->count);
grib_int_t startByte = atol(a->octet);

  count = (grib_int_t)((b->packed)-(b->startOctets)) +
                       OCTETSTART - 1 -startByte;
  while( count > 0 ) {
    count -= multiple;
  }
  count = -count;

  b->totalUnPackedBytes += count;
  (b->packed) += count;

  return a->next;
}

action* packUP_TO(action* a,buffer *b, unsigned char character) {
/*
// Pad buffer with 'characters's upto given byte
*/
grib_int_t count = 0, loop;

  updateOffsets(a,b,&count);

  if( count != 0 ) {

    count = atoi(a->count) - (int)((b->packed)-(b->startOctets)+OCTETSTART-1);

    for( loop = 0; loop < count; loop++ ) *(b->packed)++ = character;

    b->totalPackedBytes += count;
  }
  return a->next;
}

action* packSP_TO(action* a,buffer *b) {
/*
// Pad buffer with spaces upto given byte
*/
  return packUP_TO(a,b,' ');
}

action* unpackSP_TO(action* a,buffer *b) {
grib_int_t count = 0, loop;

  updateOffsets(a,b,&count);
  b->totalUnPackedBytes = count + 1 - OCTETSTART;
  b->packed = b->startOctets + count + 1 - OCTETSTART;

  return a->next;
}



action* packPADTO(action* a,buffer *b) {
/*
// Pad buffer with zeroes upto given byte
*/
  return packUP_TO(a,b,'\0');
}

action* unpackPADTO(action* a,buffer *b) {
grib_int_t count = 0, loop;

  updateOffsets(a,b,&count);
  b->totalUnPackedBytes = count + 1 - OCTETSTART;
  b->packed = b->startOctets + count + 1 - OCTETSTART;

  return a->next;
}

action* packBYTES(action* a,buffer *b) {
/*
// Fill buffer with a string of bytes
*/
grib_int_t count, numberOfBytes, numberOfValues;

  if( stringIsNotANumber(a->count) )
    numberOfBytes = (a->reference->value);
  else
    numberOfBytes = atoi(a->count);

  numberOfValues = (numberOfBytes+sizeof(grib_int_t)-1) / sizeof(grib_int_t);

  updateOffsets(a,b,&count);

  memcpy((b->packed),(b->values),numberOfBytes);

  (b->packed) += numberOfBytes;
  (b->values) += numberOfValues;

  b->totalPackedBytes += numberOfBytes;
  return a->next;
}

action* unpackBYTES(action* a,buffer *b) {
/*
// Unpack a string of bytes from buffer
*/
grib_int_t count, numberOfBytes, numberOfValues;

  if( stringIsNotANumber(a->count) )
    numberOfBytes = (a->reference->value);
  else
    numberOfBytes = atoi(a->count);

  numberOfValues = (numberOfBytes+sizeof(grib_int_t)-1) / sizeof(grib_int_t);

  updateOffsets(a,b,&count);

  memcpy((b->values),(b->packed),numberOfBytes);

  (b->packed) += numberOfBytes;
  (b->values) += numberOfValues;

  b->totalUnPackedBytes += numberOfBytes;
  b->totalUnPackedValues += numberOfValues;

  return a->next;
}

action* packF1(action* a,buffer *b) {
/*
// Pack a flag (=1) into buffer
*/
grib_int_t count;
grib_int_t* flag;

  flag = (grib_int_t*) malloc(sizeof(grib_int_t));
  if( flag == NULL ) {
    perror("packF1: malloc failed in packF1");
    exit(1);
  }

  *flag = atol(a->count);
  (a->value) = *flag;
  updateOffsets(a,b,&count);

  MOVE1BYTE((b->packed),flag);
  (b->packed)++;

  b->totalPackedBytes++;
  return a->next;
}

action* packD3(action* a,buffer *b) {
/*
// Pack a 3-byte date modified by 19000000 if necessary
// (see definitions 6 and 17)
*/
grib_int_t numberOfValues = 1, loop;
grib_int_t date;

  updateOffsets(a,b,&numberOfValues);
/*
// Save values which may be loop count and/or a running summation in a list
*/
  (a->value) = *(b->values);
/*
//(a->sum) += *(b->values);
*/

  for( loop = 0; loop < numberOfValues; loop++ ) {
    date = *(b->values)++;
    if( date > 19000000 ) date -= 19000000;
    MOVE3BYTES((b->packed),&date);
    (b->packed) += 3;
  }

  b->totalPackedBytes += numberOfValues*3;
  return a->next;
}

action* unpackD3(action* a,buffer *b) {
/*
// Unpack a 3-byte date modified by 19000000 if necessary
// (see definitions 6 and 17)
*/
grib_int_t numberOfValues = 1, loop;
grib_int_t date;

  updateOffsets(a,b,&numberOfValues);

  for( loop = 0; loop < numberOfValues; loop++ ) {
    date = THREE_BYTE_INT((b->packed));
    if( (date < 19000000) && (date > 100 ) ) date += 19000000;
    *(b->values) = date;
    (b->packed) += 3;
    b->totalUnPackedBytes += 3;
    (b->values)++;
  }
/*
// Save value which is a loop count
*/
  if( numberOfValues > 1 )
    (a->value) = numberOfValues;
  else
    (a->value) = date;
  (a->sum) += *(b->values);

  b->totalUnPackedValues += numberOfValues;
  return a->next;
}

action* packSn(action* a,buffer *b, grib_int_t n) {
/*
// Pack a signed n-byte value
// Negative values are stored as a positive value with the most significant
// bit set to 1
*/
grib_int_t numberOfValues = 1, loop;
grib_int_t value;

  updateOffsets(a,b,&numberOfValues);

  for( loop = 0; loop < numberOfValues; loop++ ) {
    value = *(b->values)++;
    switch( (int) n ) {
      case 1:
        if( value < 0 ) value = - (value & 0x7f);
        MOVE1BYTE((b->packed),&value);
        break;
      case 2:
        if( value < 0 ) value = - (value & 0x7fff);
        MOVE2BYTES((b->packed),&value);
        break;
      case 3:
        if( value < 0 ) value = - (value & 0x7fffff);
        MOVE3BYTES((b->packed),&value);
        break;
      case 4:
        if( value < 0 ) value = - (value & 0x7fffffff);
        MOVE4BYTES((b->packed),&value);
        break;
      default:
        fprintf(stderr,"packSn: %d not yet handled\n",n);
        exit(1);
    }
    (b->packed) += n;
  }

  b->totalPackedBytes += numberOfValues*n;
  return a->next;
}

action* packS1(action* a,buffer *b) { return packSn(a,b,1); }

action* packS2(action* a,buffer *b) { return packSn(a,b,2); }

action* packS3(action* a,buffer *b) { return packSn(a,b,3); }

action* packS4(action* a,buffer *b) { return packSn(a,b,4); }


action* unpackSn(action* a,buffer *b, grib_int_t n) {
/*
// Unpack a signed n-byte value
// Negative values are stored as a positive value with the most significant
// bit set to 1
*/
grib_int_t numberOfValues = 1, loop;
grib_int_t value;

  updateOffsets(a,b,&numberOfValues);

  for( loop = 0; loop < numberOfValues; loop++ ) {
    switch( (int) n ) {
      case 1:
        value = ONE_BYTE_INT((b->packed));
        if( value & 0x80 ) value = - (value & 0x7f);
        *(b->values) = value;
        break;
      case 2:
        value = TWO_BYTE_INT((b->packed));
        if( value & 0x8000 ) value = - (value & 0x7fff);
        *(b->values) = value;
        break;
      case 3:
        value = THREE_BYTE_INT((b->packed));
        if( value & 0x800000 ) value = - (value & 0x7fffff);
        *(b->values) = value;
        break;
      case 4:
        value = FOUR_BYTE_INT((b->packed));
        if( value & 0x80000000 ) value = - (value & 0x7fffffff);
        *(b->values) = value;
        break;
      default:
        fprintf(stderr,"unpackSn: %d not yet handled\n",n);
        exit(1);
    }
    (b->packed) += n;
    (b->values)++;
  }
  if( numberOfValues > 1 )
    (a->value) = numberOfValues;
  else
    (a->value) = value;

  b->totalUnPackedBytes += numberOfValues * n;
  b->totalUnPackedValues += numberOfValues;

  return a->next;
}

action* unpackS1(action* a,buffer *b) { return unpackSn(a,b,1); }

action* unpackS2(action* a,buffer *b) { return unpackSn(a,b,2); }

action* unpackS3(action* a,buffer *b) { return unpackSn(a,b,3); }

action* unpackS4(action* a,buffer *b) { return unpackSn(a,b,4); }


action* packIn(action* a,buffer *b, grib_int_t n) {
/*
// Pack an n-byte value
*/
grib_int_t numberOfValues = 1, loop;

  updateOffsets(a,b,&numberOfValues);

  for( loop = 0; loop < numberOfValues; loop++ ) {
    switch( (int) n ) {
      case 1:
        MOVE1BYTE((b->packed),(b->values));
        break;
      case 2:
        MOVE2BYTES((b->packed),(b->values));
        break;
      case 3:
        MOVE3BYTES((b->packed),(b->values));
        break;
      case 4:
        MOVE4BYTES((b->packed),(b->values));
        break;
      default:
        fprintf(stderr,"packIn: %d not yet handled\n",n);
        exit(1);
    }
    (b->packed) += n;
    (b->values)++;
  }

  b->totalPackedBytes += numberOfValues*n;
  return a->next;
}

action* packI1(action* a,buffer *b) {
/*
// Save values which may be loop count and/or a running summation in a list
*/
  (a->value) = *(b->values);
/*
//(a->sum) += *(b->values);
*/

  return packIn(a,b,1);
}

action* packI2(action* a,buffer *b) {
/*
// Save values which may be loop count and/or a running summation in a list
*/
  (a->value) = *(b->values);
/*
//(a->sum) += *(b->values);
*/

  return packIn(a,b,2);
}

action* packI3(action* a,buffer *b) {
/*
// Save values which may be loop count and/or a running summation in a list
*/
  (a->value) = *(b->values);
/*
//(a->sum) += *(b->values);
*/

  return packIn(a,b,3);
}

action* packI4(action* a,buffer *b) {
/*
// Save values which may be loop count and/or a running summation in a list
*/
  (a->value) = *(b->values);
/*
//(a->sum) += *(b->values);
*/

  return packIn(a,b,4);
}

action* packA4(action* a,buffer *b) {
/*
// Pack an 4-byte ASCII value
//
// Note: assumes that only one A4 value is being handled (count is -)
*/
  memcpy((b->packed),(b->values),4);

  (b->packed) +=4;
  (b->values)++;
  b->totalPackedBytes += 4;

  return a->next;
}

action* packA8(action* a,buffer *b) {
/*
// Pack an 8-byte ASCII value
//
// Note: assumes that only one A8 value is being handled (count is -)
*/
  memcpy((b->packed),(b->values),8);

  (b->packed) +=8;
  (b->values)++;
  (b->values)++;
  b->totalPackedBytes += 8;

  return a->next;
}

action* packA1(action* a,buffer *b) {
/*
// Pack an 1-byte ASCII value
*/
  return packIn(a,b,1);
}

action* packPAD(action* a,buffer *b) {
/*
// Pack buffer with zeroes
*/
grib_int_t zero = 0, loop, numberOfZeroes = atoi((a->count));

  updateOffsets(a,b,&numberOfZeroes);

  if( NEQUAL(a->octet,"n/a") ) {
    for( loop = 0; loop < numberOfZeroes; loop++ ) {
      MOVE1BYTE((b->packed),&zero);
      (b->packed)++;
    }
    b->totalPackedBytes += numberOfZeroes;
  }

  if( NEQUAL(a->ksec1,"n/a") ) (b->values) += numberOfZeroes;

  return a->next;
}

action* packLP_In(action* a,buffer *b, grib_int_t n) {
/*
// Packs a list of single n-byte values in buffer
*/
action* relatedAction;
grib_int_t count, numberOfValues, loop;
grib_int_t N = (n < 0) ? -n : n;

  relatedAction = a->reference;
  if( relatedAction == NULL ) {
    fprintf(stderr,"packLP_In: relatedAction '%s' not found for '%s'\n",
           (a->count), (a->description));
    exit(1);
  }
  numberOfValues = (relatedAction->value);

  if((numberOfValues>0)&&( n < 0 )) numberOfValues--;

  updateOffsets(a,b,&count);

  for( loop = 0; loop < numberOfValues; loop++ ) {
    switch( (int) N ) {
      case 1:
        MOVE1BYTE((b->packed),(b->values));
        break;
      case 2:
        MOVE2BYTES((b->packed),(b->values));
        break;
      case 3:
        MOVE3BYTES((b->packed),(b->values));
        break;
      case 4:
        MOVE4BYTES((b->packed),(b->values));
        break;
      default:
        fprintf(stderr,"packLP_In: %d not yet handled\n",N);
        exit(1);
    }
    (b->packed) += N;
    (b->values)++;
  }

  b->totalPackedBytes += (numberOfValues*N);
  return a->next;
}

action* packLP_I1(action* a,buffer *b) { return packLP_In(a,b,1); }

action* packLP_I2(action* a,buffer *b) { return packLP_In(a,b,2); }

action* packLP_I3(action* a,buffer *b) { return packLP_In(a,b,3); }

action* packLP_I4(action* a,buffer *b) { return packLP_In(a,b,4); }

action* packLP_I4M1(action* a,buffer *b) { return packLP_In(a,b,-4); }

/*=======================================*/

action* packLP_Sn(action* a,buffer *b, grib_int_t n) {
/*
// Packs a list of single n-byte values in buffer
*/
action* relatedAction;
grib_int_t count, numberOfValues, loop;
grib_int_t N = (n < 0) ? -n : n;
grib_int_t value;


  relatedAction = a->reference;
  if( relatedAction == NULL ) {
    fprintf(stderr,"packLP_Sn: relatedAction '%s' not found for '%s'\n",
           (a->count), (a->description));
    exit(1);
  }
  numberOfValues = (relatedAction->value);

  if((numberOfValues>0)&&( n < 0 )) numberOfValues--;

  updateOffsets(a,b,&count);

  for( loop = 0; loop < numberOfValues; loop++ ) {
    value = *(b->values)++;
    switch( (int) N ) {
      case 1:
        if( value < 0 ) value = - (value & 0x7f);
        MOVE1BYTE((b->packed),&value);
        break;
      case 2:
        if( value < 0 ) value = - (value & 0x7fff);
        MOVE2BYTES((b->packed),&value);
        break;
      case 3:
        if( value < 0 ) value = - (value & 0x7fffff);
        MOVE3BYTES((b->packed),&value);
        break;
      case 4:
        if( value < 0 ) value = - (value & 0x7fffffff);
        MOVE4BYTES((b->packed),&value);
        break;
      default:
        fprintf(stderr,"packLP_Sn: %d not yet handled\n",N);
        exit(1);
    }
    (b->packed) += N;
  }

  b->totalPackedBytes += (numberOfValues*N);
  return a->next;
}

action* packLP_S1(action* a,buffer *b) { return packLP_Sn(a,b,1); }

action* packLP_S2(action* a,buffer *b) { return packLP_Sn(a,b,2); }

action* packLP_S3(action* a,buffer *b) { return packLP_Sn(a,b,3); }

action* packLP_S4(action* a,buffer *b) { return packLP_Sn(a,b,4); }


/*=======================================*/

action* unpackF1(action* a,buffer *b) {
/*
// Skips a flag in buffer
*/
  (a->value) = *(b->packed);
  (b->packed)++;
  b->totalUnPackedBytes++;
  return a->next;
}

action* packL3(action* a,buffer *b) {
/*
// Does (almost) nothing
*/
  return a->next;
}

action* unpackL3(action* a,buffer *b) {
/*
// Unpack a 3-byte value without disturbing the buffer counts
*/
  *(b->values) = THREE_BYTE_INT((b->packed));
  (a->value) = *(b->values);

  return a->next;
}

action* unpackIn(action* a,buffer *b, grib_int_t n) {
/*
// Unpack an n-byte value
*/
grib_int_t numberOfValues = 1, loop;

  updateOffsets(a,b,&numberOfValues);

  if((numberOfValues>0)&&( n < 0 )) numberOfValues--;

  for( loop = 0; loop < numberOfValues; loop++ ) {
    switch( (int) n ) {
      case 1:
        *(b->values) = ONE_BYTE_INT((b->packed));
        break;
      case 2:
        *(b->values) = TWO_BYTE_INT((b->packed));
        break;
      case 3:
        *(b->values) = THREE_BYTE_INT((b->packed));
        break;
      case 4:
        *(b->values) = FOUR_BYTE_INT((b->packed));
        break;
      default:
        fprintf(stderr,"unpackIn: %d not yet handled\n",n);
        exit(1);
    }
/*
// Save value which may be loop count
*/
    (a->value) = *(b->values);
    (a->sum) += *(b->values);

    (b->packed) += n;
    (b->values)++;
  }

  b->totalUnPackedBytes += numberOfValues * n;
  b->totalUnPackedValues += numberOfValues;

  return a->next;
}

action* unpackI1(action* a,buffer *b) { return unpackIn(a,b,1); }

action* unpackI2(action* a,buffer *b) { return unpackIn(a,b,2); }

action* unpackI3(action* a,buffer *b) { return unpackIn(a,b,3); }

action* unpackI4(action* a,buffer *b) { return unpackIn(a,b,4); }

action* unpackA4(action* a,buffer *b) {
/*
// Unpacks a 4-byte ASCII value
//
// Note: assumes that only one A4 value is being handled (count is -)
*/

  memcpy((b->values),(b->packed),4);
  (a->value) = *(b->values);

  (b->packed) += 4;
  (b->values)++;

  b->totalUnPackedBytes += 4;
  b->totalUnPackedValues ++;

  return a->next;
}

action* unpackA8(action* a,buffer *b) {
/*
// Unpacks a 8-byte ASCII value
//
// Note: assumes that only one A8 value is being handled (count is -)
*/

  memcpy((b->values),(b->packed),8);
  (a->value) = *(b->values);

  (b->packed) += 8;
  (b->values) ++;
  (b->values) ++;

  b->totalUnPackedBytes += 8;
  b->totalUnPackedValues ++;
  b->totalUnPackedValues ++;

  return a->next;
}

action* unpackA1(action* a,buffer *b) { return unpackIn(a,b,1); }


action* unpackPAD(action* a,buffer *b) {
/*
// Unpack zeroes from buffer
*/
grib_int_t loop, numberOfZeroes = atoi((a->count)), zero = 0;

  if( NEQUAL(a->octet,"n/a") ) {
    if( NEQUAL(a->octet,"-") )
      (b->packed) =  (b->startOctets)+atoi(a->octet)-OCTETSTART;
    (b->packed) += numberOfZeroes;
    (b->totalUnPackedBytes) += numberOfZeroes;
  }

  if( NEQUAL(a->ksec1,"n/a") ) {
    if( NEQUAL(a->ksec1,"-") )
      (b->values) = (b->startKsec1)+atoi(a->ksec1)-KSEC1START;

    for( loop = 0; loop < numberOfZeroes; loop++ ) *(b->values)++ = 0;
    b->totalUnPackedValues += numberOfZeroes;
  }
  return a->next;
}

action* unpackLP_In(action* a,buffer *b, grib_int_t n) {
/*
// Unpacks a list of single n-byte values from buffer
*/
action* relatedAction;
grib_int_t count, numberOfValues, loop;
grib_int_t N = (n < 0) ? -n : n;

  updateOffsets(a,b,&count);

  relatedAction = a->reference;
  if( relatedAction == NULL ) {
    fprintf(stderr,"unpackLP_In: relatedAction '%s' not found for '%s'\n",
           (a->count), (a->description));
    exit(1);
  }
  numberOfValues = (relatedAction->value);

  if((numberOfValues>0)&&( n < 0 )) numberOfValues--;

  for( loop = 0; loop < numberOfValues; loop++ ) {
    switch( (int) N ) {
      case 1:
        *(b->values) = ONE_BYTE_INT((b->packed));
        (b->packed) += 1;
        b->totalUnPackedBytes += 1;
        break;
      case 2:
        *(b->values) = TWO_BYTE_INT((b->packed));
        (b->packed) += 2;
        b->totalUnPackedBytes += 2;
        break;
      case 3:
        *(b->values) = THREE_BYTE_INT((b->packed));
        (b->packed) += 3;
        b->totalUnPackedBytes += 3;
        break;
      case 4:
        *(b->values) = FOUR_BYTE_INT((b->packed));
        (b->packed) += 4;
        b->totalUnPackedBytes += 4;
        break;
      default:
        fprintf(stderr,"unpackLP_In: %d not yet handled\n",N);
        exit(1);
    }
    (b->values)++;
  }

  b->totalUnPackedValues += numberOfValues;
  return a->next;
}

action* unpackLP_I1(action* a,buffer *b) { return unpackLP_In(a,b,1); }

action* unpackLP_I2(action* a,buffer *b) { return unpackLP_In(a,b,2); }

action* unpackLP_I3(action* a,buffer *b) { return unpackLP_In(a,b,3); }

action* unpackLP_I4(action* a,buffer *b) { return unpackLP_In(a,b,4); }

action* unpackLP_I4M1(action* a,buffer *b) { return unpackLP_In(a,b,-4); }

/*==========================================================*/

action* unpackLP_Sn(action* a,buffer *b, grib_int_t n) {
/*
// Unpacks a list of single n-byte values from buffer
*/
action* relatedAction;
grib_int_t count, numberOfValues, loop;
grib_int_t N = (n < 0) ? -n : n;
grib_int_t value;

  updateOffsets(a,b,&count);

  relatedAction = a->reference;
  if( relatedAction == NULL ) {
    fprintf(stderr,"unpackLP_Sn: relatedAction '%s' not found for '%s'\n",
           (a->count), (a->description));
    exit(1);
  }
  numberOfValues = (relatedAction->value);

  if((numberOfValues>0)&&( n < 0 )) numberOfValues--;

  for( loop = 0; loop < numberOfValues; loop++ ) {

    switch( (int) N ) {
      case 1:
        value = ONE_BYTE_INT((b->packed));
        if( value & 0x80 ) value = - (value & 0x7f);
        *(b->values) = value;
        (b->packed) += 1;
        b->totalUnPackedBytes += 1;
        break;
      case 2:
        value = TWO_BYTE_INT((b->packed));
        if( value & 0x8000 ) value = - (value & 0x7fff);
        *(b->values) = value;
        (b->packed) += 2;
        b->totalUnPackedBytes += 2;
        break;
      case 3:
        value = THREE_BYTE_INT((b->packed));
        if( value & 0x800000 ) value = - (value & 0x7fffff);
        *(b->values) = value;
        (b->packed) += 3;
        b->totalUnPackedBytes += 3;
        break;
      case 4:
        value = FOUR_BYTE_INT((b->packed));
        if( value & 0x80000000 ) value = - (value & 0x7fffffff);
        *(b->values) = value;
        (b->packed) += 4;
        b->totalUnPackedBytes += 4;
        break;
      default:
        fprintf(stderr,"unpackLP_Sn: %d not yet handled\n",N);
        exit(1);
    }
    (b->values)++;
  }

  b->totalUnPackedValues += numberOfValues;
  return a->next;
}

action* unpackLP_S1(action* a,buffer *b) { return unpackLP_Sn(a,b,1); }

action* unpackLP_S2(action* a,buffer *b) { return unpackLP_Sn(a,b,2); }

action* unpackLP_S3(action* a,buffer *b) { return unpackLP_Sn(a,b,3); }

action* unpackLP_S4(action* a,buffer *b) { return unpackLP_Sn(a,b,4); }


/*==========================================================*/

action* packLIST(action* a,buffer *b) {
/*
// Packs a list of several values into buffer
*/
grib_int_t count = a->reference->value;
grib_int_t i;

  (a->sum) = 0;
  for(i = 0; i < count; i++) {
    encode(a->next,b);
  }

  return a->end->next;
}

action* unpackLIST(action* a,buffer *b) {
/*
// Unpacks a list of several values from buffer
*/
grib_int_t count = a->reference->value;
grib_int_t i;

  for(i = 0; i < count; i++) {
    decode(a->next,b);
  }

  return a->end->next;
}

/*==========================================================*/


void init_reference(
  action* first,
  action* a,
  char* description,
  char* octet,
  char* ksec1,
  char* count)
{
/*
// Finds a reference from a 'count' field to a previous action
// which stores the count value
*/

  if( NEQUAL(count,"-") ) {
    while(first)
    {
      if( EQUAL(first->description,count) ) {
        a->reference = first;
        return;
      }
      first = first->next;
    }
  }
  a->reference = NULL;

}

/*==========================================================*/
action* packENDLIST(action* a,buffer *b) { return 0; }

action* unpackENDLIST(action* a,buffer *b) { return 0; }

action* packENDIF(action* a,buffer *b) { return 0; }

action* unpackENDIF(action* a,buffer *b) { return 0; }

void initENDLIST(
  action* first,
  action* a,
  char* description,
  char* octet,
  char* code,
  char* ksec1,
  char* count) {
/*
// Finds the start action described in a 'count' field
*/
  init(first,a,description,octet,code,ksec1,count);

  while(first) {
    if( EQUAL(first->description,count) ) {
      first->end = a;
      return;
    }
    first = first->next;
  }

  fprintf(stderr,"initENDLIST: Cannot find end loop (%s)",count);
  exit(1);
}

void initENDIF(
  action* first,
  action* a,
  char* description,
  char* octet,
  char* code,
  char* ksec1,
  char* count) {
/*
// Finds the start action described in a 'ksec1' field
*/
  init(first,a,description,octet,code,ksec1,count);

  while(first) {
    if( EQUAL(first->description,ksec1) ) {
      first->end = a;
      return;
    }
    first = first->next;
  }

  fprintf(stderr,"initENDIF: Cannot find end loop (%s)",ksec1);
  exit(1);
}

/*==========================================================*/

int test_eq(grib_int_t a,grib_int_t b)  { return a == b; }
int test_ne(grib_int_t a,grib_int_t b)  { return a != b; }
int test_ge(grib_int_t a,grib_int_t b)  { return a >= b; }
int test_gt(grib_int_t a,grib_int_t b)  { return a > b;  }
int test_le(grib_int_t a,grib_int_t b)  { return a <= b; }
int test_lt(grib_int_t a,grib_int_t b)  { return a < b;  }
int test_bit(grib_int_t a,grib_int_t b) { return (a & (1 << b)) != 0;  }

/*==========================================================*/

action* pack_if(action* a,buffer *b)
{
grib_int_t val1 = a->reference->value;
grib_int_t val2 = atol(a->ksec1);

  if(a->test(val1,val2)) encode(a->next,b);

  return a->end->next;
}

action* unpack_if(action* a,buffer *b)
{
grib_int_t val1 = a->reference->value;
grib_int_t val2 = atol(a->ksec1);

  if(a->test(val1,val2)) decode(a->next,b);

  return a->end->next;
}

action* createLocalDefinition(action*,grib_int_t);

action* packLOCAL(action* a,buffer *b)
{
grib_int_t localDefinitionNumber;
grib_int_t length;
action* local;
action next;
grib_int_t* lengthValue = (b->values) - 1;
grib_octet_t* packedLengthValue = (b->packed) - 2;

/*
// Setup ccc = 98, sss = 000, nnn = number
*/
  localDefinitionNumber = *(b->values);
  localDefinitionNumber += 98000000;

  local = createLocalDefinition(a,localDefinitionNumber);
  local = local->next;

  while( local ) {
    memmove(&next,local,sizeof(action));
    next.next = NULL;
    encode(&next,b);
    local = local->next;
  }

  length = (b->values) - lengthValue - 1;

  *lengthValue = length;
  MOVE2BYTES(packedLengthValue,&length);

  return NULL;
}

action* unpackLOCAL(action* a,buffer *b)
{
grib_int_t localDefinitionNumber;
action* local;
action next;
grib_int_t* lengthValue = (b->values) - 1;
grib_octet_t* packedLengthValue = (b->packed) - 2;

  *lengthValue = TWO_BYTE_INT(packedLengthValue);
/*
// Setup ccc = 98, sss = 000, nnn = number
*/
  localDefinitionNumber = ONE_BYTE_INT((b->packed));
  localDefinitionNumber += 98000000;

  local = createLocalDefinition(a,localDefinitionNumber);
  local = local->next;

  while( local ) {
    memmove(&next,local,sizeof(action));
    next.next = NULL;
    decode(&next,b);
    local = local->next;
  }

  return NULL;
}


/*==========================================================*/

static op_code codes[] = {
  { "IF_EQ",   init,        pack_if,     unpack_if,     test_eq },
  { "IF_NEQ",  init,        pack_if,     unpack_if,     test_ne},
  { "IF_GT",   init,        pack_if,     unpack_if,     test_gt},
  { "ENDIF",   initENDIF,   packENDIF,   unpackENDIF, },
  { "F1",      init,        packF1,      unpackF1, },
  { "SP_TO",   init,        packSP_TO,   unpackSP_TO, },
  { "PAD",     init,        packPAD,     unpackPAD, },
  { "PADTO",   init,        packPADTO,   unpackPADTO, },
  { "PADMULT", init,        packPADMULT, unpackPADMULT, },
  { "PADFROM", init,        packPADFROM, unpackPADFROM, },
  { "BYTES",   init,        packBYTES,   unpackBYTES, },
  { "D3",      init,        packD3,      unpackD3, },
  { "S1",      init,        packS1,      unpackS1, },
  { "S2",      init,        packS2,      unpackS2, },
  { "S3",      init,        packS3,      unpackS3, },
  { "S4",      init,        packS4,      unpackS4, },
  { "L3",      init,        packL3,      unpackL3, },
  { "I1",      init,        packI1,      unpackI1, },
  { "I2",      init,        packI2,      unpackI2, },
  { "I3",      init,        packI3,      unpackI3, },
  { "I4",      init,        packI4,      unpackI4, },
  { "A4",      init,        packA4,      unpackA4, },
  { "A8",      init,        packA8,      unpackA8, },
  { "A1",      init,        packA1,      unpackA1, },
  { "LOCAL",   init,        packLOCAL,   unpackLOCAL, },
  { "LP_I1",   init,        packLP_I1,   unpackLP_I1, },
  { "LP_I2",   init,        packLP_I2,   unpackLP_I2, },
  { "LP_I3",   init,        packLP_I3,   unpackLP_I3, },
  { "LP_I4",   init,        packLP_I4,   unpackLP_I4, },
  { "LP_I4M1", init,        packLP_I4M1, unpackLP_I4M1, },
  { "LP_S1",   init,        packLP_S1,   unpackLP_S1, },
  { "LP_S2",   init,        packLP_S2,   unpackLP_S2, },
  { "LP_S3",   init,        packLP_S3,   unpackLP_S3, },
  { "LP_S4",   init,        packLP_S4,   unpackLP_S4, },
  { "LIST",    init,        packLIST,    unpackLIST, },
  { "ENDLIST", initENDLIST, packENDLIST, unpackENDLIST, },
};

action* createLocalDefinitionHandler(grib_int_t definitionNumber) {
/*
// Creates a handler from a text description of a local definition.
//
// The file name is localDefinitionTemplate_mmm_nnn for definition nnn
// of a centre with WMO identifier mmm (eg ECMWF = 098).
// The directory containing the text definition can be specified
// by environment variable LOCAL_DEFINITION_TEMPLATES.
*/
static char * fullPathName;
FILE* file;
char line[1024];
grib_int_t i;
action* first = 0;
action* last  = 0;
int useInternalDefinition = 0, number;
char* definition = NULL;
char* next = NULL;

  fullPathName = findLocalDefinitionFile(definitionNumber);
  if( fullPathName == NULL) {
    useInternalDefinition = 1;
    number = definitionNumber % 1000;

    switch( (int) number ) {

      default:
        {
          int DefinitionNumber = (int) definitionNumber;
          int centre    = DefinitionNumber/1000000;
          int subcentre = (DefinitionNumber/1000)%1000;
          int number    = DefinitionNumber%1000;

          printf("createLocalDefinitionHandler: no local definition found for:\n");

          printf("createLocalDefinitionHandler: - centre     %d\n",centre);
          printf("createLocalDefinitionHandler: - subcentre  %d\n",subcentre);
          printf("createLocalDefinitionHandler: - definition %d\n",number);
          return NULL;
        }
    }
  }

  if( ! useInternalDefinition ) {
    file = fopen(fullPathName,"r");
    if( file == NULL) {
      perror("createLocalDefinitionHandler: Error opening template file");
      return NULL;
    }
  }

  do
  {
    char description[40];
    char octet[40];
    char code[40];
    char ksec1[40];
    char count[40];
    action* a = 0;


      if( useInternalDefinition ) {
        int count;

        next = strchr(definition,'\n');
        if( next == NULL )
          strcpy(line,definition);
        else {
          count = next - definition;
          memcpy(line,definition,count);
          line[count] = '\0';
          definition += (count+1);
        }
      }
      else {
        next = fgets(line,sizeof(line)-1,file);
        if( next == NULL ) continue;
      }

      if( line[0] == '!') continue;

      sscanf(line,"%s %s %s %s %s",description,octet,code,ksec1,count);

      for(i = 0; i < NUMBER(codes); i++ ) {

        if( EQUAL(code,codes[i].description) ) {

          a = (action*)malloc(sizeof(action));
          if( a == NULL ) {
            fprintf(stderr,"createLocalDefinitionHandler: out of memory");
            exit(1);
          }

          codes[i].init(first,a,description,octet,code,ksec1,count);
          a->pack   = codes[i].pack;
          a->unpack = codes[i].unpack;
          a->test   = codes[i].test;
          a->sum    = 0;
          init_reference(first,a,description,octet,ksec1,count);

          if(!first)
            first = last = a;
          else
            last->next = a;

          last = a;
          break;

        }
      }

      if(!a) {
        fprintf(stderr,"createLocalDefinitionHandler: Invalid opcode(%s)",code);
        return NULL;
      }

  } while( next != NULL );
  if(file) fclose(file);
  return first;

}

void encode(action* a,buffer* b)
{
  while(a) {
    a = a->pack(a,b);
  }
  return;
}

int encodeLocalDefinitionValues(
  action* a,
  grib_int_t* values,
  unsigned char* packed,
  grib_int_t * numberOfValues,
  grib_int_t * numberOfBytes) {
/*
// Builds packed bytes in 'buffer' from integer 'values'.
//
// Returns the number of bytes packed in buffer.
*/
buffer b;

  b.totalPackedBytes = 0;
  b.totalUnPackedBytes = 0;
  b.values = values;
  b.startKsec1 = values;
  b.packed = packed;
  b.startOctets  = packed;
  encode(a,&b);

  *numberOfValues = (b.values - b.startKsec1);
  *numberOfBytes  = b.totalPackedBytes;
  return 0;
}

void decode(action* a,buffer* b)
{
  while(a) {
    a = a->unpack(a,b);
  }
  return;
}

int decodeLocalDefinitionValues(
  action* a,
  unsigned char* packed,
  grib_int_t* values,
  grib_int_t * numberOfValues,
  grib_int_t * numberOfBytes) {
/*
// Builds integer 'values' from packed bytes in 'buffer'.
//
// Returns the number of values unpacked.
*/
buffer b;

  b.totalUnPackedValues = 0;
  b.totalUnPackedBytes = 0;
  b.values = values;
  b.startKsec1 = values;
  b.packed = packed;
  b.startOctets  = packed;
  decode(a,&b);

  *numberOfValues = b.totalUnPackedValues;
  *numberOfBytes  = b.totalUnPackedBytes;
  return 0;
}

action* createLocalDefinition(action* actionSoFar,grib_int_t definitionNumber) {
/*
// Creates a handler from a text description of a local definition.
//
// The file name is localDefinitionTemplate_mmm_nnn for definition nnn
// of a centre with WMO identifier mmm (eg ECMWF = 098).
// The directory containing the text definition can be specified
// by environment variable LOCAL_DEFINITION_TEMPLATES.
*/
static char * fullPathName;
FILE* file;
char line[1024];
grib_int_t i;
action* latest  = actionSoFar;
int useInternalDefinition = 0, number;
char* definition = NULL;
char* next = NULL;

  fullPathName = findLocalDefinitionFile(definitionNumber);
  if( fullPathName == NULL) {
    useInternalDefinition = 1;
    number = definitionNumber % 1000;

    switch( (int) number ) {

      default:
        {
          int DefinitionNumber = (int) definitionNumber;
          int centre    = DefinitionNumber/1000000;
          int subcentre = (DefinitionNumber/1000)%1000;
          int number    = DefinitionNumber%1000;

          printf("createLocalDefinition: no local definition found for:\n");

          printf("createLocalDefinition: - centre     %d\n",centre);
          printf("createLocalDefinition: - subcentre  %d\n",subcentre);
          printf("createLocalDefinition: - definition %d\n",number);
          exit(1);
        }
    }
  }

  if( ! useInternalDefinition ) {
    file = fopen(fullPathName,"r");
    if( file == NULL) {
      perror("createLocalDefinition: Error opening template file");
      return NULL;
    }
  }

  do
  {
    char description[40];
    char octet[40];
    char code[40];
    char ksec1[40];
    char count[40];
    action* a = 0;


      if( useInternalDefinition ) {
        int count;

        next = strchr(definition,'\n');
        if( next == NULL )
          strcpy(line,definition);
        else {
          count = next - definition;
          memcpy(line,definition,count);
          line[count] = '\0';
          definition += (count+1);
        }
      }
      else {
        next = fgets(line,sizeof(line)-1,file);
        if( next == NULL ) continue;
      }

      if( line[0] == '!') continue;

      sscanf(line,"%s %s %s %s %s",description,octet,code,ksec1,count);

      for(i = 0; i < NUMBER(codes); i++ ) {

        if( EQUAL(code,codes[i].description) ) {

          a = (action*)malloc(sizeof(action));
          if( a == NULL ) {
            fprintf(stderr,"createLocalDefinition: out of memory");
            exit(1);
          }
          if( NEQUAL(octet,"n/a") ) strcpy(octet,"-");
          if( NEQUAL(ksec1,"n/a") ) strcpy(ksec1,"-");
          codes[i].init(actionSoFar,a,description,octet,code,ksec1,count);
          a->pack   = codes[i].pack;
          a->unpack = codes[i].unpack;
          a->test   = codes[i].test;
          a->sum    = 0;
          init_reference(actionSoFar,a,description,octet,ksec1,count);

          latest->next = a;
          latest = a;

          break;

        }
      }

      if(!a) {
        fprintf(stderr,"createLocalDefinition: Invalid opcode(%s)",code);
        return NULL;
      }

  } while( next != NULL );
  if(file) fclose(file);
  return actionSoFar;

}
