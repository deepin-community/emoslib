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

#include "bitmap.h"
#include "common/fortint.h"

#define BUFFLEN 50000
#define NOTSET -1

fortint MAKEMAP(
  char * filename,
  fortint *n,
  fortint *m,
  char ** mybitmap,
  fortint filenameLength) {
/*
// Builds a bitmap in memory from the definition in a file.
//
// Called from FORTRAN:
//
// STATUS = MAKEMAP(FILENAME,N,M,BITMAP)
//
// where:
//
// FILENAME is a FORTRAN CHARACTER variable containing the file name.
//
// Returns:
//
// N is the number of rows in the bitmap.
// M is the number of columns in the bitmap.
// BITMAP is the binary map expanded in memory.
//
// STATUS = 0 if the processing is successful.
//
*/
FILE * in;
char * bitmap;
char buffer[BUFFLEN];
int rowCount, columnCount;
int start, finish, defaultValue, size, i;
int firstColumn, lastColumn, rowNumber, columnNumber;
char defaultValueSetting, delimiter = '\1', previousDelimiter = '\0';
static int oldRow = NOTSET;
char * fileName, *p;

/*
// Open and read the file containing the bitmap definition.
*/
  fileName = (char *) malloc(filenameLength+1);
  if( fileName == NULL ) {
    perror("MAKEMAP: malloc error (fileName)");
    exit(1);
  }
  strncpy(fileName,filename,filenameLength);
  fileName[filenameLength] = '\0';
  p = fileName + filenameLength - 1;
  while ( *p == ' ' ) *(p--) = '\0';

  in = fopen(fileName,"r");
  if( in == NULL ) {
    printf("MAKEMAP: Problem opening bitmap file: %s\n",fileName);
    perror("MAKEMAP: File open error");
    return 1;
  }
  free(fileName);

  fread(buffer, 1, BUFFLEN, in);
  if( ! feof(in) ) {
    printf("MAKEMAP: Internal buffer size too small = %d bytes\n", BUFFLEN);
    return 2;
  }

/*
// Check for "SPEC,SIZE="
*/
  if( strncmp(buffer,"SPEC",4) != 0 ) {
    printf("MAKEMAP: Specification does not start with SPEC\n");
    return 3;
  }

  start = 5;

  if( strncmp((buffer+start),"SIZE=",5) != 0 ) {
    printf("MAKEMAP: In specification, SPEC not followed by SIZE=\n");
    return 4;
  }

/*
// Get count of rows and columns
*/
  start = 1 + findCharacter(buffer,BUFFLEN,start,'=');
  if( start == 0 ) {
    printf("MAKEMAP: '=' missing in SIZE specification\n");
    return 5;
  }

  finish = findCharacter(buffer,BUFFLEN,start,':');
  if( finish == 0 ) {
    printf("MAKEMAP: ':' missing in SIZE specification\n");
    return 6;
  }
  rowCount = findNumber(buffer,BUFFLEN,start,&finish,':');
  *n = rowCount;

  start = 1 + finish;
  finish = findCharacter(buffer,BUFFLEN,start,',');
  if( finish == 0 ) {
    printf("MAKEMAP: ',' missing after SIZE specification\n");
    return 7;
  }
  columnCount = findNumber(buffer,BUFFLEN,start,&finish,',');
  *m = columnCount;

/*
// Find default VALUE (OFF/ON)
*/
  start = 1 + finish;
  if( strncmp((buffer+start),"VALUES=",7) != 0 ) {
    printf("MAKEMAP: In specification, SIZE not followed by VALUES=\n");
    return 8;
  }
  start += 7;
  if( strncmp((buffer+start),"ON",2) == 0 ) {
    defaultValueSetting = 0xff;
    defaultValue = 1;
  }
  else {
    defaultValueSetting = 0;
    defaultValue = 0;
  }

/*
// Claim memory for bitmap and fill it with default setting
*/
   size = (rowCount*columnCount+7)/8;
   bitmap = (char *) malloc(size);
   if( bitmap == NULL ) {
     perror("MAKEMAP: malloc error (bitmap)");
     exit(1);
   }
   *mybitmap = bitmap;
   for( i = 0; i < size; i++ )
     bitmap[i] = defaultValueSetting;

/*
// Fill in the bits
*/
  start = findCharacter(buffer,BUFFLEN,start,'P');
  if( start == 0 ) {
    printf("MAKEMAP: '=' POINTS missing in specification\n");
    return 9;
  }
  if( strncmp((buffer+start),"POINTS=",7) != 0 ) {
    printf("MAKEMAP: In specification, VALUES= not followed by POINTS=\n");
    return 10;
  }

  finish = start + 6;

/*
// Work through POINTS specification
*/

  do {
    previousDelimiter = delimiter;
    start = 1 + finish;
    delimiter = findDelimiter(buffer,BUFFLEN,start);
    if( delimiter == 0 ) {
      fclose(in);
      return 0;
    }

    switch( delimiter ) {

      case '-':                              /* eg 03-08 */
        firstColumn = findNumber(buffer,BUFFLEN,start,&finish,delimiter);
        start = 1 + finish;
        delimiter = findDelimiter(buffer,BUFFLEN,start);
        lastColumn = findNumber(buffer,BUFFLEN,start,&finish,delimiter);
        for(columnNumber=firstColumn; columnNumber<=lastColumn; columnNumber++)
          setBit(bitmap,defaultValue,columnCount,rowNumber,columnNumber);
        break;

      case '/':                              /* eg 03/04 */
        columnNumber = findNumber(buffer,BUFFLEN,start,&finish,delimiter);
        setBit(bitmap,defaultValue,columnCount,rowNumber,columnNumber);
        break;

      case '\n':                             /* eg 03\n */
        if( previousDelimiter == ',' ) {
          finish++;
          break;
        }
        if( previousDelimiter == '\n' ) {
          if( (oldRow < rowCount) && (oldRow != NOTSET) ) {
            int loop;

            for( loop = (oldRow+1); loop <= rowCount; loop++ )
              copyRow(bitmap, columnCount, oldRow, loop, defaultValue);
          }
          oldRow = rowCount;
          fclose(in);
          return 0;
        }

      case ',':                              /* eg 03,  */
        columnNumber = findNumber(buffer,BUFFLEN,start,&finish,delimiter);
        if( columnNumber != 0 )
          setBit(bitmap,defaultValue,columnCount,rowNumber,columnNumber);
        break;

      case ':':                              /* eg 01:  */
        rowNumber = findNumber(buffer,BUFFLEN,start,&finish,delimiter);
        if( (oldRow < rowNumber) && (oldRow != NOTSET) ) {
          int loop;

          for( loop = (oldRow+1); loop < rowNumber; loop++ )
            copyRow(bitmap, columnCount, oldRow, loop, defaultValue);
        }
        oldRow = rowNumber;
        break;

      default:
        break;

    }

  } while( 1 );

}

int findNumber(char * buffer,int length,int start,int * finish,char delimiter) {
char number[20];
int end;

  end = findCharacter(buffer,length,start,delimiter);
  *finish = end;
  strncpy(number,(buffer+start),(end-start));
  number[(end-start)] = '\0';
  return atoi(number);

}

char findDelimiter(char * buffer, int length, int point) {
int i, j;
char delimiter[5] = {'-','/',',','\n',':'};

  for( i = point; i < length; i++ )
    for( j = 0; j < 5; j++ )
      if( buffer[i] == delimiter[j] ) return buffer[i];

  return (char) 0;
}

int findCharacter(char * buffer, int length, int point, char character) {
int i;

  for( i = point; i < length; i++ )
    if( buffer[i] == character ) return i;

  return 0;
}

void setBit(char * buffer, int value, int columnCount, int row, int column) {
int bitNumber, byte, bit;
char mask[8] = {0x80,0x40,0x20,0x10,0x08,0x04,0x02,0x01};

  bitNumber = (row - 1)*columnCount + (column - 1);
  byte = bitNumber / 8;
  bit  = bitNumber % 8;

  if( value == 1 )
    buffer[byte] &= (char) ~mask[bit];
  else
    buffer[byte] |= (char) mask[bit];

  return;
}

void copyRow(char * buffer, int columnCount, int oldRow, int newRow, int value) {
int oldByte, oldBit, newByte, newBit, bitNumber;
char oldValue, newValue, mask;
int loop;

  for( loop = 0; loop < columnCount; loop++ ) {
    bitNumber = (oldRow - 1)*columnCount + loop;
    oldByte = bitNumber / 8;
    oldBit  = bitNumber % 8;
    oldValue = (char) ((buffer[oldByte] >> (7-oldBit)) & 0x01);

    bitNumber = (newRow - 1)*columnCount + loop;
    newByte = bitNumber / 8;
    newBit  = bitNumber % 8;

    newValue = oldValue << (7-newBit);
    if( value == 1 ) {
      mask = 1 << (7-newBit);
      buffer[newByte] &= (char) ~mask;
      buffer[newByte] |= (char) newValue;
    }
    else
      buffer[newByte] |= newValue;
  }

}

