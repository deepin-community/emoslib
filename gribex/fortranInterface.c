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
#include "common/fortint.h"
#include "handleLocalDefinitions.h"
#include "grib_int_t.h"

grib_int_t stringIsNotANumber(const char * count);

typedef struct knownActions {
    int count;
    int * numbers;
    action ** actions;
} knownActions;

knownActions known = {0,NULL,NULL};

int definitionIsDefined(struct knownActions * known, fortint number) {
    int loop;
    grib_int_t Number = (grib_int_t) number;
    
    for( loop = 0; loop < known->count; loop++ ) {
        if( known->numbers[loop] == (int)number ) return loop;
    }
    
    known->count++;
    
    known->numbers = (int*) realloc(known->numbers,known->count*sizeof(int));
    if( known->numbers == NULL ) return -1;
    
    known->actions = (action**) realloc(known->actions,
                                        known->count*sizeof(action*));
    if( known->actions == NULL ) return -1;
    
    known->numbers[known->count-1] = (int) number;
    known->actions[known->count-1] = createLocalDefinitionHandler(Number);
    if( known->actions[known->count-1] == NULL ) return -1;
    
    return (known->count-1);
}

void decodeLocalDefinition(
        fortint localDefinitionNumber,
        fortint* packedValues,
        fortint* unpackedValues,
        fortint* numberOfUnpackedIntegers,
        fortint* numberOfPackedBytes) {
    int listPosition, loop;
    unsigned char* packedValuesBytes = (unsigned char*) packedValues;
    grib_int_t UnpackedValues[1024];
    grib_int_t NumberOfUnpackedIntegers;
    grib_int_t NumberOfPackedBytes;
    
    listPosition = definitionIsDefined(&known,localDefinitionNumber);
    if( listPosition == -1 ) {
        *numberOfPackedBytes = 0;
        return;
    }
    
    decodeLocalDefinitionValues(known.actions[listPosition],
                                packedValuesBytes,UnpackedValues,
                                &NumberOfUnpackedIntegers,
                                &NumberOfPackedBytes);
    *numberOfUnpackedIntegers = NumberOfUnpackedIntegers;
    *numberOfPackedBytes = NumberOfPackedBytes;
    for( loop = 0; loop < NumberOfUnpackedIntegers; loop++ )
        *(unpackedValues+loop) = (fortint) UnpackedValues[loop];
    
    return;
}

void dldefs(
        fortint* localDefinitionNumber,
        fortint* packedValues,
        fortint* unpackedValues,
        fortint* numberOfUnpackedIntegers,
        fortint* numberOfPackedBytes) {
    
    decodeLocalDefinition(*localDefinitionNumber,packedValues,unpackedValues,
                          numberOfUnpackedIntegers,numberOfPackedBytes);
    return;
}

void dldefs_(
        fortint* localDefinitionNumber,
        fortint* packedValues,
        fortint* unpackedValues,
        fortint* numberOfUnpackedIntegers,
        fortint* numberOfPackedBytes) {
    
    decodeLocalDefinition(*localDefinitionNumber,packedValues,unpackedValues,
                          numberOfUnpackedIntegers,numberOfPackedBytes);
    return;
}

void encodeLocalDefinition(
        fortint localDefinitionNumber,
        fortint* unpackedValues,
        fortint* packedValues,
        fortint* numberOfUnpackedIntegers,
        fortint* numberOfPackedBytes) {
    int listPosition, loop;
    unsigned char* packedValuesBytes = (unsigned char*) packedValues;
    grib_int_t UnpackedValues[1024];
    grib_int_t NumberOfUnpackedIntegers;
    grib_int_t NumberOfPackedBytes;
    
    listPosition = definitionIsDefined(&known,localDefinitionNumber);
    if( listPosition == -1 ) {
        *numberOfPackedBytes = 0;
        return;
    }
    
    for( loop = 0; loop < 1024; loop++ )
        UnpackedValues[loop] = (grib_int_t) *(unpackedValues+loop);
    
    encodeLocalDefinitionValues(known.actions[listPosition],
                                UnpackedValues,packedValuesBytes,
                                &NumberOfUnpackedIntegers,
                                &NumberOfPackedBytes);
    *numberOfUnpackedIntegers = NumberOfUnpackedIntegers;
    *numberOfPackedBytes = NumberOfPackedBytes;
    
    return;
}

fortint eldefs(
        fortint* localDefinitionNumber,
        fortint* unpackedValues,
        fortint* packedValues,
        fortint* numberOfUnpackedIntegers,
        fortint* numberOfPackedBytes) {
    
    encodeLocalDefinition(*localDefinitionNumber,unpackedValues,packedValues,
                          numberOfUnpackedIntegers,numberOfPackedBytes);
    return 0;
}

fortint eldefs_(
        fortint* localDefinitionNumber,
        fortint* unpackedValues,
        fortint* packedValues,
        fortint* numberOfUnpackedIntegers,
        fortint* numberOfPackedBytes) {
    
    encodeLocalDefinition(*localDefinitionNumber,unpackedValues,packedValues,
                          numberOfUnpackedIntegers,numberOfPackedBytes);
    return 0;
}

void insertSection1Length(fortint* section1, fortint* length) {
    unsigned char* psection1 = (unsigned char*) section1;
    unsigned char section1Length[3] = {'\0','\0','\0'};
    fortint Length = *length;
    
    section1Length[0] = (Length & 0xff0000) >> 16;
    section1Length[1] = (Length & 0xff00) >> 8;
    section1Length[2] = (Length & 0xff);
    memcpy((psection1+8),section1Length,3);
    return;
}

void isec1l_(fortint* section1, fortint* length) {
    insertSection1Length(section1,length);
    return;
}

void isec1l(fortint* section1, fortint* length) {
    insertSection1Length(section1,length);
    return;
}

fortint ldefnum(fortint* centre, fortint* subcentre, fortint* number) {
    int shift = (sizeof(fortint)-1)*8;
#ifdef LITTLE_ENDIAN
    return ((*centre*1000000) + (*subcentre*1000) + (*number & 0xff));
#else
    return ((*centre*1000000) + (*subcentre*1000) + ((*number>>shift) & 0xff));
#endif
}

fortint ldefnum_(fortint* centre, fortint* subcentre, fortint* number) {
    return ldefnum(centre,subcentre,number);
}

void loadPrintLine(
        char* printLine,
        int printLineLength,
        char* contents,
        char* code,
        fortint* ksec1Value) {
    int loop, printLength;
    char value[9];
    char p;
    fortint number;
    
    for( loop = 0; loop < printLineLength; loop++) printLine[loop] = ' ';
    printLength = strlen(contents);
    if( printLength > (printLineLength-10) ) printLength = (printLineLength-10);
    strncpy((printLine+1),contents,printLength);
    
    if( EQUAL(code,"A4") ) {
        memcpy(value,ksec1Value,4);
        value[4] = '\0';
        sprintf((printLine+37),"      %s",value);
    }
    else if( EQUAL(code,"A8") ) {
        memcpy(value,ksec1Value,8);
        value[8] = '\0';
        sprintf((printLine+37),"      %s",value);
    } else if( EQUAL(code,"BYTES") ){
        sprintf(value,"%0x",*ksec1Value /*,8*/ );
        if( strlen(value) < 8 ) {
            int missing = 8 - strlen(value);
            for( loop = 7; loop >= missing; loop-- ) value[loop] = value[loop-missing];
            for( loop = 0; loop < missing; loop++ ) value[loop] = '0';
        }
#ifdef LITTLE_ENDIAN
        p = value[0];
        value[0] = value[6];
        value[6] = p;
        p = value[1];
        value[1] = value[7];
        value[7] = p;
        p = value[2];
        value[2] = value[4];
        value[4] = p;
        p = value[3];
        value[3] = value[5];
        value[5] = p;
#endif
        value[8] = '\0';
        sprintf((printLine+37),"  %s",value);
    }
    
    else
        sprintf((printLine+37)," %9d",*ksec1Value);
    
    return;
}

action* createLocalDefinition(action*,grib_int_t);

void displayUnpackedSection1Values(
        fortint fileNumber,
        fortint* unpackedSection1Values) {
    FILE * out;
    char fileName[] = "fort.nn";
    fortint localDefinitionNumber, listPosition;
    action* a;
    int Local = 0;
    int valueToPrint = 41;
    int expverFound = 0;
    int printLength, loop, printableValue = 0, numberOfPrintLines;
    int loopCount, listCount, padCount, byteCount;
    int localDefinitionLengthOffset;
    grib_int_t first = 1, firstLocal = 1;
    char* loopName, *byteName;
    grib_int_t previousDefinitionLengthOffset = 0;
    grib_int_t previousDefinitionLength = 0;
#define LINELENGTH 46
    char printLine[LINELENGTH] = "                                        ";
    
    if( (fileNumber < 1) || (fileNumber > 99) ) return;
    
    if( fileNumber == 6 ) {
        out = stdout;
        setbuf(out,NULL);
    }
    else {
        if( fileNumber < 10 )
            sprintf((fileName+5),"%1d",fileNumber);
        else
            sprintf((fileName+5),"%2d",fileNumber);
        out = fopen(fileName,"w");
    }
    if( out == NULL ) return;
    
    localDefinitionNumber = unpackedSection1Values[1]*1000000 +
            unpackedSection1Values[21]*1000 +
            unpackedSection1Values[36];
    listPosition = definitionIsDefined(&known,localDefinitionNumber);
    if( listPosition == -1 )
        return;
    else
        a = known.actions[listPosition];
    
    while(a) {
        if( expverFound ) {
            /*
//    Abandon printing on following:
*/
            if( EQUAL((a->code),"IF_EQ") ||
                    EQUAL((a->code),"IF_NEQ") ||
                    EQUAL((a->code),"ENDIF") ||
                    EQUAL((a->code),"PADTO") ||
                    EQUAL((a->code),"SP_TO") ||
                    EQUAL((a->code),"PADMULT") ) return;
            /*
//    Avoid straightforward printing of following:
*/
            printableValue = ( NEQUAL((a->ksec1),"n/a")    &&
                               NEQUAL((a->code),"PAD")  &&
                               NEQUAL((a->code),"LP_I") &&
                               NEQUAL((a->code),"LIST")    &&
                               NEQUAL((a->code),"BYTES")   &&
                               NEQUAL((a->code),"F1") );
            if( printableValue ) {
                loadPrintLine(printLine,LINELENGTH,
                              (char*)(a->description),(char*)(a->code),
                              &unpackedSection1Values[valueToPrint]);
                fprintf(out,"%s\n",printLine);
            }
            /*
//    Print loops:
*/
            if( EQUAL((a->code),"LP_I") ){
                loopCount = a->reference->value;
                loopName = strdup(a->description);
                for( loop = 0; loop < loopCount; loop++ ) {
                    loadPrintLine(printLine,LINELENGTH,
                                  loopName,(char*)(a->code),
                                  &unpackedSection1Values[valueToPrint]);
                    fprintf(out,"%s\n",printLine);
                    valueToPrint++;
                }
                free(loopName);
            }
            /*
//    Print bytes:
*/
            if( EQUAL((a->code),"BYTES") ) {
                byteCount = a->reference->value;
                byteName = strdup(a->description);
                for( loop = 0; loop < byteCount/4; loop++ ) {
                    loadPrintLine(printLine,LINELENGTH,
                                  byteName,"BYTES",
                                  &unpackedSection1Values[valueToPrint]);
                    fprintf(out,"%s\n",printLine);
                    valueToPrint++;
                }
                free(byteName);
            }
            /*
//    Skip padding and only move the count if ksec1 is affected:
*/
            if( EQUAL((a->code),"PAD") ) {
                if( (! stringIsNotANumber(a->count)) && NEQUAL((a->ksec1),"n/a") ) {
                    padCount = atoi(a->count);
                    valueToPrint += padCount;
                }
            }
            /*
//    Print lists:
*/
            if( EQUAL((a->code),"LIST") ) {
                char** listName = NULL;
                char** listCode = NULL;
                action* listPointer = a->next;
                int separateItems = 0, nextItem = 0;
                int inLocal = 0;
                
                listCount = a->reference->value;
                listName = (char**) malloc(sizeof(char*));
                
                while( NEQUAL((listPointer->code),"ENDLIST") ) {
                    static grib_int_t localCount = 0;
                    if( NEQUAL(listPointer->code,"LOCAL") &&
                            (!inLocal) &&
                            (localCount <= 1) ) {
                        separateItems++;
                        listName = (char**) realloc(listName,separateItems*sizeof(char*));
                        listName[separateItems-1] = strdup(listPointer->description);
                        listCode = (char**) realloc(listCode,separateItems*sizeof(char*));
                        listCode[separateItems-1] = strdup(listPointer->code);
                        listPointer = listPointer->next;
                        localDefinitionLengthOffset = separateItems - 1;
                    }
                    else {
                        /*
//          Special handling for definition 192 (LOCAL within a LIST):
//
//   numberOfLocalDefinitions   52      I1      44      -
//   listOfLocalDefinitions     -       LIST    -       numberOfLocalDefinitions
//   localDefinitionLength      -       I2      -       -
//   localDefinition            -       LOCAL   -       -
//   endListOfLocalDefinitions  -       ENDLIST -       listOfLocalDefinitions
*/
                        action* local, * a;
                        action next;
                        grib_int_t lengthOffset;
                        static grib_int_t length;
                        grib_int_t localDefinitionNumber;
                        
                        inLocal = 1;
                        
                        if( first ) {
                            first = 0;
                            lengthOffset = valueToPrint;
                            localCount = listCount;
                        }
                        else {
                            lengthOffset =
                                    previousDefinitionLengthOffset + previousDefinitionLength + 1;
                        }
                        length = unpackedSection1Values[lengthOffset];
                        previousDefinitionLengthOffset = lengthOffset;
                        previousDefinitionLength = length;
                        
                        if( !firstLocal ) {
                            separateItems++;
                            listName = (char**) realloc(listName,separateItems*sizeof(char*));
                            listName[separateItems-1] =
                                    strdup(listName[localDefinitionLengthOffset]);
                            listCode = (char**) realloc(listCode,separateItems*sizeof(char*));
                            listCode[separateItems-1] =
                                    strdup(listCode[localDefinitionLengthOffset]);
                        }
                        else
                            firstLocal = 0;
                        /*
//          Setup ccc = 98, sss = 000, nnn = number
*/
                        localDefinitionNumber = 98000000 +
                                unpackedSection1Values[lengthOffset+1];
                        
                        a = listPointer;
                        Local = 1;
                        local = createLocalDefinition(a,localDefinitionNumber);
                        local = local->next;
                        
                        while( local ) {
                            if( NEQUAL(local->ksec1,"n/a") ) {
                                separateItems++;
                                listName = (char**) realloc(listName,separateItems*sizeof(char*));
                                listName[separateItems-1] = strdup(local->description);
                                listCode = (char**) realloc(listCode,separateItems*sizeof(char*));
                                listCode[separateItems-1] = strdup(local->code);
                            }
                            else {
                                /*
//              previousDefinitionLengthOffset++;
*/
                            }
                            local = local->next;
                        }
                        localCount--;
                        if( localCount == 0 ) {
                            inLocal = 0;
                            break;
                        }
                    }
                }
                
                if( Local )
                    numberOfPrintLines = separateItems;
                else
                    numberOfPrintLines = listCount * separateItems;
                
                nextItem = 0;
                for( loop = 0; loop < numberOfPrintLines; loop++ ) {
                    loadPrintLine(printLine,LINELENGTH,
                                  listName[nextItem],listCode[nextItem],
                                  &unpackedSection1Values[valueToPrint]);
                    fprintf(out,"%s\n",printLine);
                    valueToPrint++;
                    if(EQUAL(listCode[nextItem],"A8"))
                        valueToPrint++;
                    nextItem++;
                    if( nextItem == separateItems ) nextItem = 0;
                }
                for( loop = 0; loop < separateItems; loop++ ) {
                    free(listName[loop]);
                    free(listCode[loop]);
                }
                free(listName);
                free(listCode);
                /*
//      Skip to ENDLIST (which will not be displayed)
//      Return if a LIST of LOCAL definitions (definition 192).
*/
                a = listPointer;
                if( Local ) {
                    first = 1;
                    firstLocal = 1;
                    /*
          previousDefinitionLengthOffset = 0;
          previousDefinitionLength = 0;
*/
                    Local = 0;
                    if( fileNumber != 6 ) fclose(out);
                    return;
                }
            }
        }
        
        if( EQUAL("experimentVersionNumber",a->description) ) expverFound = 1;
        if( printableValue ) valueToPrint++;
        if( printableValue && EQUAL((a->code),"A8") ) valueToPrint++;
        a = a->next;
    }
    
    if( fileNumber != 6 ) fclose(out);
    
    return;
}

void ldefprt(fortint* fileNumber, fortint* unpackedSection1Values) {
    displayUnpackedSection1Values(*fileNumber,unpackedSection1Values);
    return;
}

void ldefprt_(fortint* fileNumber, fortint* unpackedSection1Values) {
    displayUnpackedSection1Values(*fileNumber,unpackedSection1Values);
    return;
}
