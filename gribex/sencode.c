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

#include "gdecode.h"
#include "sencode.h"

#define DEBUGOFF 1
#define DEBUG1 (debugSet > DEBUGOFF )
#define DEBUG2 (debugSet > (DEBUGOFF + 1) )
static char * debugLevel;
static int debugSet = 0;

#define ABS(a) ((a)<0?-(a):(a))
#define SMALL (1E-12)
#define PRACTICALLYZERO(a) (ABS((a))<(SMALL))
#define QUITESMALL (0.05)
#define ALMOSTZERO(a) (ABS((a))<(QUITESMALL))
#define TRUE    1
#define FALSE   0
#define CHECK   1
#define NOCHECK 0

#define NORTH   0
#define SOUTH   1
#define WEST    2
#define EAST    3
#define W_E_INC 4
#define N_S_INC 5
#define W_E_PTS 6
#define N_S_PTS 7

fortint initialiseNewGrib(gribProduct**);
fortint copyExistingGrib(gribProduct**,gribProduct**);
fortint getIntegerValue(gribProduct**,unsigned char*);
void adjustGridAreaDefinition(gribProduct*,int);

fortint SENCODE(gribProduct ** newGrib, gribProduct ** oldGrib) {
fortint status;
/*
// See if DEBUG switched on.
*/
    if( ! debugSet ) {
      debugLevel = getenv("GDECODE_DEBUG");
      if( debugLevel == NULL )
        debugSet = DEBUGOFF;              /* off */
      else {
        int loop;
        for( loop = 0; loop < strlen(debugLevel) ; loop++ ) {
          if( ! isdigit(debugLevel[loop]) ) {
            printf("Invalid number string in GDECODE_DEBUG: %s\n", debugLevel);
            printf("GDECODE_DEBUG must comprise only digits [0-9].\n");
            debugSet = DEBUGOFF;
          }
        }
        debugSet = DEBUGOFF + atol( debugLevel );
      }
      if( DEBUG1 ) printf("SENCODE: GDECODE_DEBUG switched on, level = %s\n",
                          debugLevel);
    }

  if( *oldGrib == NULL ) {
    status = initialiseNewGrib(newGrib);
    if( DEBUG1 ) {
      if( status )
        printf("SENCODE: initialiseNewGrib failed.\n");
      else
        printf("SENCODE: initialiseNewGrib ran OK\n");
    }
  }
  else {
    status = copyExistingGrib(newGrib,oldGrib);
    if( DEBUG1 ) {
      if( status )
        printf("SENCODE: copyExistingGrib failed.\n");
      else
        printf("SENCODE: copyExistingGrib ran OK\n");
    }
  }

  return status;

}

fortint initialiseNewGrib(gribProduct** newGrib) {

  if( DEBUG1 ) printf("initialiseNewGrib\n");

  *newGrib = (gribProduct *) allocateMemory(sizeof(gribProduct));
  (*newGrib)->g0 = (gribSection0 *) allocateMemory(sizeof(gribSection0));
  (*newGrib)->g1 = (gribSection1 *) allocateMemory(sizeof(gribSection1));
  (*newGrib)->g2 = (gribSection2 *) allocateMemory(sizeof(gribSection2));
  (*newGrib)->g3 = NULL;
  (*newGrib)->g4 = (gribSection4 *) allocateMemory(20);
  (*newGrib)->g5 = (gribSection5 *) allocateMemory(sizeof(gribSection5));
  memcpy((*((*newGrib)->g5)).end7777,"7777",4);

  (*newGrib)->currentPointIndex               = 0;
  (*newGrib)->numberOfValues                  = 0;
  (*newGrib)->value                           = NULL;
  (*newGrib)->bitStart                        = NULL;
  (*newGrib)->bitsPerValue                    = 0;
  (*newGrib)->bitmapped                       = 0;
  (*newGrib)->nextValueFirstBit               = 0;
  (*newGrib)->nextBit                         = 0;
  (*newGrib)->scale                           = (fortdouble) 0.0;
  (*newGrib)->minimum                         = (fortdouble) 0.0;
  (*newGrib)->missingValue                    = (fortdouble) 0.0;
  (*newGrib)->latitudeOffsets                 = NULL;
  (*newGrib)->expandedValues                  = NULL;
  (*newGrib)->currentPoint.latitude           = NULL;
  (*newGrib)->currentPoint.longitudeIncrement = NULL;
  (*newGrib)->currentPoint.gridPointValue     = NULL;
  (*newGrib)->northSet =
  (*newGrib)->southSet =
  (*newGrib)->westSet =
  (*newGrib)->eastSet =
  (*newGrib)->northSouthIncrementSet =
  (*newGrib)->westEastIncrementSet =
  (*newGrib)->northSouthNumberOfPointsSet =
  (*newGrib)->westEastNumberOfPointsSet = TRUE;

  return (fortint) 0;
}

fortint copyExistingGrib(gribProduct** newGrib, gribProduct** oldGrib) {
fortint lengthOfSectionToCopy;
gribProduct * oldG = *oldGrib;
gribProduct * newG;

  if( DEBUG1 ) printf("copyExistingGrib\n");

  *newGrib = (gribProduct *) allocateMemory(sizeof(gribProduct));
  newG = *newGrib;
/*
// Create section 0
*/
  newG->g0 = (gribSection0 *) allocateMemory(sizeof(gribSection0));
  memcpy((newG->g0),(oldG->g0),sizeof(gribSection0));
/*
// Copy section 1
*/
  lengthOfSectionToCopy = g1_length(oldG);
  newG->g1 = (gribSection1 *) allocateMemory(lengthOfSectionToCopy);
  memcpy((newG->g1),(oldG->g1),lengthOfSectionToCopy);
/*
// Copy section 2
*/
  lengthOfSectionToCopy = g2_length(oldG);
  newG->g2 = (gribSection2 *) allocateMemory(lengthOfSectionToCopy);
  memcpy((newG->g2),(oldG->g2),lengthOfSectionToCopy);

/*
  if( anyLatLonGrid(oldG) )  {
*/
  if( generalLatLonGrid(oldG) )  {
    newG->northSet =
    newG->southSet =
    newG->westSet =
    newG->eastSet =
    newG->northSouthIncrementSet =
    newG->westEastIncrementSet =
    newG->northSouthNumberOfPointsSet =
    newG->westEastNumberOfPointsSet = TRUE;
  }
/*
// Section 3 may or may not have to be created.
*/
  newG->g3 = NULL;
/*
// Copy standard part of section 4
*/
  lengthOfSectionToCopy = 20;
  newG->g4 = (gribSection4 *) allocateMemory(lengthOfSectionToCopy);
  memcpy((newG->g4),(oldG->g4),lengthOfSectionToCopy);
/*
// Create section 5.
*/
  newG->g5 = (gribSection5 *) allocateMemory(sizeof(gribSection5));
  memcpy((*(newG->g5)).end7777,"7777",4);


  newG->currentPointIndex = 0;
  newG->numberOfValues    = getIntegerValue(oldGrib,(unsigned char*)"numberOfFieldValues");
  newG->value             = NULL;
  newG->bitStart          = NULL;
  newG->bitsPerValue      = oldG->bitsPerValue;
  newG->bitmapped         = oldG->bitmapped;
  newG->nextValueFirstBit = 0;
  newG->nextBit           = 0;
  newG->scale             = (fortdouble) oldG->scale;
  newG->minimum           = (fortdouble) oldG->minimum;
  newG->missingValue      = (fortdouble) oldG->missingValue;
  newG->latitudeOffsets                 = NULL;
  newG->currentPoint.latitude           = NULL;
  newG->currentPoint.longitudeIncrement = NULL;
  newG->expandedValues                  = NULL;
  newG->currentPoint.gridPointValue     = NULL;

  return (fortint) 0;
}

fortint SENPACK(
  gribProduct** newGrib, unsigned char * buffer, fortint * bufferLength)
{
fortint totalLength;
gribProduct* g = *newGrib;
fortint lengthOfSectionToMove, next;

  totalLength = 8 + g1_length(g) + g2_length(g) + g4_length(g) + 4;
  if( primaryBitmapPresent(g) ) totalLength += g3_length(g);

  if( DEBUG1 ) printf("GRIB totalLength = %d bytes\n",totalLength);

  if( totalLength > *bufferLength ) {
    if( DEBUG1 ) printf("GRIB length (%d) greater than buffer length (%d)\n",
                         totalLength, *bufferLength );
    return (fortint) -1;
  }
  else {
    memcpy(buffer,(g->g0),sizeof(gribSection0));
    MOVE3BYTES((buffer+4),&totalLength);
    next = sizeof(gribSection0);

    lengthOfSectionToMove = g1_length(g);
    memcpy((buffer+next),(g->g1),lengthOfSectionToMove);
    next += g1_length(g);

    lengthOfSectionToMove = g2_length(g);
    memcpy((buffer+next),(g->g2),lengthOfSectionToMove);
    next += g2_length(g);

    if( primaryBitmapPresent(g) > 0 ) {
      lengthOfSectionToMove = g3_length(g);
      memcpy((buffer+next),(g->g3),lengthOfSectionToMove);
      next += g3_length(g);
    }

    lengthOfSectionToMove = g4_length(g);
    memcpy((buffer+next),(g->g4),lengthOfSectionToMove);
    next += g4_length(g);

    lengthOfSectionToMove = 4;
    memcpy((buffer+next),"7777",lengthOfSectionToMove);

    return (fortint) totalLength;
  }
}

fortint ISTIME(gribProduct ** grib, fortint * HHMM ) {
gribProduct * g = *grib;
fortint hhmm = (fortint) (*HHMM);
fortint hh = hhmm/100;
fortint mm = MOD(hhmm,100);

  if( DEBUG1 ) printf("ISTIME: HHMM = %d\n", *HHMM);

  MOVE1BYTE(((g->g1)->hour),&hh);
  MOVE1BYTE(((g)->g1->minute),&mm);

  return 0;
}

fortint RSTIME(gribProduct ** grib, double * HHMM ) {
fortint hhmm = (fortint) (*HHMM);

  if( DEBUG1 ) printf("RSTIME: HHMM = %f\n", *HHMM);

  return ISTIME(grib,&hhmm);
}

fortint ISDATE(gribProduct ** grib, fortint * YYYYMMDD ) {
gribProduct * g = *grib;
fortint yyyymmdd = *YYYYMMDD;
fortint yyyy, mmdd, year, month, day, century;

  if( DEBUG1 ) printf("ISDATE: YYYYMMDD = %d\n", yyyymmdd);

  yyyy = yyyymmdd/10000;
  mmdd = MOD(yyyymmdd,10000);
  year = MOD(yyyy,100);
  month = mmdd/100;
  day = MOD(mmdd,100);
  century = 1 + (yyyy/100);
  if( year == 0 ) century -= 1;

  MOVE1BYTE(((g->g1)->year),&year);
  MOVE1BYTE(((g)->g1->month),&month);
  MOVE1BYTE(((g)->g1->day),&day);
  MOVE1BYTE(((g)->g1->century),&century);

  return 0;
}

fortint RSDATE(gribProduct ** grib, double * YYYYMMDD ) {
fortint yyyymmdd = (fortint) *YYYYMMDD;

  if( DEBUG1 ) printf("RSDATE: YYYYMMDD = %f\n", *YYYYMMDD);

  return ISDATE(grib,&yyyymmdd);
}

fortint ISTABLE(gribProduct ** grib, fortint * table) {
gribProduct * g = *grib;

  if( DEBUG1 ) printf("ISTABLE: table = %d\n", *table);

  MOVE1BYTE(((g->g1)->tableVersionNumber),table);

  return 0;
}

fortint RSTABLE(gribProduct ** grib, fortdouble * table) {
gribProduct * g = *grib;
fortint Table = (fortint) *table;

  if( DEBUG1 ) printf("RSTABLE: table = %f\n", *table);

  return ISTABLE(grib,&Table);
}

fortint ISCENTR(gribProduct ** grib, fortint * centre) {
gribProduct * g = *grib;

  if( DEBUG1 ) printf("ISCENTR: centre = %d\n", *centre);

  MOVE1BYTE(((g->g1)->originatingCentre),centre);

  return 0;
}

fortint RSCENTR(gribProduct ** grib, fortdouble * centre) {
gribProduct * g = *grib;
fortint Centre = (fortint) *centre;

  if( DEBUG1 ) printf("RSCENTR: centre = %f\n", *centre);

  return ISCENTR(grib,&Centre);
}

fortint ISPARAM(gribProduct ** grib, fortint * parameter) {
gribProduct * g = *grib;

  if( DEBUG1 ) printf("ISPARAM: parameter = %d\n", *parameter);

  MOVE1BYTE(((g->g1)->parameter),parameter);

  return 0;
}

fortint RSPARAM(gribProduct ** grib, fortdouble * parameter) {
gribProduct * g = *grib;
fortint Parameter = (fortint) *parameter;

  if( DEBUG1 ) printf("RSPARAM: parameter = %f\n", *parameter);

  return ISPARAM(grib,&Parameter);
}

fortint ISLEVTY(gribProduct ** grib, fortint * typeOfLevel) {
gribProduct * g = *grib;
fortint TypeOfLevel = *typeOfLevel;
fortint zero =0;

  if( DEBUG1 ) printf("ISLEVTY: typeOfLevel = %d\n", TypeOfLevel);

  MOVE1BYTE(((g->g1)->typeOfLevel),typeOfLevel);

  if( (TypeOfLevel < 20) ||
      ((TypeOfLevel > 20) && (TypeOfLevel < 100)) ||
      (TypeOfLevel == 102) ||
      (TypeOfLevel == 118) ||
      ((TypeOfLevel > 121) && (TypeOfLevel < 125)) ||
      (TypeOfLevel == 126) ||
      ((TypeOfLevel > 128) && (TypeOfLevel < 141)) ||
      ((TypeOfLevel > 141) && (TypeOfLevel < 160)) ||
      ((TypeOfLevel > 160) && (TypeOfLevel < 200)) ||
      ((TypeOfLevel > 201) && (TypeOfLevel < 210)) ||
      (TypeOfLevel > 210) ) {
     MOVE2BYTES(((g->g1)->level1),&zero);
   }

  return 0;
}

fortint RSLEVTY(gribProduct ** grib, fortdouble * typeOfLevel) {
gribProduct * g = *grib;
fortint TypeOfLevel = (fortint) *typeOfLevel;

  if( DEBUG1 ) printf("RSLEVTY: typeOfLevel = %f\n", *typeOfLevel);

  return ISLEVTY(grib,&TypeOfLevel);
}

fortint ISLEVEL(gribProduct ** grib, fortint * level) {
gribProduct * g = *grib;
fortint zero = 0, bottomLayer, topLayer;

  if( DEBUG1 ) printf("ISLEVEL: level = %d\n", *level);

  switch( (int) g1_typeOfLevel(g) ) {

    case  20:
    case 100:
    case 103:
    case 105:
    case 107:
    case 109:
    case 111:
    case 113:
    case 115:
    case 117:
    case 119:
    case 125:
    case 127:
    case 160:
    case 210:
      if( DEBUG1 ) printf("ISLEVEL: two-byte level value\n");
      MOVE2BYTES(((g->g1)->level1),level);
      break;

    case 101:
    case 104:
    case 106:
    case 108:
    case 110:
    case 112:
    case 114:
    case 116:
    case 120:
    case 121:
    case 128:
    case 141:
      if( DEBUG1 ) printf("ISLEVEL: top and bottom level values\n");
      topLayer = (*level)/1000;
      MOVE1BYTE(((g->g1)->level1),&topLayer);
      bottomLayer = MOD((*level),1000);
      MOVE1BYTE(((g->g1)->level2),&bottomLayer);
      break;

    default:
      if( DEBUG1 ) printf("ISLEVEL: level value set to zero\n");
      MOVE2BYTES(((g->g1)->level1),&zero);
      break;
  }


  return 0;
}

fortint RSLEVEL(gribProduct ** grib, fortdouble * level) {
gribProduct * g = *grib;
fortint Level = (fortint) *level;

  if( DEBUG1 ) printf("RSLEVEL: level = %f\n", *level);

  return ISLEVEL(grib,&Level);
}

fortint ISTUNIT(gribProduct ** grib, fortint * timeUnit) {
gribProduct * g = *grib;

  if( DEBUG1 ) printf("ISTUNIT: timeUnit = %d\n", *timeUnit);

  switch( (int) *timeUnit ) {

  case 0:
  case 1:
  case 2:
  case 3:
  case 4:
  case 5:
  case 6:
  case 7:
  case 10:
  case 11:
  case 12:
  case 254:
    MOVE1BYTE(((g->g1)->unitOfTimeRange),timeUnit);
    break;
    return 0;

  default:
    if( DEBUG1 ) printf("ISTUNIT: invalid time unit\n");
  }
  return -1;
}

fortint RSTUNIT(gribProduct ** grib, fortdouble * timeUnit) {
gribProduct * g = *grib;
fortint TimeUnit = (fortint) *timeUnit;

  if( DEBUG1 ) printf("RSTUNIT: timeUnit = %f\n", *timeUnit);

  return ISTUNIT(grib,&TimeUnit);
}

fortint ISTRIND(gribProduct ** grib, fortint * timeRangeIndicator) {
gribProduct * g = *grib;

  if( DEBUG1 ) printf("ISTRIND: timeRangeIndicator = %d\n",*timeRangeIndicator);
  MOVE1BYTE(((g->g1)->timeRangeIndicator),timeRangeIndicator);

  return 0;
}

fortint RSTRIND(gribProduct ** grib, fortdouble * timeRangeIndicator) {
gribProduct * g = *grib;
fortint TimeRangeIndicator = (fortint) *timeRangeIndicator;

  if( DEBUG1 ) printf("RSTRIND: timeRangeIndicator = %f\n",*timeRangeIndicator);

  return ISTRIND(grib,&TimeRangeIndicator);
}

fortint ISSTEP(gribProduct ** grib, fortint * timeStep) {
gribProduct * g = *grib;
fortint zero = 0;

  if( DEBUG1 ) printf("ISSTEP: timeStep = %d\n",*timeStep);

  switch( (int) g1_timerange(g) ) {

    case 0:
      MOVE1BYTE(((g->g1)->P1),timeStep);
      MOVE1BYTE(((g->g1)->P2),&zero);
      return 0;

    case 1:
      MOVE1BYTE(((g->g1)->P1),&zero);
      MOVE1BYTE(((g->g1)->P2),&zero);
      return 0;

    case 2:
    case 3:
    case 4:
    case 5:
    case 113:
    case 114:
    case 115:
    case 116:
    case 117:
    case 118:
    case 119:
      printf("ISSTEP: time range indicator %d requires separate values for P1 and P2\n", g1_timerange(g));
      return -1;

    case 10:
      MOVE2BYTES(((g->g1)->P1),timeStep);
      return 0;

    case 123:
    case 124:
      MOVE1BYTE(((g->g1)->P1),&zero);
      MOVE1BYTE(((g->g1)->P2),timeStep);
      return 0;

    default:
      printf("ISSTEP: unable to set step for reserved time range indicator %d\n", g1_timerange(g));
      return -1;
  }

}

fortint RSSTEP(gribProduct ** grib, fortdouble * timeStep) {
gribProduct * g = *grib;
fortint TimeStep = (fortint) *timeStep;

  if( DEBUG1 ) printf("RSSTEP: timeStep = %f\n",*timeStep);

  return ISSTEP(grib,&TimeStep);
}

fortint ISSTEP1(gribProduct ** grib, fortint * timeStepP1) {
gribProduct * g = *grib;
fortint zero = 0;

  if( DEBUG1 ) printf("ISSTEP1: timeStepP1 = %d\n",*timeStepP1);

  switch( (int) g1_timerange(g) ) {

    case 0:
    case 2:
    case 3:
    case 4:
    case 5:
    case 113:
    case 114:
    case 115:
    case 116:
    case 117:
    case 118:
    case 119:
      MOVE1BYTE(((g->g1)->P1),timeStepP1);
      return 0;

    case 1:
    case 123:
    case 124:
      MOVE1BYTE(((g->g1)->P1),&zero);
      return 0;

    case 10:
      MOVE2BYTES(((g->g1)->P1),timeStepP1);
      return 0;

    default:
      printf("ISSTEP1: unable to set P1 for reserved time range indicator %d\n", g1_timerange(g));
      return -1;
  }

}

fortint RSSTEP1(gribProduct ** grib, fortdouble * timeStepP1) {
gribProduct * g = *grib;
fortint TimeStepP1 = (fortint) *timeStepP1;

  if( DEBUG1 ) printf("RSSTEP1: timeStepP1 = %f\n",*timeStepP1);

  return ISSTEP1(grib,&TimeStepP1);
}

fortint ISSTEP2(gribProduct ** grib, fortint * timeStepP2) {
gribProduct * g = *grib;
fortint zero = 0;

  if( DEBUG1 ) printf("ISSTEP2: timeStepP2 = %d\n",*timeStepP2);

  switch( (int) g1_timerange(g) ) {

    case 0:
    case 1:
      MOVE1BYTE(((g->g1)->P2),&zero);
      return 0;

    case 2:
    case 3:
    case 4:
    case 5:
    case 113:
    case 114:
    case 115:
    case 116:
    case 117:
    case 118:
    case 119:
    case 123:
    case 124:
      MOVE1BYTE(((g->g1)->P2),timeStepP2);
      return 0;

    case 10:
      printf("ISSTEP2: unable to set P2 for time range indicator %d\n", g1_timerange(g));
      return -1;

    default:
      printf("ISSTEP2: unable to set P2 for reserved time range indicator %d\n", g1_timerange(g));
      return -1;
  }

}

fortint RSSTEP2(gribProduct ** grib, fortdouble * timeStepP2) {
gribProduct * g = *grib;
fortint TimeStepP2 = (fortint) *timeStepP2;

  if( DEBUG1 ) printf("RSSTEP2: timeStepP2 = %f\n",*timeStepP2);

  return ISSTEP2(grib,&TimeStepP2);
}

fortint ISNUMAV(gribProduct ** grib, fortint * numberInAverage) {
gribProduct * g = *grib;

  if( DEBUG1 ) printf("ISNUMAV: numberInAverage = %d\n",*numberInAverage);

  MOVE2BYTES(((g->g1)->numberInAverage),numberInAverage);
  return 0;
}

fortint RSNUMAV(gribProduct ** grib, fortdouble * numberInAverage) {
gribProduct * g = *grib;
fortint NumberInAverage = (fortint) *numberInAverage;

  if( DEBUG1 ) printf("RSNUMAV: numberInAverage = %f\n",*numberInAverage);

  return ISNUMAV(grib,&NumberInAverage);

}

fortint ISNUMMS(gribProduct ** grib, fortint * numberMissing) {
gribProduct * g = *grib;

  if( DEBUG1 ) printf("ISNUMMS: numberMissing = %d\n",*numberMissing);

  MOVE1BYTE(((g->g1)->numberMissing),numberMissing);
  return 0;
}

fortint RSNUMMS(gribProduct ** grib, fortdouble * numberMissing) {
gribProduct * g = *grib;
fortint NumberMissing = (fortint) *numberMissing;

  if( DEBUG1 ) printf("RSNUMMS: numberMissing = %f\n",*numberMissing);

  return ISNUMMS(grib,&NumberMissing);
}

fortint ISSUBID(gribProduct ** grib, fortint * subCentreId) {
gribProduct * g = *grib;

  if( DEBUG1 ) printf("ISSUBID: subCentreId = %d\n",*subCentreId);

  MOVE1BYTE(((g->g1)->subCentreId),subCentreId);
  return 0;
}

fortint RSSUBID(gribProduct ** grib, fortdouble * subCentreId) {
gribProduct * g = *grib;
fortint SubCentreId = (fortint) *subCentreId;

  if( DEBUG1 ) printf("RSSUBID: subCentreId = %f\n",*subCentreId);

  return ISSUBID(grib,&SubCentreId);
}

fortint ISUDECF(gribProduct ** grib, fortint * decimalScale) {
gribProduct * g = *grib;
fortint scale = *decimalScale;

  if( DEBUG1 ) printf("ISUDECF: decimalScale = %d\n",*decimalScale);

  if( scale < 0 ) scale = (-scale) | 0x8000;
  MOVE2BYTES(((g->g1)->unitsDecimalScaleFactor),&scale);
  return 0;
}

fortint RSUDECF(gribProduct ** grib, fortdouble * decimalScale) {
gribProduct * g = *grib;
fortint DecimalScale = (fortint) *decimalScale;

  if( DEBUG1 ) printf("RSUDECF: decimalScale = %f\n",*decimalScale);

  return ISUDECF(grib,&DecimalScale);
}

fortint ISTYPE(gribProduct ** grib, fortint * ecmwfType) {
gribProduct * g = *grib;

  if( DEBUG1 ) printf("ISTYPE: ecmwfType = %d\n",*ecmwfType);

  if( ecmwfLocalDefinitionPresent(g) ) {
    MOVE1BYTE(((g->g1)->local.mars.type),ecmwfType);
    return 0;
  }

  if(  centreUsingECMWFLocalDefinition((*grib)) ) {
    MOVE1BYTE(((g->g1)->local.mars.type),ecmwfType);
    return 0;
  }
  else {
    if( DEBUG1 ) printf("ISTYPE: no ECMWF local definition present\n");
    return (fortint) -1;
  }

}

fortint RSTYPE(gribProduct ** grib, fortdouble * ecmwfType) {
gribProduct * g = *grib;
fortint EcmwfType = (fortint) *ecmwfType;

  if( DEBUG1 ) printf("RSTYPE: ecmwfType = %f\n",*ecmwfType);

  return ISTYPE(grib,&EcmwfType);
}

fortint ISCLASS(gribProduct ** grib, fortint * ecmwfClass) {
gribProduct * g = *grib;

  if( DEBUG1 ) printf("ISCLASS: ecmwfClass = %d\n",*ecmwfClass);

  if( ecmwfLocalDefinitionPresent(g) ) {
    MOVE1BYTE(((g->g1)->local.mars.ecmwfClass),ecmwfClass);
    return 0;
  }

  if(  centreUsingECMWFLocalDefinition((*grib)) ) {
    MOVE1BYTE(((g->g1)->local.mars.ecmwfClass),ecmwfClass);
    return 0;
  }
  else {
    if( DEBUG1 ) printf("ISCLASS: no ECMWF local definition present\n");
    return (fortint) -1;
  }

}

fortint RSCLASS(gribProduct ** grib, fortdouble * ecmwfClass) {
gribProduct * g = *grib;
fortint EcmwfClass = (fortint) *ecmwfClass;

  if( DEBUG1 ) printf("RSCLASS: ecmwfClass = %f\n",*ecmwfClass);

  return ISCLASS(grib,&EcmwfClass);
}

fortint ISSTREM(gribProduct ** grib, fortint * ecmwfStream) {
gribProduct * g = *grib;

  if( DEBUG1 ) printf("ISSTREM: ecmwfStream = %d\n",*ecmwfStream);

  if( ecmwfLocalDefinitionPresent(g) ) {
    MOVE2BYTES(((g->g1)->local.mars.stream),ecmwfStream);
    return 0;
  }

  if(  centreUsingECMWFLocalDefinition((*grib)) ) {
    MOVE2BYTES(((g->g1)->local.mars.stream),ecmwfStream);
    return 0;
  }
  else {
    if( DEBUG1 ) printf("ISSTREM: no ECMWF local definition present\n");
    return (fortint) -1;
  }

}

fortint RSSTREM(gribProduct ** grib, fortdouble * ecmwfStream) {
gribProduct * g = *grib;
fortint EcmwfStream = (fortint) *ecmwfStream;

  if( DEBUG1 ) printf("RSSTREM: ecmwfStream = %f\n",*ecmwfStream);

  return ISSTREM(grib,&EcmwfStream);
}

fortint ISEXPVR(gribProduct ** grib, fortint * ecmwfExpver) {
gribProduct * g = *grib;

  if( DEBUG1 ) printf("ISEXPVR: ecmwfExpver = %d\n",*ecmwfExpver);

  if( ecmwfLocalDefinitionPresent(g) ) {
    MOVE4BYTES(((g->g1)->local.mars.experimentVersionNumber),ecmwfExpver);
    return 0;
  }

  if(  centreUsingECMWFLocalDefinition((*grib)) ) {
    MOVE4BYTES(((g->g1)->local.mars.experimentVersionNumber),ecmwfExpver);
    return 0;
  }
  else {
    if( DEBUG1 ) printf("ISEXPVR: no ECMWF local definition present\n");
    return (fortint) -1;
  }

}

fortint RSEXPVR(gribProduct ** grib, fortdouble * ecmwfExpver) {
gribProduct * g = *grib;
fortint EcmwfExpver = (fortint) *ecmwfExpver;
gribSection1 * newG1;

  if( DEBUG1 ) printf("RSEXPVR: ecmwfExpver = %f\n",*ecmwfExpver);

  return ISEXPVR(grib,&EcmwfExpver);
}

fortint ISDEFIN(gribProduct ** grib, fortint * definitionNumber) {
gribProduct * g = *grib;
localDefinition * local;
fortint length, oldCopyLength, loop;
gribSection1 * newG1, * oldG1;

  if( DEBUG1 ) printf("ISDEFIN: definitionNumber = %d\n",*definitionNumber);

  if( DEBUG1 ) {
    if(ecmwfLocalDefinitionPresent(g) ||  centreUsingECMWFLocalDefinition(g)) {
      if( *definitionNumber == g1_definition(g) ) {
        printf("ISDEFIN: definitionNumber already has the given value\n");
      }
      else {
        printf("ISDEFIN: changing the ECMWF local definition from %d to %d\n",
                g1_definition(g), *definitionNumber);
      }
    }
    else
      printf("ISDEFIN: creating a new ECMWF local definition\n");
  }

  length = (40 + sizeof(marsHeader));

  switch( (int) *definitionNumber ) {

    case 1:
      length += sizeof(ECMWFdefinition1);
      break;

    case 2:
      length += sizeof(ECMWFdefinition2);
      break;

    case 3:
      length += sizeof(ECMWFdefinition3);
      break;

    case 5:
      length += sizeof(ECMWFdefinition5);
      break;

    case 6:
      length += sizeof(ECMWFdefinition6);
      break;

    case 7:
      length += sizeof(ECMWFdefinition7);
      break;

    case 8:
      length += sizeof(ECMWFdefinition8);
      break;

    case 9:
      length += sizeof(ECMWFdefinition9);
      break;

    case 10:
      length += sizeof(ECMWFdefinition10);
      break;

    case 11:
      length += sizeof(ECMWFdefinition11);
      break;

    case 14:
      length = 1080;
      break;

    case 15:
      length = 60;
      break;

    case 16:
      length = 80;
      break;

    case 18:
    case 19:
      length = 120;
      break;

    case 20:
      length += sizeof(ECMWFdefinition20);
      break;

    case 50:
      length = 300;
      break;

    default:
      printf("ISDEFIN: ECMWF local definition from %d not yet handled\n",
              *definitionNumber);
      return -1;
  }

  newG1 = (gribSection1 *) allocateMemory(length);
  {
    unsigned char * p = (unsigned char * ) newG1;
    for( loop = 40; loop < length; loop++ ) p[loop] = '\0';
  }

  if( ecmwfLocalDefinitionPresent(g) ||  centreUsingECMWFLocalDefinition(g) )
    oldCopyLength = (40 + sizeof(marsHeader));
  else
    oldCopyLength = 40;

  oldG1 = (gribSection1 *) (g->g1);
  memcpy(newG1,oldG1,oldCopyLength);

/*freeMemory(oldG1);*/
  freeMemory(oldG1);

  (g->g1) = newG1;
  MOVE1BYTE(((g->g1)->local.mars.definition),definitionNumber);
  MOVE3BYTES(((g->g1)->sectionLength),&length);
  return 0;

}

fortint RSDEFIN(gribProduct ** grib, fortdouble * definitionNumber) {
gribProduct * g = *grib;
fortint DefinitionNumber = (fortint) *definitionNumber;

  if( DEBUG1 ) printf("RSDEFIN: definitionNumber = %f\n",*definitionNumber);

  return ISDEFIN(grib,&DefinitionNumber);
}

fortint ISBTSPV(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
  if( DEBUG2) printf("ISBTSPV: set number of bits per value to %d\n",*value);
  MOVE1BYTE(((g->g4)->numberOfBitsPerValue),value);
  return 0;
}

fortint RSBTSPV(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) *value;

  if( DEBUG2) printf("RSBTSPV\n");
  return ISBTSPV(grib,&Ivalue);
}

fortint ISREPRS(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
fortint Value = *value, defaultValue, loop;
fortint zero = 0, ninety = 90000;
unsigned char * p;

  if( DEBUG1) printf("ISREPRS: set data representation type to %d\n",Value);

/*
//if( g2_datatype(g) == Value ) {
//  if( DEBUG1) printf("ISREPRS: data representation type remains unchanged\n");
//  return 0;
//}
*/
  MOVE1BYTE(((g->g2)->NV),&zero);
  defaultValue = 0xff;
  MOVE1BYTE(((g->g2)->PV_PL),&defaultValue);

  switch( (int) Value ) {

    case 0:
    case 10:
    case 20:
    case 30:
      if( DEBUG1) printf("ISREPRS: setup for latitude/longitude grid\n");
      MOVE1BYTE(((g->g2)->dataRepresentationType),value);

      MOVE2BYTES(((g->g2)->grid.latlon.numberOfPointsAlongParallel),&zero);
      MOVE2BYTES(((g->g2)->grid.latlon.numberOfPointsAlongMeridian),&zero);
      g->northSouthNumberOfPointsSet = g->westEastNumberOfPointsSet = FALSE;

      MOVE3BYTES(((g->g2)->grid.latlon.latitudeOfFirstPoint),&ninety);
      MOVE3BYTES(((g->g2)->grid.latlon.longitudeOfFirstPoint),&zero);
      defaultValue = ninety | 0x800000;
      MOVE3BYTES(((g->g2)->grid.latlon.latitudeOfLastPoint),&defaultValue);
      defaultValue = 360000;
      MOVE3BYTES(((g->g2)->grid.latlon.longitudeOfLastPoint),&defaultValue);
      g->northSet = g->southSet = g->westSet = g->eastSet = TRUE;

      defaultValue = 0x80;
      MOVE1BYTE(((g->g2)->grid.latlon.resolutionAndComponentsFlag),&defaultValue);
      MOVE2BYTES(((g->g2)->grid.latlon.iDirectionIncrement),&zero);
      MOVE2BYTES(((g->g2)->grid.latlon.jDirectionIncrement),&zero);
      g->northSouthIncrementSet = g->westEastIncrementSet = FALSE;

      MOVE1BYTE(((g->g2)->grid.latlon.scanningMode),&zero);
      MOVE4BYTES(((g->g2)->grid.latlon.setToZero),&zero);

      defaultValue = ninety | 0x800000;
      MOVE3BYTES(((g->g2)->grid.latlon.latitudeOfSouthPole),&defaultValue);
      MOVE3BYTES(((g->g2)->grid.latlon.longitudeOfSouthPole),&zero);
      MOVE4BYTES(((g->g2)->grid.latlon.angleOfRotationOrStretchingFactor),&zero);
      MOVE3BYTES(((g->g2)->grid.latlon.latitudeOfPoleOfStretching),&zero);
      MOVE3BYTES(((g->g2)->grid.latlon.longitudeOfPoleOfStretching),&zero);
      MOVE4BYTES(((g->g2)->grid.latlon.stretchingFactor),&zero);
      break;

    case 50:
    case 60:
    case 70:
    case 80:
      if( DEBUG1) printf("ISREPRS: setup for spherical harmonics\n");
      MOVE1BYTE(((g->g2)->dataRepresentationType),value);
      MOVE2BYTES(((g->g2)->grid.spectral.J),&zero);
      MOVE2BYTES(((g->g2)->grid.spectral.K),&zero);
      MOVE2BYTES(((g->g2)->grid.spectral.M),&zero);
      defaultValue = 1;
      MOVE1BYTE(((g->g2)->grid.spectral.representationType),&defaultValue);
      MOVE1BYTE(((g->g2)->grid.spectral.representationMode),&defaultValue);
      p = (unsigned char *) &((g->g2)->grid.spectral.setToZero);
      for( loop = 0; loop < 18; loop++ ) { MOVE1BYTE(p,&zero); p++; }
      defaultValue = ninety | 0x800000;
      MOVE3BYTES(((g->g2)->grid.spectral.latitudeOfSouthPole),&defaultValue);
      MOVE3BYTES(((g->g2)->grid.spectral.longitudeOfSouthPole),&zero);
      MOVE4BYTES(((g->g2)->grid.spectral.angleOfRotationOrStretchingFactor),&zero);
      MOVE3BYTES(((g->g2)->grid.spectral.latitudeOfPoleOfStretching),&zero);
      MOVE3BYTES(((g->g2)->grid.spectral.longitudeOfPoleOfStretching),&zero);
      MOVE4BYTES(((g->g2)->grid.spectral.stretchingFactor),&zero);
      break;

    case  4:
    case 14:
    case 24:
    case 34:
      if( DEBUG1) printf("ISREPRS: setup for gaussian grids\n");
      MOVE1BYTE(((g->g2)->dataRepresentationType),value);

      MOVE2BYTES(((g->g2)->grid.gaussian.numberOfPointsAlongParallel),&zero);
      MOVE2BYTES(((g->g2)->grid.gaussian.numberOfPointsAlongMeridian),&zero);
      g->northSouthNumberOfPointsSet = g->westEastNumberOfPointsSet = FALSE;

      MOVE3BYTES(((g->g2)->grid.gaussian.latitudeOfFirstPoint),&ninety);
      MOVE3BYTES(((g->g2)->grid.gaussian.longitudeOfFirstPoint),&zero);
      defaultValue = ninety | 0x800000;
      MOVE3BYTES(((g->g2)->grid.gaussian.latitudeOfLastPoint),&defaultValue);
      defaultValue = 360000;
      MOVE3BYTES(((g->g2)->grid.gaussian.longitudeOfLastPoint),&defaultValue);
      g->northSet = g->southSet = g->westSet = g->eastSet = TRUE;

      defaultValue = 0x80;
      MOVE1BYTE(((g->g2)->grid.gaussian.resolutionAndComponentsFlag),&defaultValue);
      MOVE2BYTES(((g->g2)->grid.gaussian.iDirectionIncrement),&zero);
      MOVE2BYTES(((g->g2)->grid.gaussian.numberOfParallelsBetweenPoleAndEquator),&zero);
      g->northSouthIncrementSet = g->westEastIncrementSet = FALSE;

      MOVE1BYTE(((g->g2)->grid.gaussian.scanningMode),&zero);
      MOVE4BYTES(((g->g2)->grid.gaussian.setToZero),&zero);

      defaultValue = ninety | 0x800000;
      MOVE3BYTES(((g->g2)->grid.gaussian.latitudeOfSouthPole),&defaultValue);
      MOVE3BYTES(((g->g2)->grid.gaussian.longitudeOfSouthPole),&zero);
      MOVE4BYTES(((g->g2)->grid.gaussian.angleOfRotationOrStretchingFactor),&zero);
      MOVE3BYTES(((g->g2)->grid.gaussian.latitudeOfPoleOfStretching),&zero);
      MOVE3BYTES(((g->g2)->grid.gaussian.longitudeOfPoleOfStretching),&zero);
      MOVE4BYTES(((g->g2)->grid.gaussian.stretchingFactor),&zero);
      break;

    default:
      if( DEBUG1) printf("ISREPRS: data representation type not yet handled\n");
      break;
  }

  return 0;
}

fortint RSREPRS(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) *value;
  if( DEBUG2) printf("RSREPRS\n");
  return ISREPRS(grib,&Ivalue);
}

fortint ISNWLAT(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
fortint Value = *value;
  if( DEBUG2) printf("ISNWLAT: value = %d\n", Value);
  if( !generalLatLonGrid(g) ) return 0;

  if( Value < 0 ) Value = 0x800000 | (-Value);
  MOVE3BYTES(((g->g2)->grid.latlon.latitudeOfFirstPoint),&Value);
  g->northSet = TRUE;
  adjustGridAreaDefinition(g,(int)NORTH);
  return 0;
}

fortint RSNWLAT(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value * 1000.0);
  if( DEBUG2) printf("ISNWLAT\n");
  return ISNWLAT(grib,&Ivalue);
}

fortint ISNWLON(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
fortint Value = *value;
  if( DEBUG2) printf("ISNWLON: value = %d\n", Value);
  if( !generalLatLonGrid(g) ) return 0;

  if( Value < 0 ) Value = 0x800000 | (-Value);
  MOVE3BYTES(((g->g2)->grid.latlon.longitudeOfFirstPoint),&Value);
  g->westSet = TRUE;
  adjustGridAreaDefinition(g,(int)WEST);
  return 0;
}

fortint RSNWLON(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value * 1000.0);
  if( DEBUG2) printf("ISNWLON\n");
  return ISNWLON(grib,&Ivalue);
}

fortint ISSELAT(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
fortint Value = *value;
  if( DEBUG2) printf("ISSELAT: value = %d\n", Value);
  if( !generalLatLonGrid(g) ) return 0;

  if( Value < 0 ) Value = 0x800000 | (-Value);
  MOVE3BYTES(((g->g2)->grid.latlon.latitudeOfLastPoint),&Value);
  g->southSet = TRUE;
  adjustGridAreaDefinition(g,(int)SOUTH);
  return 0;
}

fortint RSSELAT(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value * 1000.0);
  if( DEBUG2) printf("ISSELAT\n");
  return ISSELAT(grib,&Ivalue);
}

fortint ISSELON(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
fortint Value = *value;
  if( DEBUG2) printf("ISSELON: value = %d\n", Value);
  if( !generalLatLonGrid(g) ) return 0;

  if( Value < 0 ) Value = 0x800000 | (-Value);
  MOVE3BYTES(((g->g2)->grid.latlon.longitudeOfLastPoint),&Value);
  g->eastSet = TRUE;
  adjustGridAreaDefinition(g,(int)EAST);
  return 0;
}

fortint RSSELON(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value * 1000.0);
  if( DEBUG2) printf("ISSELON\n");
  return ISSELON(grib,&Ivalue);
}

fortint ISDI(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
  if( DEBUG2) printf("ISDI: value = %d\n", *value);
  if( !generalLatLonGrid(g) ) return 0;

  MOVE2BYTES(((g->g2)->grid.latlon.iDirectionIncrement), value);
  g->westEastIncrementSet = TRUE;
  adjustGridAreaDefinition(g,(int)W_E_INC);
  return 0;
}

fortint RSDI(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value * 1000.0);
  if( DEBUG2) printf("RSDI\n");
  return ISDI(grib,&Ivalue);
}

fortint ISDJ(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
  if( DEBUG2) printf("ISDJ: value = %d\n", *value);
  if( !generalLatLonGrid(g) ) return 0;

  MOVE2BYTES(((g->g2)->grid.latlon.jDirectionIncrement), value);
  g->northSouthIncrementSet = TRUE;
  adjustGridAreaDefinition(g,(int)N_S_INC);
  return 0;
}

fortint RSDJ(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value * 1000.0);
  if( DEBUG2) printf("RSDJ\n");
  return ISDJ(grib,&Ivalue);
}

fortint ISDIJ(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
  if( DEBUG2) printf("ISDIJ: value = %d\n", *value);
  if( !generalLatLonGrid(g) ) return 0;

  ISDI(grib,value);
  ISDJ(grib,value);
  return 0;
}

fortint RSDIJ(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value * 1000.0);
  if( DEBUG2) printf("RSDIJ\n");
  return ISDIJ(grib,&Ivalue);
}

fortint ISNJ(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
  if( DEBUG2) printf("ISNJ: value = %d\n", *value);
  if( !generalLatLonGrid(g) ) return 0;

  MOVE2BYTES(((g->g2)->grid.latlon.numberOfPointsAlongMeridian), value);
  g->northSouthNumberOfPointsSet = TRUE;
  adjustGridAreaDefinition(g,(int)N_S_PTS);
  return 0;
}

fortint RSNJ(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value);
  if( DEBUG2) printf("ISNJ\n");
  return ISNJ(grib,&Ivalue);
}

fortint ISNI(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
  if( DEBUG2) printf("ISNI: value = %d\n", *value);
  if( !generalLatLonGrid(g) ) return 0;

  MOVE2BYTES(((g->g2)->grid.latlon.numberOfPointsAlongParallel), value);
  g->westEastNumberOfPointsSet = TRUE;
  adjustGridAreaDefinition(g,(int)W_E_PTS);
  return 0;
}

fortint RSNI(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value);
  if( DEBUG2) printf("ISNI\n");
  return ISNI(grib,&Ivalue);
}

fortint ISJ(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
fortint truncation = *value;

  if( DEBUG2) printf("ISJ value = %d\n", truncation);
  if( anySpectralField(g) ) {
    MOVE2BYTES(((g->g2)->grid.spectral.J),value);
    g->numberOfValues = (truncation+1) * (truncation+2);
  }
  return 0;
}

fortint RSJ(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value);
  if( DEBUG2) printf("RSJ value = %f\n", *value);
  return ISJ(grib,&Ivalue);
}

fortint ISK(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
  if( DEBUG2) printf("ISK value = %d\n", *value);
  if( anySpectralField(g) ) MOVE2BYTES(((g->g2)->grid.spectral.K),value);
  return 0;
}

fortint RSK(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value);
  if( DEBUG2) printf("RSK value = %f\n", *value);
  return ISK(grib,&Ivalue);
}

fortint ISM(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
  if( DEBUG2) printf("ISM value = %d\n", *value);
  if( anySpectralField(g) ) MOVE2BYTES(((g->g2)->grid.spectral.M),value);
  return 0;
}

fortint RSM(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value);
  if( DEBUG2) printf("RSM value = %f\n", *value);
  return ISM(grib,&Ivalue);
}

fortint ISJKM(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
  if( DEBUG2) printf("ISJKM value = %d\n", *value);
  ISJ(grib,value);
  ISK(grib,value);
  ISM(grib,value);
  return 0;
}

fortint RSJKM(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value);
  if( DEBUG2) printf("RSJKM value = %f\n", *value);
  return ISJKM(grib,&Ivalue);
}

fortint ISTJ(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
  if( DEBUG2) printf("ISTJ value = %d\n", *value);
  if( anyComplexPackedSpectralField(g) )
    MOVE1BYTE(((g->g4)->data.complexSpectral.J),value);
  return 0;
}

fortint RSTJ(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value);
  if( DEBUG2) printf("RSTJ value = %f\n", *value);
  return ISTJ(grib,&Ivalue);
}

fortint ISTK(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
  if( DEBUG2) printf("ISTK value = %d\n", *value);
  if( anyComplexPackedSpectralField(g) )
    MOVE1BYTE(((g->g4)->data.complexSpectral.K),value);
  return 0;
}

fortint RSTK(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value);
  if( DEBUG2) printf("RSTK value = %f\n", *value);
  return ISTK(grib,&Ivalue);
}

fortint ISTM(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
  if( DEBUG2) printf("ISTM value = %d\n", *value);
  if( anyComplexPackedSpectralField(g) )
    MOVE1BYTE(((g->g4)->data.complexSpectral.M),value);
  return 0;
}

fortint RSTM(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value);
  if( DEBUG2) printf("RSTM value = %f\n", *value);
  return ISTM(grib,&Ivalue);
}

fortint ISTJKM(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
  if( DEBUG2) printf("ISTJKM value = %d\n", *value);
  ISTJ(grib,value);
  ISTK(grib,value);
  ISTM(grib,value);
  return 0;
}

fortint RSTJKM(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value);
  if( DEBUG2) printf("RSTJKM value = %f\n", *value);
  return ISTJKM(grib,&Ivalue);
}

fortint ISLATRP(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
fortint Value = *value;
  if( DEBUG2) printf("ISLATRP: value = %d\n", Value);
  if( !generalRotatedGrid(g) ) return 0;

  if( Value < 0 ) Value = 0x800000 | (-Value);
  MOVE3BYTES(((g->g2)->grid.latlon.latitudeOfSouthPole),&Value);
  return 0;
}

fortint RSLATRP(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value * 1000.0);
  if( DEBUG2) printf("RSLATRP\n");
  return ISLATRP(grib,&Ivalue);
}

fortint ISLONRP(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
fortint Value = *value;
  if( DEBUG2) printf("ISLONRP: value = %d\n", Value);
  if( !generalRotatedGrid(g) ) return 0;

  if( Value < 0 ) Value = 0x800000 | (-Value);
  MOVE3BYTES(((g->g2)->grid.latlon.longitudeOfSouthPole),&Value);
  return 0;
}

fortint RSLONRP(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value * 1000.0);
  if( DEBUG2) printf("RSLONRP\n");
  return ISLONRP(grib,&Ivalue);
}

fortint RSROTAT(gribProduct ** grib, fortdouble * value) {
gribProduct * g = *grib;
fortint valueExponent, valueMantissa, numberOfBitsPerInteger;
fortint status;

  if( DEBUG2) printf("RSROTAT: value = %f\n", *value);
  if( !generalRotatedGrid(g) ) return 0;

  numberOfBitsPerInteger = sizeof(fortint) * 8;
  status = REF2GRB(value,&valueExponent,
                   &valueMantissa,&numberOfBitsPerInteger);
  if( status ) {
    printf("RSROTAT: call to REF2GRB failed\n");
    exit(1);
  }
  MOVE1BYTE(((g->g2)->grid.latlon.angleOfRotationOrStretchingFactor),&valueExponent);
  MOVE3BYTES((((g->g2)->grid.latlon.angleOfRotationOrStretchingFactor)+1),&valueMantissa);
  return 0;
}

fortint ISROTAT(gribProduct ** grib, fortint * value) {
fortdouble Value = ((fortdouble) *value) / 1000.0;
  if( DEBUG2) printf("ISROTAT: value = %d\n", *value);
  return RSROTAT(grib,&Value);
}

fortint ISLATSP(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
fortint Value = *value;
  if( DEBUG2) printf("ISLATSP: value = %d\n", Value);
  if( !generalStretchedGrid(g) ) return 0;

  if( Value < 0 ) Value = 0x800000 | (-Value);

  if( generalStretchedAndRotatedGrid(g) )
    MOVE3BYTES(((g->g2)->grid.latlon.latitudeOfPoleOfStretching),&Value);
  else
    MOVE3BYTES(((g->g2)->grid.latlon.latitudeOfSouthPole),&Value);

  return 0;
}

fortint RSLATSP(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value * 1000.0);
  if( DEBUG2) printf("RSLATSP\n");
  return ISLATSP(grib,&Ivalue);
}

fortint ISLONSP(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
fortint Value = *value;
  if( DEBUG2) printf("ISLONSP: value = %d\n", Value);
  if( !generalStretchedGrid(g) ) return 0;

  if( Value < 0 ) Value = 0x800000 | (-Value);

  if( generalStretchedAndRotatedGrid(g) )
    MOVE3BYTES(((g->g2)->grid.latlon.longitudeOfPoleOfStretching),&Value);
  else
    MOVE3BYTES(((g->g2)->grid.latlon.longitudeOfSouthPole),&Value);

  return 0;
}

fortint RSLONSP(gribProduct ** grib, fortdouble * value) {
fortint Ivalue = (fortint) (*value * 1000.0);
  if( DEBUG2) printf("RSLONSP\n");
  return ISLONSP(grib,&Ivalue);
}

fortint RSSFACT(gribProduct ** grib, fortdouble * value) {
gribProduct * g = *grib;
fortint valueExponent, valueMantissa, numberOfBitsPerInteger;
fortint status;

  if( DEBUG2) printf("RSSFACT: value = %f\n", *value);
  if( !generalStretchedGrid(g) ) return 0;

  numberOfBitsPerInteger = sizeof(fortint) * 8;
  status = REF2GRB(value,&valueExponent,
                   &valueMantissa,&numberOfBitsPerInteger);
  if( status ) {
    printf("RSSFACT: call to REF2GRB failed\n");
    exit(1);
  }

  if( generalStretchedAndRotatedGrid(g) ) {
    MOVE1BYTE(((g->g2)->grid.latlon.stretchingFactor),&valueExponent);
    MOVE3BYTES((((g->g2)->grid.latlon.stretchingFactor)+1),&valueMantissa);
  }
  else {
    MOVE1BYTE(((g->g2)->grid.latlon.angleOfRotationOrStretchingFactor),&valueExponent);
    MOVE3BYTES((((g->g2)->grid.latlon.angleOfRotationOrStretchingFactor)+1),&valueMantissa);
  }
  return 0;
}

fortint ISSFACT(gribProduct ** grib, fortint * value) {
fortdouble Value = ((fortdouble) *value) / 1000.0;
  if( DEBUG2) printf("ISSFACT: value = %d\n", *value);
  return RSSFACT(grib,&Value);
}

fortint ISSETQG(gribProduct ** grib, fortint * value) {
gribProduct * g = *grib;
fortint northToSouth;
fortint number = (*value), missing = 0xffff, zero = 0;
fortdouble * gaussianLatitudes;
fortint * countOfPointsAtLatitudes;
fortint total, loop, status, one = 1, coordinate;
unsigned char htype[2] = "R";
fortdouble lastLongitude;
gribSection2 * newSection2;
fortint newSection2Size, listOffset, count;
unsigned char * p;

  if( DEBUG2) printf("ISSETQG: reduced gaussian number = %d\n", *value);

  if( !anyGaussianGrid(g) ) return 0;
/*
//if( number == g2_gaussNumber(g) ) return 0;
*/

  MOVE2BYTES(((g->g2)->grid.gaussian.numberOfPointsAlongParallel),&missing);
  northToSouth = number * 2;
  MOVE2BYTES(((g->g2)->grid.gaussian.numberOfPointsAlongMeridian),&northToSouth);

  MOVE3BYTES(((g->g2)->grid.gaussian.longitudeOfFirstPoint),&zero);

  MOVE1BYTE(((g->g2)->grid.gaussian.resolutionAndComponentsFlag),&zero);

  MOVE2BYTES(((g->g2)->grid.gaussian.iDirectionIncrement),&missing);
  MOVE2BYTES(((g->g2)->grid.gaussian.numberOfParallelsBetweenPoleAndEquator),
             &number);
  MOVE1BYTE(((g->g2)->grid.gaussian.scanningMode),&zero);

  MOVE4BYTES(((g->g2)->grid.gaussian.setToZero),&zero);
  MOVE3BYTES(((g->g2)->grid.gaussian.latitudeOfSouthPole),&zero);
  MOVE3BYTES(((g->g2)->grid.gaussian.longitudeOfSouthPole),&zero);
  MOVE4BYTES(((g->g2)->grid.gaussian.angleOfRotationOrStretchingFactor),&zero);
  MOVE3BYTES(((g->g2)->grid.gaussian.latitudeOfPoleOfStretching),&zero);
  MOVE3BYTES(((g->g2)->grid.gaussian.longitudeOfPoleOfStretching),&zero);
  MOVE4BYTES(((g->g2)->grid.gaussian.stretchingFactor),&zero);

  gaussianLatitudes =
                 (fortdouble *) allocateMemory(northToSouth*sizeof(fortdouble));
  countOfPointsAtLatitudes =
                 (fortint *) allocateMemory(northToSouth*sizeof(fortint));
  JGETGG(&number,htype,gaussianLatitudes,countOfPointsAtLatitudes,&status,
         (long)one);
  if( status ) {
    printf("ISSETQG: JGETGG failed for gaussian number %d\n",number);
    exit(1);
  }

  coordinate = (fortint) ((0.0005 + gaussianLatitudes[0])*1000.0);
  MOVE3BYTES(((g->g2)->grid.gaussian.latitudeOfFirstPoint),&coordinate);

  coordinate = 0x800000 | coordinate;
  MOVE3BYTES(((g->g2)->grid.gaussian.latitudeOfLastPoint),&coordinate);

  freeMemory(gaussianLatitudes);

  lastLongitude = 360.0 - (360/(fortdouble)countOfPointsAtLatitudes[number-1]);
  coordinate = (fortint) (lastLongitude * 1000.0);
  if( coordinate < 0 ) coordinate = 0x800000 | (-coordinate);
  MOVE3BYTES(((g->g2)->grid.gaussian.longitudeOfLastPoint),&coordinate);

  switch( (int) g2_datatype(g) ) {

    case  4:
      listOffset = 32;
      break;

    case 14:
    case 24:
      listOffset = 42;
      break;

    case 34:
      listOffset = 52;
      break;

    default:
      printf("ISSETQG: unexpected gaussian representation type = %d\n",
             g2_datatype(g));
      exit(1);
  }
  listOffset += g2_NV(g) * 4;

  newSection2Size = listOffset + (northToSouth * 2);
  newSection2 = (gribSection2 *) allocateMemory(newSection2Size);
  memcpy(newSection2,(g->g2),sizeof(gribSection2));

  total = 0;
  p = (unsigned char *) newSection2;
  p += listOffset;
  for(loop=0;loop<northToSouth;loop++) {
    count = countOfPointsAtLatitudes[loop];
    total += count;
    MOVE2BYTES(p,&count);
    p +=2;
  }
  g->numberOfValues = total;
  freeMemory((g->g2));
  g->g2 = newSection2;

  MOVE3BYTES(((g->g2)->sectionLength),&newSection2Size);
  listOffset++;
  MOVE1BYTE(((g->g2)->PV_PL),&listOffset);

  return 0;
}

fortint RSSETQG(gribProduct ** grib, fortdouble * value) {
fortint Value = (fortint) *value;
  if( DEBUG2) printf("RSSETQG\n");
  return ISSETQG(grib,&Value);;
}

fortint ISSETRG(gribProduct ** grib, fortint * value) {
  if( DEBUG2) printf("ISSETRG: regular gaussian number = %d\n", *value);
  return 0;
}

fortint RSSETRG(gribProduct ** grib, fortdouble * value) {
  if( DEBUG2) printf("RSSETRG\n");
  return 0;
}

fortint ISDUMMY(gribProduct ** grib, fortint * value) {
  if( DEBUG2) printf("ISDUMMY\n");
  return 0;
}

fortint RSDUMMY(gribProduct ** grib, fortdouble * value) {
  if( DEBUG2) printf("RSDUMMY\n");
  return 0;
}

fortint SVALUES(
  gribProduct ** grib,
  fortdouble * arrayOfUnpackedValues,
  fortint * arraySize,
  fortdouble * userSuppliedMissingValue ) {
gribProduct * g = *grib;
fortint loop, startValue, lengthOfSection3;
fortint N, numberOfUnpackedValues, nextNonMissingValue = 0;
fortint numberOfValues, numberOfMissingValues = 0;
fortdouble missingValue = *userSuppliedMissingValue;
fortdouble value, maximum, minimum, truncation;
unsigned char * bitmap, * section3;

  if( DEBUG1 ) printf("SVALUES\n");
/*
// Calculate the number of points from the GRIB headers
*/
  numberOfUnpackedValues = g->numberOfValues;
  if( DEBUG1 )
    printf("SVALUES: numberOfUnpackedValues = %d\n",numberOfUnpackedValues);

  if( numberOfUnpackedValues <= 0 ) {
    printf("SVALUES: grib headers not sufficiently configured for packing\n");
    return -1;
  }

  if( numberOfUnpackedValues > *arraySize ) {
    printf(
     "SVALUES: calculated number of values (%d) greater than array size(%d)\n",
      numberOfUnpackedValues, *arraySize);
    return -1;
  }

  N = numberOfUnpackedValues;
/*
// For gridpoint fields, it may be necessary to create a bitmap if
// there are missing data values
*/
  if( !anySpectralField(g) ) {
    if( DEBUG1 ) printf("SVALUES: gridpoint field\n");
    lengthOfSection3 = (6+(N+7)/8);
    if( MOD(lengthOfSection3,2) == 1 ) lengthOfSection3++;
    section3 = (unsigned char *) allocateMemory(lengthOfSection3);
    bitmap = section3 + 6;
    for( loop = 0; loop < (N+7)/8; loop++) bitmap[loop] = 0;

    maximum = minimum = arrayOfUnpackedValues[0];
    for( loop = 0; loop < N; loop++) {
      value = arrayOfUnpackedValues[loop];
      if( value == missingValue ) {
        numberOfMissingValues++;
      }
      else {
        setBitMap(bitmap,loop);
        if( (value < minimum) || (minimum == missingValue) ) {
          minimum = value;
        }
        if( (value > maximum) || (maximum == missingValue) ) {
          maximum = value;
        }
      }
    }
/*
// Create section 3 if there is a bitmap
*/
    if( numberOfMissingValues > 0 ) {
      fortint numberOfUnusedBits = (lengthOfSection3-6)*8 - N;
      fortint zero = 0;

      if( DEBUG1 ) printf("SVALUES: create section 3 bitmap\n");

      g->bitmapped = 1;
      freeMemory(g->g3);
      g->g3 = (gribSection3 *) section3;
      MOVE3BYTES((g->g3)->sectionLength,&lengthOfSection3);
      MOVE1BYTE((g->g3)->numberOfUnusedBits,&numberOfUnusedBits);
      MOVE2BYTES((g->g3)->tableReference,&zero);
      *((g->g1)->section2and3PresentFlag) |= 0x40;
/*
//    Repack the input array, leaving out the missing data
*/
      nextNonMissingValue = 0;
      for( loop = 0; loop < N; loop++) {
        if( arrayOfUnpackedValues[loop] != missingValue )
          arrayOfUnpackedValues[nextNonMissingValue++] =
            arrayOfUnpackedValues[loop];
      }
    }
    else {
      g->bitmapped = 0;
      *((g->g1)->section2and3PresentFlag) &= 0xbf;
      freeMemory(section3);
    }
    numberOfValues = N - numberOfMissingValues;

    if( simplePacking(g) ) {
      fortint estimatedSizeOfSection4;
      unsigned char * section4;
      fortint status, lengthOfSection4, numberOfUnusedBits;
      fortint referenceValueExponent, referenceValueMantissa;
      fortint numberOfBitsPerPackedValue, numberOfBitsPerInteger;
      fortint firstCoefficientExponent, firstCoefficientMantissa;
      fortint integerScale = 0;
      fortdouble range = maximum - minimum;
      fortdouble realScale = 1.0;
      fortint numberOfValuesToScale, startOfPackedValueBits;
      fortint * arrayOfScaledValues;

      if( DEBUG1 ) printf("SVALUES: gridpoint field with simple packing\n");

      estimatedSizeOfSection4 = 512+(N*sizeof(fortint)*8+7)/8;
      section4 = (unsigned char *) allocateMemory(estimatedSizeOfSection4);


      numberOfBitsPerPackedValue = g4_bits(g);
      numberOfBitsPerInteger = sizeof(fortint) * 8;
      if( numberOfBitsPerPackedValue == numberOfBitsPerInteger)
        numberOfBitsPerPackedValue--;

      if( PRACTICALLYZERO(range) )
        integerScale = 0;
      else
        if( (minimum != 0) && PRACTICALLYZERO(range/minimum) ){
          integerScale = 0;
        }
        else
          if( PRACTICALLYZERO(range-1.0) )
            integerScale = 1 - numberOfBitsPerPackedValue;
          else
            if( range > 1.0 ) {
              for( loop = 1; loop < 128; loop++ ) {
                realScale *= 2.0;
                if( realScale > (range + SMALL) ) {
                  integerScale = loop - numberOfBitsPerPackedValue;
                  break;
                }
              }
            }
            else {
              for( loop = 1; loop < 127; loop++ ) {
                realScale /= 2.0;
                if( realScale < (range - SMALL) ) {
                  integerScale = 1 - loop - numberOfBitsPerPackedValue;
                  break;
                }
              }
            }

      realScale = (fortdouble) pow((double)2.0,(double)integerScale);
      if( integerScale < 0 ) integerScale = 0x8000 | (-integerScale);
      MOVE2BYTES((section4+4),&integerScale);

      status = REF2GRB(&minimum,&referenceValueExponent,
                       &referenceValueMantissa,&numberOfBitsPerInteger);
      if( status ) {
        printf("SVALUES: call to REF2GRB failed\n");
        return -1;
      }
      MOVE1BYTE((section4+6),&referenceValueExponent);
      MOVE3BYTES((section4+7),&referenceValueMantissa);

      MOVE1BYTE((section4+10),&numberOfBitsPerPackedValue);

      status = REF2GRB(&arrayOfUnpackedValues[0],&firstCoefficientExponent,
                       &firstCoefficientMantissa,&numberOfBitsPerInteger);
      if( status ) {
        printf("SVALUES: call to REF2GRB failed\n");
        return -1;
      }
      MOVE1BYTE((section4+11),&firstCoefficientExponent);
      MOVE3BYTES((section4+12),&firstCoefficientMantissa);

      numberOfValuesToScale = numberOfValues;
      arrayOfScaledValues =
        (fortint *) allocateMemory(numberOfValuesToScale*sizeof(fortint));

      INSCAL(&arrayOfUnpackedValues[0],arrayOfScaledValues,
             &numberOfValuesToScale,&minimum,&realScale,
             &numberOfBitsPerPackedValue);

      startOfPackedValueBits = 88;
      lengthOfSection4 = estimatedSizeOfSection4/sizeof(fortint);
      INXBIT((fortint*)section4,&lengthOfSection4,
             &startOfPackedValueBits,arrayOfScaledValues,
             &numberOfValuesToScale,&numberOfBitsPerInteger,
             &numberOfBitsPerPackedValue,"C",&status,(long)1);
      if( status ) {
        printf("SVALUES: call to INXBIT failed\n");
        return -1;
      }
      freeMemory(arrayOfScaledValues);

      lengthOfSection4 = (startOfPackedValueBits + 7)/8;
      if( MOD(lengthOfSection4,2) == 1 ) lengthOfSection4++;
      MOVE3BYTES(section4,&lengthOfSection4);

      numberOfUnusedBits = lengthOfSection4*8 - startOfPackedValueBits;
      if( !floatingPoint(g) ) numberOfUnusedBits |= 0x20;
      MOVE1BYTE((section4+3),&numberOfUnusedBits);

      freeMemory(g->g4);
      g->g4 = (gribSection4 *) section4;
    }
    else {
printf("Second order packing not yet handled!\n");
      return -1;
    }
    return 0;
  }

/*
// Spherical harmonics
*/
  if( anySpectralField(g) && floatingPoint(g) ) {
    fortint estimatedSizeOfSection4;
    unsigned char * section4;

    estimatedSizeOfSection4 = 512+(N*sizeof(fortint)*8+7)/8;
    section4 = (unsigned char *) allocateMemory(estimatedSizeOfSection4);

    if( simplePacking(g) )
      startValue = 1;
    else
      startValue = 0;
    maximum = minimum = arrayOfUnpackedValues[startValue];
    for( loop = startValue; loop < N; loop++) {
      value = arrayOfUnpackedValues[loop];
      if( (value < minimum) || (minimum == missingValue) ) {
        minimum = value;
      }
      if( (value > maximum) || (maximum == missingValue) ) {
        maximum = value;
      }
    }

/*
// If complex packing required, use GRIBEX routines GRSMKP and CSECT4
*/
    if( !simplePacking(g) ) {
      fortint * ksec1, * ksec4;
      fortint ktrunc, kleng, knspt, status, adjustment;
      fortint numberOfBitsPerInteger, numberOfBitsPerPackedValue;

      if( DEBUG1 ) printf("SVALUES: spectral field with complex packing\n");

      ksec1 = (fortint *) allocateMemory(1024*sizeof(fortint));
      for( loop = 0; loop < 1024; loop++) ksec1[loop] = 0;

      ksec4 = (fortint *) allocateMemory(512*sizeof(fortint));
      for( loop = 0; loop < 512; loop++) ksec4[loop] = 0;

      ksec1[22] = g1_scale(g);

      ksec4[ 1] = g4_bits(g);
      ksec4[16] = g4_ip(g);
      ksec4[17] = g4_j(g);
      ksec4[18] = g4_k(g);
      ksec4[19] = g4_m(g);

      ktrunc = g2_J(g);
      kleng  = (estimatedSizeOfSection4+sizeof(fortint)-1)/sizeof(fortint);
      knspt  = 0;
      numberOfBitsPerInteger  = sizeof(fortint) * 8;
      numberOfBitsPerPackedValue  = g4_bits(g);

      {
        fortint forceComputation = 1;
        GRSMKP(&forceComputation);
      }
      status = CSECT4(arrayOfUnpackedValues,&ktrunc,ksec1,ksec4,
                      (fortint *)section4,&kleng,&knspt,
                      &numberOfBitsPerInteger,&numberOfBitsPerPackedValue);
      if( status ) {
        printf("SVALUES: call to CSECT4 failed\n");
        return -1;
      }
/*
// NB. Adjust the octet number at which first-order packed data begins
//     to be consistent with an ECMWF bug.
*/
      adjustment = TWOBYTEINT(section4+11);
      adjustment += 8 + g1_length(g) + g2_length(g);
      MOVE2BYTES((section4+11),&adjustment);

      freeMemory(g->g4);
      g->g4 = (gribSection4 *) section4;
    }
/*
// Simple packing required
*/
    else {
      fortint status, lengthOfSection4, numberOfUnusedBits;
      fortint referenceValueExponent, referenceValueMantissa;
      fortint numberOfBitsPerPackedValue, numberOfBitsPerInteger;
      fortint firstCoefficientExponent, firstCoefficientMantissa;
      fortint integerScale = 0;
      fortdouble range = maximum - minimum;
      fortdouble realScale = 1.0;
      fortint numberOfValuesToScale, startOfPackedValueBits;
      fortint * arrayOfScaledValues;

      if( DEBUG1 ) printf("SVALUES: spectral field with simple packing\n");

      numberOfBitsPerPackedValue = g4_bits(g);
      numberOfBitsPerInteger = sizeof(fortint) * 8;
      if( numberOfBitsPerPackedValue == numberOfBitsPerInteger)
        numberOfBitsPerPackedValue--;

      if( PRACTICALLYZERO(range) )
        integerScale = 0;
      else
        if( (minimum != 0) && PRACTICALLYZERO(range/minimum) )
          integerScale = 0;
        else
          if( PRACTICALLYZERO(range-1.0) )
            integerScale = 1 - numberOfBitsPerPackedValue;
          else
            if( range > 1.0 ) {
              for( loop = 1; loop < 128; loop++ ) {
                realScale *= 2.0;
                if( realScale > (range + SMALL) ) {
                  integerScale = loop - numberOfBitsPerPackedValue;
                  break;
                }
              }
            }
            else {
              for( loop = 1; loop < 127; loop++ ) {
                realScale /= 2.0;
                if( realScale < (range - SMALL) ) {
                  integerScale = 1 - loop - numberOfBitsPerPackedValue;
                  break;
                }
              }
            }

      realScale = (fortdouble) pow((double)2.0,(double)integerScale);
      if( integerScale < 0 ) integerScale = 0x8000 | (-integerScale);
      MOVE2BYTES((section4+4),&integerScale);

      status = REF2GRB(&minimum,&referenceValueExponent,
                       &referenceValueMantissa,&numberOfBitsPerInteger);
      if( status ) {
        printf("SVALUES: call to REF2GRB failed\n");
        return -1;
      }
      MOVE1BYTE((section4+6),&referenceValueExponent);
      MOVE3BYTES((section4+7),&referenceValueMantissa);

      MOVE1BYTE((section4+10),&numberOfBitsPerPackedValue);

      status = REF2GRB(&arrayOfUnpackedValues[0],&firstCoefficientExponent,
                       &firstCoefficientMantissa,&numberOfBitsPerInteger);
      if( status ) {
        printf("SVALUES: call to REF2GRB failed\n");
        return -1;
      }
      MOVE1BYTE((section4+11),&firstCoefficientExponent);
      MOVE3BYTES((section4+12),&firstCoefficientMantissa);

      numberOfValuesToScale = N - 1;
      arrayOfScaledValues =
        (fortint *) allocateMemory(numberOfValuesToScale*sizeof(fortint));

      INSCAL(&arrayOfUnpackedValues[1],arrayOfScaledValues,
             &numberOfValuesToScale,&minimum,&realScale,
             &numberOfBitsPerPackedValue);

      startOfPackedValueBits = 120;
      lengthOfSection4 = estimatedSizeOfSection4/sizeof(fortint);
      INXBIT((fortint*)section4,&lengthOfSection4,
             &startOfPackedValueBits,arrayOfScaledValues,
             &numberOfValuesToScale,&numberOfBitsPerInteger,
             &numberOfBitsPerPackedValue,"C",&status,(long)1);
      if( status ) {
        printf("SVALUES: call to INXBIT failed\n");
        return -1;
      }
      freeMemory(arrayOfScaledValues);

      lengthOfSection4 = (startOfPackedValueBits + 7)/8;
      if( MOD(lengthOfSection4,2) == 1 ) lengthOfSection4++;
      MOVE3BYTES(section4,&lengthOfSection4);

      numberOfUnusedBits = lengthOfSection4*8 - startOfPackedValueBits;
      numberOfUnusedBits |= 0x80;
      MOVE1BYTE((section4+3),&numberOfUnusedBits);

      freeMemory(g->g4);
      g->g4 = (gribSection4 *) section4;
    }
  }
  else {
    if( DEBUG1 ) printf("SVALUES: gridpoint field\n");
  }
  return 0;
}

void setBitMap(unsigned char * bitmap, fortint bitNumber ) {
fortint byteOffset = (bitNumber>>3);
fortint bitOffset = bitNumber - (byteOffset<<3);

  bitmap[byteOffset] |= (0x80>>bitOffset);
}

void adjustGridAreaDefinition(gribProduct* g, int type) {
fortdouble north, south, west, east, increment;
fortint value, numberOfPoints ;

  switch( type) {

    case NORTH:
      if( DEBUG2 ) printf("adjustGridAreaDefinition: new north given\n");
      north = RGNWLAT(&g);
      if( g->southSet ) {
        south = RGSELAT(&g);
        if( g->northSouthIncrementSet ) {
          increment   = RGDJ(&g);
          numberOfPoints = (fortint) (0.5 + (north-south)/increment) + 1;
          MOVE2BYTES(((g->g2)->grid.latlon.numberOfPointsAlongMeridian),
                     &numberOfPoints);
          g->northSouthNumberOfPointsSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: numberOfPoints set = %d\n",
                    numberOfPoints);
        }
        else if( g->northSouthNumberOfPointsSet ) {
          numberOfPoints = IGNJ(&g) - 1;
          increment = (north-south)/(fortdouble) numberOfPoints;
          value = (fortint) (increment * 1000.0);
          MOVE2BYTES(((g->g2)->grid.latlon.jDirectionIncrement),&value);
          g->northSouthIncrementSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: north-south increment set = %f\n",
                    increment);
        }
      }
      else {
        if( g->northSouthIncrementSet && g->northSouthNumberOfPointsSet ) {
          increment   = RGDJ(&g);
          numberOfPoints = IGNJ(&g) - 1;
          south = north - increment * (fortdouble) numberOfPoints;
          value = (fortint) (south * 1000.0);
          if( value < 0 ) value = 0x800000 | (-value);
          MOVE3BYTES(((g->g2)->grid.latlon.latitudeOfLastPoint),&value);
          g->southSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: south set = %f\n", south);
        }
      }
      break;

    case SOUTH:
      if( DEBUG2 ) printf("adjustGridAreaDefinition: new south given\n");
      south = RGSELAT(&g);
      if( g->northSet ) {
        north = RGNWLAT(&g);
        if( g->northSouthIncrementSet ) {
          increment   = RGDJ(&g);
          numberOfPoints = (fortint) (0.5 + (north-south)/increment) + 1;
          MOVE2BYTES(((g->g2)->grid.latlon.numberOfPointsAlongMeridian),
                     &numberOfPoints);
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: numberOfPoints set = %d\n",
                    numberOfPoints);
        }
        else if( g->northSouthNumberOfPointsSet ) {
          numberOfPoints = IGNJ(&g) - 1;
          increment = (north-south)/(fortdouble) numberOfPoints;
          value = (fortint) (increment * 1000.0);
          MOVE2BYTES(((g->g2)->grid.latlon.jDirectionIncrement),&value);
          g->northSouthIncrementSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: north-south increment set = %f\n",
                    increment);
        }
      }
      else {
        if( g->northSouthIncrementSet && g->northSouthNumberOfPointsSet ) {
          increment   = RGDJ(&g);
          numberOfPoints = IGNJ(&g) - 1;
          north = south + increment * (fortdouble) numberOfPoints;
          value = (fortint) (north * 1000.0);
          if( value < 0 ) value = 0x800000 | (-value);
          MOVE3BYTES(((g->g2)->grid.latlon.latitudeOfFirstPoint),&value);
          g->northSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: north set = %f\n", north);
        }
      }
      break;

    case WEST:
      if( DEBUG2 ) printf("adjustGridAreaDefinition: new west given\n");
      west = RGNWLON(&g);
      if( g->eastSet ) {
        east = RGSELON(&g);
        if( g->westEastIncrementSet ) {
          increment   = RGDI(&g);
          if( ALMOSTZERO(east - west - 360.0) )
            numberOfPoints = (fortint) (0.5 + (east-west)/increment);
          else
            numberOfPoints = (fortint) (0.5 + (east-west)/increment) + 1;
          MOVE2BYTES(((g->g2)->grid.latlon.numberOfPointsAlongParallel),
                     &numberOfPoints);
          g->westEastNumberOfPointsSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: numberOfPoints set = %d\n",
                    numberOfPoints);
        }
        else if( g->westEastNumberOfPointsSet ) {
          numberOfPoints = IGNI(&g) - 1;
/*
//        Test for longitude wraparound
*/
          if( ALMOSTZERO(east - west - 360.0) ) numberOfPoints++;
          increment = (east-west)/(fortdouble) numberOfPoints;
          value = (fortint) (increment * 1000.0);
          MOVE2BYTES(((g->g2)->grid.latlon.iDirectionIncrement),&value);
          g->westEastIncrementSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: west-east increment set = %f\n",
                    increment);
        }
      }
      else {
        if( g->westEastIncrementSet && g->westEastNumberOfPointsSet ) {
          increment   = RGDI(&g);
          numberOfPoints = IGNI(&g) - 1;
          east = west + increment * (fortdouble) numberOfPoints;
          value = (fortint) (east * 1000.0);
          if( value < 0 ) value = 0x800000 | (-value);
          MOVE3BYTES(((g->g2)->grid.latlon.longitudeOfLastPoint),&value);
          g->eastSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: east set = %f\n", east);
        }
      }
      break;

    case EAST:
      if( DEBUG2 ) printf("adjustGridAreaDefinition: new east given\n");
      east = RGSELON(&g);
      if( g->westSet ) {
        west = RGNWLON(&g);
        if( g->westEastIncrementSet ) {
          increment   = RGDI(&g);
          if( ALMOSTZERO(east - west - 360.0) )
            numberOfPoints = (fortint) (0.5 + (east-west)/increment);
          else
            numberOfPoints = (fortint) (0.5 + (east-west)/increment) + 1;
          MOVE2BYTES(((g->g2)->grid.latlon.numberOfPointsAlongParallel),
                     &numberOfPoints);
          g->westEastNumberOfPointsSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: numberOfPoints set = %d\n",
                    numberOfPoints);
        }
        else if( g->westEastNumberOfPointsSet ) {
          numberOfPoints = IGNI(&g) - 1;
/*
//        Test for longitude wraparound
*/
          if( ALMOSTZERO(east - west - 360.0) ) numberOfPoints++;
          increment = (east-west)/(fortdouble) numberOfPoints;
          value = (fortint) (increment * 1000.0);
          MOVE2BYTES(((g->g2)->grid.latlon.iDirectionIncrement),&value);
          g->westEastIncrementSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: west-east increment set = %f\n",
                    increment);
        }
      }
      else {
        if( g->westEastIncrementSet && g->westEastNumberOfPointsSet ) {
          increment   = RGDI(&g);
          numberOfPoints = IGNI(&g);
          west = east - increment * (fortdouble) numberOfPoints;
          value = (fortint) (west * 1000.0);
          if( value < 0 ) value = 0x800000 | (-value);
          MOVE3BYTES(((g->g2)->grid.latlon.longitudeOfFirstPoint),&value);
          g->westSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: west set = %f\n", west);
        }
      }
      break;

    case W_E_INC:
      if( DEBUG2 ) printf("adjustGridAreaDefinition: new west-east increment given\n");
      increment = RGDI(&g);
      if( g->westSet ) {
        west = RGNWLON(&g);
        if( g->eastSet ) {
          east = RGSELON(&g);
          if( ALMOSTZERO(east - west - 360.0) )
            numberOfPoints = (fortint) (0.5 + (east-west)/increment);
          else
            numberOfPoints = (fortint) (0.5 + (east-west)/increment) + 1;
          MOVE2BYTES(((g->g2)->grid.latlon.numberOfPointsAlongParallel),
                     &numberOfPoints);
          g->westEastNumberOfPointsSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: numberOfPoints set = %d\n",
                    numberOfPoints);
        }
        else if( g->westEastNumberOfPointsSet ) {
          numberOfPoints = IGNI(&g);
          east = west + increment *(fortdouble) numberOfPoints;
          value = (fortint) (east * 1000.0);
          if( value < 0 ) value = 0x800000 | (-value);
          MOVE3BYTES(((g->g2)->grid.latlon.longitudeOfLastPoint),&value);
          g->eastSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: east set = %f\n", east);
        }
      }
      else {
        if( g->eastSet && g->westEastNumberOfPointsSet ) {
          east = RGSELON(&g);
          numberOfPoints = IGNI(&g);
          west = east - increment * (fortdouble) numberOfPoints;
          value = (fortint) (west * 1000.0);
          if( value < 0 ) value = 0x800000 | (-value);
          MOVE3BYTES(((g->g2)->grid.latlon.longitudeOfFirstPoint),&value);
          g->westSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: west set = %f\n", west);
        }
      }
      break;

    case N_S_INC:
      if( DEBUG2 ) printf("adjustGridAreaDefinition: new north-south increment given\n");
      increment = RGDJ(&g);
      if( g->northSet ) {
        north = RGNWLAT(&g);
        if( g->southSet ) {
          south = RGSELAT(&g);
          numberOfPoints = (fortint) (0.5 + (north-south)/increment) + 1;
          MOVE2BYTES(((g->g2)->grid.latlon.numberOfPointsAlongMeridian),
                     &numberOfPoints);
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: numberOfPoints set = %d\n",
                    numberOfPoints);
        }
        else if( g->northSouthNumberOfPointsSet ) {
          numberOfPoints = IGNJ(&g) - 1;
          south = north - increment *(fortdouble) numberOfPoints;
          value = (fortint) (south * 1000.0);
          if( value < 0 ) value = 0x800000 | (-value);
          MOVE3BYTES(((g->g2)->grid.latlon.latitudeOfLastPoint),&value);
          g->southSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: south set = %f\n", south);
        }
      }
      else {
        if( g->southSet && g->northSouthNumberOfPointsSet ) {
          south = RGSELAT(&g);
          numberOfPoints = IGNJ(&g);
          north = south + increment *(fortdouble) numberOfPoints;
          value = (fortint) (north * 1000.0);
          if( value < 0 ) value = 0x800000 | (-value);
          MOVE3BYTES(((g->g2)->grid.latlon.latitudeOfFirstPoint),&value);
          g->northSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: north set = %f\n", north);
        }
      }
      break;

    case W_E_PTS:
      if( DEBUG2 ) printf("adjustGridAreaDefinition: new west-east number of points given\n");
      numberOfPoints = IGNI(&g);
      if( g->westSet ) {
        west = RGNWLON(&g);
        if( g->eastSet ) {
          east = RGSELON(&g);
          if( ALMOSTZERO(east - west - 360.0) ) numberOfPoints--;
          increment = (east-west)/(fortdouble)numberOfPoints;
          value = (fortint) (increment * 1000.0);
          MOVE2BYTES(((g->g2)->grid.latlon.iDirectionIncrement),&value);
          g->westEastIncrementSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: west-east increment set = %f\n",
                    increment);
        }
        else if( g->westEastIncrementSet ) {
          increment   = RGDI(&g);
          east = west + increment *(fortdouble) numberOfPoints;
          if( (east - west) > 360.0 ) east -= increment;
          value = (fortint) (east * 1000.0);
          if( value < 0 ) value = 0x800000 | (-value);
          MOVE3BYTES(((g->g2)->grid.latlon.longitudeOfLastPoint),&value);
          g->eastSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: east set = %f\n", east);
        }
      }
      else {
        if( g->eastSet && g->westEastIncrementSet ) {
          east = RGSELON(&g);
          increment   = RGDI(&g);
          west = east - increment *(fortdouble) numberOfPoints;
          if( (east - west) > 360.0 ) east += increment;
          value = (fortint) (west * 1000.0);
          if( value < 0 ) value = 0x800000 | (-value);
          MOVE3BYTES(((g->g2)->grid.latlon.longitudeOfFirstPoint),&value);
          g->westSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: west set = %f\n", west);
        }
      }
      break;

    case N_S_PTS:
      if( DEBUG2 ) printf("adjustGridAreaDefinition: new north-south number of points given\n");
      numberOfPoints = IGNJ(&g) - 1;
      if( g->northSet ) {
        north = RGNWLAT(&g);
        if( g->southSet ) {
          south = RGSELAT(&g);
          increment = (north-south)/(fortdouble)numberOfPoints;
          value = (fortint) (increment * 1000.0);
          MOVE2BYTES(((g->g2)->grid.latlon.jDirectionIncrement),&value);
          g->northSouthIncrementSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: north-south increment set = %f\n", increment);
        }
        else if( g->northSouthIncrementSet ) {
          increment   = RGDJ(&g);
          south = north - increment *(fortdouble) numberOfPoints;
          value = (fortint) (south * 1000.0);
          if( value < 0 ) value = 0x800000 | (-value);
          MOVE3BYTES(((g->g2)->grid.latlon.latitudeOfLastPoint),&value);
          g->southSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: south set = %f\n", south);
        }
      }
      else {
        if( g->southSet && g->northSouthIncrementSet ) {
          south = RGSELAT(&g);
          increment   = RGDJ(&g);
          north = south + increment *(fortdouble) numberOfPoints;
          value = (fortint) (north * 1000.0);
          if( value < 0 ) value = 0x800000 | (-value);
          MOVE3BYTES(((g->g2)->grid.latlon.latitudeOfFirstPoint),&value);
          g->northSet = TRUE;
          if( DEBUG2 )
            printf("adjustGridAreaDefinition: north set = %f\n", north);
        }
      }
      break;

    default:
      printf("adjustGridAreaDefinition: not yet implemented\n");
      break;

  }

  g->numberOfValues = g2_ni(g) * g2_nj(g);
  return;
}

fortint SPV(gribProduct** grib, fortdouble* list, fortint* sizeList) {
gribProduct * g = *grib;
fortint section2Size, requiredMemory, listOffset, numberOfRows;
fortint numberOfNewCoordinates = *sizeList;
gribSection2 * newG2;
fortint numberOfOldCoordinates, number, loop;
unsigned char * nextValue, * newLocation, * newCoordinateLocation;
fortint exponent, mantissa, numberOfBitsPerInteger;
fortdouble nextCoordinate;

/*
// Ensure there is enough memory to accomodate the list of vertical coordinates
*/
  section2Size = g2_length(g);
  requiredMemory = section2Size - g2_NV(g)*4 + numberOfNewCoordinates*4;

  if( requiredMemory > section2Size ) {
    newG2 = (gribSection2 *) allocateMemory(requiredMemory);
    memcpy(newG2,(g->g2),section2Size);
    freeMemory((g->g2));
    (g->g2) = newG2;
  }
  MOVE3BYTES(((g->g2)->sectionLength),&requiredMemory);

  switch( (int) g2_datatype(g) ) {

    case  0:
    case  4:
    case  5:
    case 50:
      listOffset = 32;
      break;

    case  1:
    case  3:
    case  6:
    case  8:
    case 10:
    case 13:
    case 14:
    case 20:
    case 24:
    case 60:
    case 70:
      listOffset = 42;
      break;

    case 30:
    case 34:
    case 80:
      listOffset = 52;
      break;

    case 90:
      listOffset = 44;
      break;

    default:
      printf("SPV: does not handle representation type = %d\n",
             g2_datatype(g));
      exit(1);
  }
/*
// Move the list of the number of points at each latitude if it is a
// reduced grid.
*/
  numberOfRows = g2_nj(g);

  if( !directionIncrementsGiven(g) &&  (numberOfRows != 0) ) {

    numberOfOldCoordinates = g2_NV(g);

    if( numberOfOldCoordinates < numberOfNewCoordinates ) {
      nextValue = numberOfRows*2 +
        (unsigned char*)(g->g2) + g2_PV_PL(g) + numberOfOldCoordinates*4 - 1;
      newLocation = numberOfRows*2 +
        (unsigned char*)(g->g2) + listOffset + numberOfNewCoordinates*4;
      for( loop = numberOfRows; loop >= 0; loop-- ) {
        number = TWOBYTEINT(nextValue);
        nextValue -= 2;
        MOVE2BYTES(newLocation,&number);
        newLocation -= 2;
      }
    }
    else {
{
fortint temp;
temp = g2_PV_PL(g);
printf("********************* g2_PV_PL(g) = %d\n",temp);
}
      nextValue =
        (unsigned char*)(g->g2) + g2_PV_PL(g) + numberOfOldCoordinates*4 - 1;
      newLocation =
        (unsigned char*)(g->g2) + listOffset + numberOfNewCoordinates*4;
      for( loop = 0; loop < numberOfRows; loop++ ) {
        number = TWOBYTEINT(nextValue);
        nextValue += 2;
        MOVE2BYTES(newLocation,&number);
        newLocation += 2;
      }
    }
  }
/*
// Add the vertical coordinates
*/
  newCoordinateLocation = (unsigned char*)(g->g2) + listOffset;
  numberOfBitsPerInteger = sizeof(fortint) * 8;

  for( loop = 0; loop < numberOfNewCoordinates; loop++) {
    nextCoordinate = list[loop];
    if( REF2GRB(&nextCoordinate,&exponent,&mantissa,&numberOfBitsPerInteger) ) {
      printf("SPV: call to REF2GRB failed\n");
      exit(1);
    }
    MOVE1BYTE(newCoordinateLocation,&exponent);
    MOVE3BYTES((newCoordinateLocation+1),&mantissa);
    newCoordinateLocation += 4;
  }
  MOVE1BYTE(((g->g2)->NV),&numberOfNewCoordinates);
  listOffset++;
  MOVE1BYTE(((g->g2)->PV_PL),&listOffset);
  return 0;
}
