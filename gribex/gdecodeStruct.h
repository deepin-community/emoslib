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
#ifndef GDECODESTRUCT_H
#define GDECODESTRUCT_H

typedef struct gribSection0 {
unsigned char GRIB[4];
unsigned char totalGRIBlength[3];
unsigned char editionNumber[1];
} gribSection0;

typedef struct ECMWFdefinition1 {
unsigned char number[1];
unsigned char total[1];
unsigned char spare[1];
} ECMWFdefinition1;

typedef struct ECMWFdefinition2 {
unsigned char number[1];
unsigned char total[1];
unsigned char spare[1];
unsigned char method[1];
unsigned char startTimestep[2];
unsigned char endTimestep[2];
unsigned char domainNorthLatitude[3];
unsigned char domainWestLongitude[3];
unsigned char domainSouthLatitude[3];
unsigned char domainEastLongitude[3];
unsigned char operationalForecastCluster[1];
unsigned char controlForecastCluster[1];
unsigned char numberInCluster[1];
unsigned char forecastNumberList[256];
} ECMWFdefinition2;

typedef struct ECMWFdefinition3 {
unsigned char band[1];
unsigned char functionCode[1];
unsigned char spare[1];
} ECMWFdefinition3;

typedef struct ECMWFdefinition5 {
unsigned char number[1];
unsigned char total[1];
unsigned char unitsDecimalScaleFactor[1];
unsigned char thresholdIndicator[1];
unsigned char lowerThreshold[2];
unsigned char upperThreshold[2];
unsigned char spare[1];
} ECMWFdefinition5;

typedef struct ICEField {
unsigned char date[3];
unsigned char satelliteNumber[1];
} ICEField;

typedef struct ECMWFdefinition6 {
unsigned char zero[2];
unsigned char SSTFieldDate[3];
unsigned char typeOfSSTField[1];
unsigned char countOfICEFields[1];
unsigned char lowerThreshold[2];
unsigned char upperThreshold[2];
ICEField ICEFieldList[20];
} ECMWFdefinition6;

typedef struct ECMWFdefinition7 {
unsigned char number[1];
unsigned char totalNumberOfDiagnostics[1];
unsigned char domain[1];
unsigned char diagnosticNumber[1];
unsigned char spare[1];
} ECMWFdefinition7;

typedef struct ECMWFdefinition8 {
unsigned char interval[1];
unsigned char unsignedIntegers[12];
} ECMWFdefinition8;

typedef struct ECMWFdefinition9 {
unsigned char number[2];
unsigned char numberOfIterations[2];
unsigned char numberOfSingularVectorsComputed[2];
unsigned char initialNorm[1];
unsigned char finalNorm[1];
unsigned char multiplicationFactor[4];
unsigned char northWestLatitude[4];
unsigned char northWestLongitude[4];
unsigned char southEastLatitude[4];
unsigned char southEastLongitude[4];
unsigned char accuracy[4];
unsigned char numberOfSingularVectorsEvolved[2];
unsigned char ritzNumber1[4];
unsigned char ritzNumber2[4];
unsigned char spare[1];
} ECMWFdefinition9;

typedef struct ECMWFdefinition10 {
unsigned char number[1];
unsigned char total[1];
unsigned char centralClusterDefinition[1];
unsigned char parameter[1];
unsigned char levelType[1];
unsigned char domainNorth[3];
unsigned char domainWest[3];
unsigned char domainSouth[3];
unsigned char domainEast[3];
unsigned char operationalForcastTubeNumber[1];
unsigned char controlForcastTubeNumber[1];
unsigned char level[2];
unsigned char referenceStep[2];
unsigned char radiusOfCentralCluster[2];
unsigned char ensembleStandardDeviation[2];
unsigned char distanceOfExtremeToMean[2];
unsigned char numberOfForecastsInTube[1];
unsigned char ensembleForecastList[255];
} ECMWFdefinition10;

typedef struct ECMWFdefinition11 {
unsigned char analysisClass[1];
unsigned char analysisType[1];
unsigned char analysisStream[2];
unsigned char analysisExpver[4];
unsigned char analysisYear[1];
unsigned char analysisMonth[1];
unsigned char analysisDay[1];
unsigned char analysisHour[1];
unsigned char analysisMinute[1];
unsigned char analysisCentury[1];
unsigned char analysisOriginatingCentre[1];
unsigned char analysisSubCentre[1];
unsigned char spare[7];
} ECMWFdefinition11;

typedef struct ECMWFdefinition20 {
unsigned char iteration[1];
unsigned char total[1];
unsigned char spare[1];
} ECMWFdefinition20;

typedef union localContent {
ECMWFdefinition1  def1;
ECMWFdefinition2  def2;
ECMWFdefinition3  def3;
ECMWFdefinition5  def5;
ECMWFdefinition6  def6;
ECMWFdefinition7  def7;
ECMWFdefinition8  def8;
ECMWFdefinition9  def9;
ECMWFdefinition10 def10;
ECMWFdefinition11 def11;
ECMWFdefinition20 def20;
} localContent;

typedef struct marsHeader {
unsigned char definition[1];
unsigned char ecmwfClass[1];
unsigned char type[1];
unsigned char stream[2];
unsigned char experimentVersionNumber[4];
} marsHeader;

typedef struct localDefinition {
marsHeader mars;
localContent contents;
} localDefinition;

typedef struct gribSection1 {
unsigned char sectionLength[3];
unsigned char tableVersionNumber[1];
unsigned char originatingCentre[1];
unsigned char generatingProcessId[1];
unsigned char gridDefinition[1];
unsigned char section2and3PresentFlag[1];
unsigned char parameter[1];
unsigned char typeOfLevel[1];
unsigned char level1[1];
unsigned char level2[1];
unsigned char year[1];
unsigned char month[1];
unsigned char day[1];
unsigned char hour[1];
unsigned char minute[1];
unsigned char unitOfTimeRange[1];
unsigned char P1[1];
unsigned char P2[1];
unsigned char timeRangeIndicator[1];
unsigned char numberInAverage[2];
unsigned char numberMissing[1];
unsigned char century[1];
unsigned char subCentreId[1];
unsigned char unitsDecimalScaleFactor[2];
unsigned char reserved[12];
localDefinition local;
} gribSection1;

typedef struct latitudeLongitudeGrid {
unsigned char numberOfPointsAlongParallel[2];
unsigned char numberOfPointsAlongMeridian[2];
unsigned char latitudeOfFirstPoint[3];
unsigned char longitudeOfFirstPoint[3];
unsigned char resolutionAndComponentsFlag[1];
unsigned char latitudeOfLastPoint[3];
unsigned char longitudeOfLastPoint[3];
unsigned char iDirectionIncrement[2];
unsigned char jDirectionIncrement[2];
unsigned char scanningMode[1];
unsigned char setToZero[4];
unsigned char latitudeOfSouthPole[3];
unsigned char longitudeOfSouthPole[3];
unsigned char angleOfRotationOrStretchingFactor[4];
unsigned char latitudeOfPoleOfStretching[3];
unsigned char longitudeOfPoleOfStretching[3];
unsigned char stretchingFactor[4];
} latitudeLongitudeGrid;

typedef struct gaussianGrid {
unsigned char numberOfPointsAlongParallel[2];
unsigned char numberOfPointsAlongMeridian[2];
unsigned char latitudeOfFirstPoint[3];
unsigned char longitudeOfFirstPoint[3];
unsigned char resolutionAndComponentsFlag[1];
unsigned char latitudeOfLastPoint[3];
unsigned char longitudeOfLastPoint[3];
unsigned char iDirectionIncrement[2];
unsigned char numberOfParallelsBetweenPoleAndEquator[2];
unsigned char scanningMode[1];
unsigned char setToZero[4];
unsigned char latitudeOfSouthPole[3];
unsigned char longitudeOfSouthPole[3];
unsigned char angleOfRotationOrStretchingFactor[4];
unsigned char latitudeOfPoleOfStretching[3];
unsigned char longitudeOfPoleOfStretching[3];
unsigned char stretchingFactor[4];
} gaussianGrid;


typedef struct sphericalHarmonicCoefficients {
unsigned char J[2];
unsigned char K[2];
unsigned char M[2];
unsigned char representationType[1];
unsigned char representationMode[1];
unsigned char setToZero[18];
unsigned char latitudeOfSouthPole[3];
unsigned char longitudeOfSouthPole[3];
unsigned char angleOfRotationOrStretchingFactor[4];
unsigned char latitudeOfPoleOfStretching[3];
unsigned char longitudeOfPoleOfStretching[3];
unsigned char stretchingFactor[4];
} sphericalHarmonicCoefficients;

typedef union gridDefinition {
latitudeLongitudeGrid latlon;
gaussianGrid gaussian;
sphericalHarmonicCoefficients spectral;
} gridDefinition;

typedef struct gribSection2 {
unsigned char sectionLength[3];
unsigned char NV[1];
unsigned char PV_PL[1];
unsigned char dataRepresentationType[1];
gridDefinition grid;
} gribSection2;

typedef struct gribSection3 {
unsigned char sectionLength[3];
unsigned char numberOfUnusedBits[1];
unsigned char tableReference[2];
unsigned char bitmap;
} gribSection3;

typedef struct simplePackingGridPoint {
unsigned char dataBits;
} simplePackingGridPoint;

typedef struct secondOrderPackingGridPoint {
unsigned char n1[2];
unsigned char extendedFlags[1];
unsigned char n2[2];
unsigned char p1[2];
unsigned char p2[2];
unsigned char reserved[1];
unsigned char dataBits;
} secondOrderPackingGridPoint;

typedef struct simplePackingSpectral {
unsigned char realPartOf0_0Coefficient[4];
unsigned char dataBits;
} simplePackingSpectral;

typedef struct complexPackingSpectral {
unsigned char N[2];
unsigned char IP[2];
unsigned char J[1];
unsigned char K[1];
unsigned char M[1];
unsigned char dataBits;
} complexPackingSpectral;

typedef union binaryData {
simplePackingGridPoint simpleGrid;
secondOrderPackingGridPoint complexGrid;
simplePackingSpectral simpleSpectral;
complexPackingSpectral complexSpectral;
} binaryData;

typedef struct gribSection4 {
unsigned char sectionLength[3];
unsigned char flag[1];
unsigned char scaleFactor[2];
unsigned char referenceValue[4];
unsigned char numberOfBitsPerValue[1];
binaryData data;
} gribSection4;

typedef struct gribSection5 {
unsigned char end7777[4];
} gribSection5;

typedef struct latLongValueVector {
fortdouble * latitude;
fortdouble * longitudeIncrement;
fortdouble * gridPointValue;
} latLongValueVector;

typedef struct gribProduct {
gribSection0 * g0;
gribSection1 * g1;
gribSection2 * g2;
gribSection3 * g3;
gribSection4 * g4;
gribSection5 * g5;
fortint currentPointIndex;
fortint numberOfValues;
unsigned char * value;
unsigned char * bitStart;
fortint bitsPerValue;
fortint bitmapped;
fortint nextValueFirstBit;
fortint nextBit;
fortint northSet;
fortint southSet;
fortint westSet;
fortint eastSet;
fortint northSouthIncrementSet;
fortint westEastIncrementSet;
fortint northSouthNumberOfPointsSet;
fortint westEastNumberOfPointsSet;
fortdouble scale;
fortdouble minimum;
fortdouble missingValue;
fortint * latitudeOffsets;
fortdouble * expandedValues;
latLongValueVector currentPoint;
} gribProduct;

#endif /* End of GDECODESTRUCT_H */
