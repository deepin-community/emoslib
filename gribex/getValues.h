#ifndef GETVALUES_H
#define GETVALUES_H

#include "gdecode.h"

#ifdef FORTRAN_NO_UNDERSCORE
#define GETINT  getint
#define GETREAL getreal
#else
#define GETINT  getint_
#define GETREAL getreal_
#endif

fortint  getIntegerValue(gribProduct**,unsigned char*);
fortdouble getRealValue(gribProduct**,unsigned char*);
fortint  GETINT(gribProduct**,unsigned char*,long);
fortdouble GETREAL(gribProduct**,unsigned char*,long);

fortint IGNWLAT(gribProduct**);
fortint IGNWLON(gribProduct**);
fortint IGSELAT(gribProduct**);
fortint IGSELON(gribProduct**);
fortint IGDI(gribProduct**);
fortint IGDJ(gribProduct**);
fortint IGIP(gribProduct**);
fortint IGLATRPL(gribProduct**);
fortint IGLONRPL(gribProduct**);
fortint IGROTATN(gribProduct**);
fortint IGLATSPL(gribProduct**);
fortint IGLONSPL(gribProduct**);
fortint IGSFACTR(gribProduct**);
fortint IGREFVAL(gribProduct**);
fortint IGDSCALE(gribProduct**);

fortdouble RGLENGTH(gribProduct**);
fortdouble RGTABLE(gribProduct**);
fortdouble RGCENTRE(gribProduct**);
fortdouble RGPARAM(gribProduct**);
fortdouble RGLEVEL(gribProduct**);
fortdouble RGLEVEL1(gribProduct**);
fortdouble RGLEVEL2(gribProduct**);
fortdouble RGDATE(gribProduct**);
fortdouble RGTIME(gribProduct**);
fortdouble RGSTEP(gribProduct**);
fortdouble RGDEFIN(gribProduct**);
fortdouble RGCLASS(gribProduct**);
fortdouble RGTYPE(gribProduct**);
fortdouble RGSTREAM(gribProduct**);
fortdouble RGEXPVER(gribProduct**);
fortdouble RGNUMPV(gribProduct**);
fortdouble RGREPRES(gribProduct**);
fortdouble RGNI(gribProduct**);
fortdouble RGNJ(gribProduct**);
fortdouble RGRESCOM(gribProduct**);
fortdouble RGNUMBER(gribProduct**);
fortdouble RGSCAN(gribProduct**);
fortdouble RGJ(gribProduct**);
fortdouble RGK(gribProduct**);
fortdouble RGM(gribProduct**);
fortdouble RGREPMOD(gribProduct**);
fortdouble RGTJ(gribProduct**);
fortdouble RGTK(gribProduct**);
fortdouble RGTM(gribProduct**);
fortdouble RGBITSPV(gribProduct**);
fortdouble RGUNUSED(gribProduct**);
fortdouble RGNVALUE(gribProduct**);

typedef fortint (*geti) (gribProduct **);
typedef fortdouble (*getf) (gribProduct **);

typedef struct {
  unsigned char * name;
  geti get;
} despatchI;

typedef struct {
  unsigned char * name;
  getf get;
} despatchR;

despatchI despatchInteger[] = {
  "angleOfRotation",IGROTATN,
  "bottomLayer",GLEVEL2,
  "codeTable",GTABLE,
  "complexPackingScalingFactor",IGIP,
  "dataRepresentationType",GREPRES,
  "date",GDATE,
  "ecmwfClass",GCLASS,
  "ecmwfExperimentVersionNumber",GEXPVER,
  "ecmwfLocalDefinitionNumber",GDEFIN,
  "ecmwfStream",GSTREAM,
  "ecmwfType",GTYPE,
  "gribLength",GLENGTH,
  "iDirectionIncrement",IGDI,
  "jDirectionIncrement",IGDJ,
  "jPentagonalResolution",GJ,
  "kPentagonalResolution",GK,
  "latitudeOfThePoleOfStretching",IGLATSPL,
  "latitudeOfTheSouthernPoleOfRotation",IGLATRPL,
  "longitudeOfThePoleOfStretching",IGLONSPL,
  "longitudeOfTheSouthernPoleOfRotation",IGLONRPL,
  "mPentagonalResolution",GM,
  "northWestLatitude",IGNWLAT,
  "northWestLongitude",IGNWLON,
  "numberOfBitsPerPackedValue",GBITSPV,
  "numberOfFieldValues",GNVALUE,
  "numberOfParallelsBetweenPoleAndEquator",GNUMBER,
  "numberOfPointsAlongMeridian",GNJ,
  "numberOfPointsAlongParallel",GNI,
  "numberOfUnusedBitsAtEndOfSection4",GUNUSED,
  "numberOfVerticalCoordinateParameters",GNUMPV,
  "originatingCentre",GCENTRE,
  "parameter", GPARAM,
  "pressureLevel",GLEVEL,
  "referenceValue",IGREFVAL,
  "representationMode",GREPMOD,
  "resolutionAndComponentsFlag",GRESCOM,
  "scaleFactorUsedToPackFieldValues",IGDSCALE,
  "scanningModeFlag",GSCAN,
  "southEastLatitude",IGSELAT,
  "southEastLongitude",IGSELON,
  "stretchingFactor",IGSFACTR,
  "subsetJPentagonalResolution",GTJ,
  "subsetKPentagonalResolution",GTK,
  "subsetMPentagonalResolution",GTM,
  "time",GTIME,
  "timestep",GSTEP,
  "topLayer",GLEVEL1
};

despatchR despatchReal[] = {
  "angleOfRotation",GROTATN,
  "bottomLayer",RGLEVEL2,
  "codeTable",RGTABLE,
  "complexPackingScalingFactor",GIP,
  "dataRepresentationType",RGREPRES,
  "date",RGDATE,
  "ecmwfClass",RGCLASS,
  "ecmwfExperimentVersionNumber",RGEXPVER,
  "ecmwfLocalDefinitionNumber",RGDEFIN,
  "ecmwfStream",RGSTREAM,
  "ecmwfType",RGTYPE,
  "gribLength",RGLENGTH,
  "iDirectionIncrement",GDI,
  "jDirectionIncrement",GDJ,
  "jPentagonalResolution",RGJ,
  "kPentagonalResolution",RGK,
  "latitudeOfThePoleOfStretching",GLATSPL,
  "latitudeOfTheSouthernPoleOfRotation",GLATRPL,
  "longitudeOfThePoleOfStretching",GLONSPL,
  "longitudeOfTheSouthernPoleOfRotation",GLONRPL,
  "mPentagonalResolution",RGM,
  "northWestLatitude",GNWLAT,
  "northWestLongitude",GNWLON,
  "numberOfBitsPerPackedValue",RGBITSPV,
  "numberOfFieldValues",RGNVALUE,
  "numberOfParallelsBetweenPoleAndEquator",RGNUMBER,
  "numberOfPointsAlongMeridian",RGNJ,
  "numberOfPointsAlongParallel",RGNI,
  "numberOfUnusedBitsAtEndOfSection4",RGUNUSED,
  "numberOfVerticalCoordinateParameters",RGNUMPV,
  "originatingCentre",RGCENTRE,
  "parameter",RGPARAM,
  "pressureLevel",RGLEVEL,
  "referenceValue",GREFVAL,
  "representationMode",RGREPMOD,
  "resolutionAndComponentsFlag",RGRESCOM,
  "scaleFactorUsedToPackFieldValues",GDSCALE,
  "scanningModeFlag",RGSCAN,
  "southEastLatitude",GSELAT,
  "southEastLongitude",GSELON,
  "stretchingFactor",GSFACTR,
  "subsetJPentagonalResolution",RGTJ,
  "subsetKPentagonalResolution",RGTK,
  "subsetMPentagonalResolution",RGTM,
  "time",RGTIME,
  "timestep",RGSTEP,
  "topLayer",RGLEVEL1
};

int binaryChopI(despatchI * list, int listLength, unsigned char * test);
int binaryChopR(despatchR * list, int listLength, unsigned char * test);

#endif /* End of GETVALUES_H */
