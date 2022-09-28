#ifndef GETVALUES_H
#define GETVALUES_H

#include "gdecode.h"
#include "gdecode1.h"
#include "gdecode2.h"

#include "sencode.h"
#include "sencode1.h"
#include "sencode2.h"

#ifdef FORTRAN_NO_UNDERSCORE
#define GETINT  getint
#define GETREAL getreal
#define SETINT  setint
#define SETREAL setreal
#else
#define GETINT  getint_
#define GETREAL getreal_
#define SETINT  setint_
#define SETREAL setreal_
#endif

fortint    getIntegerValue(gribProduct**,unsigned char*);
fortdouble getRealValue(gribProduct**,unsigned char*);
fortint    GETINT(gribProduct**,unsigned char*,long);
fortdouble GETREAL(gribProduct**,unsigned char*,long);

fortint setIntegerValue(gribProduct**,unsigned char*,fortint*);
fortint setRealValue(gribProduct**,unsigned char*,fortdouble*);
fortint SETINT(gribProduct**,unsigned char*,fortint*,long);
fortint SETREAL(gribProduct**,unsigned char*,fortdouble*,long);

fortint IGNWLAT(gribProduct**);
fortint IGNWLON(gribProduct**);
fortint IGSELAT(gribProduct**);
fortint IGSELON(gribProduct**);
fortint IGDI(gribProduct**);
fortint IGDJ(gribProduct**);
fortint IGIP(gribProduct**);
fortint IGLATRP(gribProduct**);
fortint IGLONRP(gribProduct**);
fortint IGROTAT(gribProduct**);
fortint IGLATSP(gribProduct**);
fortint IGLONSP(gribProduct**);
fortint IGSFACT(gribProduct**);
fortint IGREFVL(gribProduct**);
fortint IGDSCAL(gribProduct**);
fortint IGLEVTY(gribProduct**);
fortint IGTUNIT(gribProduct**);
fortint IGTRIND(gribProduct**);
fortint IGSTEP(gribProduct**);
fortint IGSTEP1(gribProduct**);
fortint IGSTEP2(gribProduct**);
fortint IGNUMAV(gribProduct**);
fortint IGNUMMS(gribProduct**);
fortint IGSUBID(gribProduct**);
fortint IGUDECF(gribProduct**);
fortint IGTYPE(gribProduct**);
fortint IGCLASS(gribProduct**);
fortint IGSTREM(gribProduct**);
fortint IGEXPVR(gribProduct**);
fortint IGDEFIN(gribProduct**);
fortint IGDUMMY(gribProduct**);

fortdouble RGLNGTH(gribProduct**);
fortdouble RGTABLE(gribProduct**);
fortdouble RGCENTR(gribProduct**);
fortdouble RGPARAM(gribProduct**);
fortdouble RGLEVEL(gribProduct**);
fortdouble RGLEVL1(gribProduct**);
fortdouble RGLEVL2(gribProduct**);
fortdouble RGDATE(gribProduct**);
fortdouble RGTIME(gribProduct**);
fortdouble RGSTEP(gribProduct**);
fortdouble RGSTEP1(gribProduct**);
fortdouble RGSTEP2(gribProduct**);
fortdouble RGCLASS(gribProduct**);
fortdouble RGTYPE(gribProduct**);
fortdouble RGSTREM(gribProduct**);
fortdouble RGEXPVR(gribProduct**);
fortdouble RGNUMPV(gribProduct**);
fortdouble RGREPRS(gribProduct**);
fortdouble RGNI(gribProduct**);
fortdouble RGNJ(gribProduct**);
fortdouble RGRESCO(gribProduct**);
fortdouble RGGAUSS(gribProduct**);
fortdouble RGSCANM(gribProduct**);
fortdouble RGJ(gribProduct**);
fortdouble RGK(gribProduct**);
fortdouble RGM(gribProduct**);
fortdouble RGREPMO(gribProduct**);
fortdouble RGTJ(gribProduct**);
fortdouble RGTK(gribProduct**);
fortdouble RGTM(gribProduct**);
fortdouble RGBTSPV(gribProduct**);
fortdouble RGUNUSD(gribProduct**);
fortdouble RGNVALU(gribProduct**);
fortdouble RGLEVTY(gribProduct**);
fortdouble RGTUNIT(gribProduct**);
fortdouble RGTRIND(gribProduct**);
fortdouble RGNUMAV(gribProduct**);
fortdouble RGNUMMS(gribProduct**);
fortdouble RGSUBID(gribProduct**);
fortdouble RGUDECF(gribProduct**);
fortdouble RGTYPE(gribProduct**);
fortdouble RGDEFIN(gribProduct**);
fortdouble RGDUMMY(gribProduct**);

fortint ISDUMMY(gribProduct **, fortint *);
fortint RSDUMMY(gribProduct **, fortdouble *);
fortint RSDATE(gribProduct **, fortdouble *);
fortint ISDATE(gribProduct **, fortint *);
fortint RSTIME(gribProduct **, fortdouble *);
fortint ISTIME(gribProduct **, fortint *);
fortint RSTABLE(gribProduct **, fortdouble *);
fortint ISTABLE(gribProduct **, fortint *);
fortint RSCENTR(gribProduct **, fortdouble *);
fortint ISCENTR(gribProduct **, fortint *);
fortint RSPARAM(gribProduct **, fortdouble *);
fortint ISPARAM(gribProduct **, fortint *);
fortint RSLEVTY(gribProduct **, fortdouble *);
fortint ISLEVTY(gribProduct **, fortint *);
fortint RSLEVEL(gribProduct **, fortdouble *);
fortint ISLEVEL(gribProduct **, fortint *);
fortint RSLEVEL(gribProduct **, fortdouble *);
fortint ISLEVEL(gribProduct **, fortint *);
fortint RSTUNIT(gribProduct **, fortdouble *);
fortint ISTUNIT(gribProduct **, fortint *);
fortint RSTRIND(gribProduct **, fortdouble *);
fortint ISTRIND(gribProduct **, fortint *);
fortint RSSTEP(gribProduct **, fortdouble *);
fortint ISSTEP(gribProduct **, fortint *);
fortint RSSTEP1(gribProduct **, fortdouble *);
fortint ISSTEP1(gribProduct **, fortint *);
fortint RSSTEP2(gribProduct **, fortdouble *);
fortint ISSTEP2(gribProduct **, fortint *);
fortint RSNUMAV(gribProduct **, fortdouble *);
fortint ISNUMAV(gribProduct **, fortint *);
fortint RSNUMMS(gribProduct **, fortdouble *);
fortint ISNUMMS(gribProduct **, fortint *);
fortint RSSUBID(gribProduct **, fortdouble *);
fortint ISSUBID(gribProduct **, fortint *);
fortint RSUDECF(gribProduct **, fortdouble *);
fortint ISUDECF(gribProduct **, fortint *);
fortint RSTYPE(gribProduct **, fortdouble *);
fortint ISTYPE(gribProduct **, fortint *);
fortint RSCLASS(gribProduct **, fortdouble *);
fortint ISCLASS(gribProduct **, fortint *);
fortint RSSTREM(gribProduct **, fortdouble *);
fortint ISSTREM(gribProduct **, fortint *);
fortint RSEXPVR(gribProduct **, fortdouble *);
fortint ISEXPVR(gribProduct **, fortint *);
fortint RSDEFIN(gribProduct **, fortdouble *);
fortint ISDEFIN(gribProduct **, fortint *);
fortint RSBTSPV(gribProduct **, fortdouble *);
fortint ISBTSPV(gribProduct **, fortint *);
fortint RSREPRS(gribProduct **, fortdouble *);
fortint ISREPRS(gribProduct **, fortint *);
fortint RSNWLAT(gribProduct **, fortdouble *);
fortint ISNWLAT(gribProduct **, fortint *);
fortint RSNWLON(gribProduct **, fortdouble *);
fortint ISNWLON(gribProduct **, fortint *);
fortint RSSELAT(gribProduct **, fortdouble *);
fortint ISSELAT(gribProduct **, fortint *);
fortint RSSELON(gribProduct **, fortdouble *);
fortint ISSELON(gribProduct **, fortint *);
fortint RSDIJ(gribProduct **, fortdouble *);
fortint ISDIJ(gribProduct **, fortint *);
fortint RSDI(gribProduct **, fortdouble *);
fortint ISDI(gribProduct **, fortint *);
fortint RSDJ(gribProduct **, fortdouble *);
fortint ISDJ(gribProduct **, fortint *);
fortint RSNI(gribProduct **, fortdouble *);
fortint ISNI(gribProduct **, fortint *);
fortint RSNJ(gribProduct **, fortdouble *);
fortint ISNJ(gribProduct **, fortint *);
fortint RSJ(gribProduct **, fortdouble *);
fortint ISJ(gribProduct **, fortint *);
fortint RSK(gribProduct **, fortdouble *);
fortint ISK(gribProduct **, fortint *);
fortint RSM(gribProduct **, fortdouble *);
fortint ISM(gribProduct **, fortint *);
fortint RSJKM(gribProduct **, fortdouble *);
fortint ISJKM(gribProduct **, fortint *);
fortint RSTJ(gribProduct **, fortdouble *);
fortint ISTJ(gribProduct **, fortint *);
fortint RSTK(gribProduct **, fortdouble *);
fortint ISTK(gribProduct **, fortint *);
fortint RSTM(gribProduct **, fortdouble *);
fortint ISTM(gribProduct **, fortint *);
fortint RSTJKM(gribProduct **, fortdouble *);
fortint ISTJKM(gribProduct **, fortint *);
fortint RSLATRP(gribProduct **, fortdouble *);
fortint ISLATRP(gribProduct **, fortint *);
fortint RSLONRP(gribProduct **, fortdouble *);
fortint ISLONRP(gribProduct **, fortint *);
fortint RSROTAT(gribProduct **, fortdouble *);
fortint ISROTAT(gribProduct **, fortint *);
fortint RSLATSP(gribProduct **, fortdouble *);
fortint ISLATSP(gribProduct **, fortint *);
fortint RSLONSP(gribProduct **, fortdouble *);
fortint ISLONSP(gribProduct **, fortint *);
fortint RSSFACT(gribProduct **, fortdouble *);
fortint ISSFACT(gribProduct **, fortint *);
fortint RSSETQG(gribProduct **, fortdouble *);
fortint ISSETQG(gribProduct **, fortint *);
fortint RSSETRG(gribProduct **, fortdouble *);
fortint ISSETRG(gribProduct **, fortint *);

fortint SPV(gribProduct **, fortdouble *, fortint *);

typedef fortint (*geti) (gribProduct **);
typedef fortdouble (*getf) (gribProduct **);
typedef fortint (*seti) (gribProduct **, fortint *);
typedef fortint (*setf) (gribProduct **, fortdouble *);

typedef struct {
  const char * name;
  geti get;
  seti set;
} despatchI;

typedef struct {
  const char * name;
  getf get;
  setf set;
} despatchR;

despatchI despatchInteger[] = {
  "angleOfRotation",IGROTAT,ISROTAT,
  "bottomLayer",IGLEVL2,ISDUMMY,
  "codeTable",IGTABLE,ISTABLE,
  "complexPackingScalingFactor",IGIP,ISDUMMY,
  "dataRepresentationType",IGREPRS,ISREPRS,
  "date",IGDATE,ISDATE,
  "definition1Number",IG1NUMB,IS1NUMB,
  "definition1Total",IG1TOTL,IS1TOTL,
  "definition2ControlForecastCluster",IG2CFCL,IS2CFCL,
  "definition2DomainEast",IG2ELON,IS2ELON,
  "definition2DomainNorth",IG2NLAT,IS2NLAT,
  "definition2DomainSouth",IG2SLAT,IS2SLAT,
  "definition2DomainWest",IG2WLON,IS2WLON,
  "definition2EndStep",IG2END,IS2END,
  "definition2Method",IG2METH,IS2METH,
  "definition2Number",IG2NUMB,IS2NUMB,
  "definition2NumberInCluster",IG2NUCL,IS2NUCL,
  "definition2OperationalForecastCluster",IG2OPCL,IS2OPCL,
  "definition2StartStep",IG2STAR,IS2STAR,
  "definition2Total",IG2TOTL,IS2TOTL,
  "ecmwfClass",IGCLASS,ISCLASS,
  "ecmwfExperimentVersionNumber",IGEXPVR,ISEXPVR,
  "ecmwfLocalDefinitionNumber",IGDEFIN,ISDEFIN,
  "ecmwfStream",IGSTREM,ISSTREM,
  "ecmwfType",IGTYPE,ISTYPE,
  "gribLength",IGLNGTH,ISDUMMY,
  "iAndjHaveTheSameIncrement",IGDUMMY,ISDIJ,
  "iDirectionIncrement",IGDI,ISDI,
  "jDirectionIncrement",IGDJ,ISDJ,
  "jPentagonalResolution",IGJ,ISJ,
  "jkmHaveSamePentagonalResolution",IGDUMMY,ISJKM,
  "kPentagonalResolution",IGK,ISK,
  "latitudeOfThePoleOfStretching",IGLATSP,ISLATSP,
  "latitudeOfTheSouthernPoleOfRotation",IGLATRP,ISLATRP,
  "longitudeOfThePoleOfStretching",IGLONSP,ISLONSP,
  "longitudeOfTheSouthernPoleOfRotation",IGLONRP,ISLONRP,
  "mPentagonalResolution",IGM,ISM,
  "northWestLatitude",IGNWLAT,ISNWLAT,
  "northWestLongitude",IGNWLON,ISNWLON,
  "numberOfBitsPerPackedValue",IGBTSPV,ISBTSPV,
  "numberOfFieldValues",IGNVALU,ISDUMMY,
  "numberOfParallelsBetweenPoleAndEquator",IGGAUSS,ISDUMMY,
  "numberOfPointsAlongMeridian",IGNJ,ISNJ,
  "numberOfPointsAlongParallel",IGNI,ISNI,
  "numberOfProductsInAverage",IGNUMAV,ISNUMAV,
  "numberOfProductsMissingFromAverage",IGNUMMS,ISNUMMS,
  "numberOfUnusedBitsAtEndOfSection4",IGUNUSD,ISDUMMY,
  "numberOfVerticalCoordinateParameters",IGNUMPV,ISDUMMY,
  "originatingCentre",IGCENTR,ISCENTR,
  "parameter",IGPARAM,ISPARAM,
  "pressureLevel",IGLEVEL,ISLEVEL,
  "referenceValue",IGREFVL,ISDUMMY,
  "representationMode",IGREPMO,ISDUMMY,
  "resolutionAndComponentsFlag",IGRESCO,ISDUMMY,
  "scaleFactorUsedToPackFieldValues",IGDSCAL,ISDUMMY,
  "scanningModeFlag",IGSCANM,ISDUMMY,
  "setReducedGaussianGridNumber",IGDUMMY,ISSETQG,
  "setRegularGaussianGridNumber",IGDUMMY,ISSETRG,
  "southEastLatitude",IGSELAT,ISSELAT,
  "southEastLongitude",IGSELON,ISSELON,
  "stretchingFactor",IGSFACT,ISSFACT,
  "subcentreIdentifier",IGSUBID,ISSUBID,
  "subsetJKMHaveSamePentagonalResolution",IGDUMMY,ISTJKM,
  "subsetJPentagonalResolution",IGTJ,ISTJ,
  "subsetKPentagonalResolution",IGTK,ISTK,
  "subsetMPentagonalResolution",IGTM,ISTM,
  "time",IGTIME,ISTIME,
  "timeRangeIndicator",IGTRIND,ISTRIND,
  "timeStep",IGSTEP,ISSTEP,
  "timeStepP1",IGSTEP1,ISSTEP1,
  "timeStepP2",IGSTEP2,ISSTEP2,
  "timeUnit",IGTUNIT,ISTUNIT,
  "topLayer",IGLEVL1,ISDUMMY,
  "typeOfLevel",IGLEVTY,ISLEVTY,
  "unitsDecimalScaleFactor",IGUDECF,ISUDECF
};

despatchR despatchReal[] = {
  "angleOfRotation",RGROTAT,RSROTAT,
  "bottomLayer",RGLEVL2,RSDUMMY,
  "codeTable",RGTABLE,RSTABLE,
  "complexPackingScalingFactor",RGIP,RSDUMMY,
  "dataRepresentationType",RGREPRS,RSREPRS,
  "date",RGDATE,RSDATE,
  "definition1Number",RG1NUMB,RS1NUMB,
  "definition1Total",RG1TOTL,RS1TOTL,
  "definition2ControlForecastCluster",RG2CFCL,RS2CFCL,
  "definition2DomainEast",RG2ELON,RS2ELON,
  "definition2DomainNorth",RG2NLAT,RS2NLAT,
  "definition2DomainSouth",RG2SLAT,RS2SLAT,
  "definition2DomainWest",RG2WLON,RS2WLON,
  "definition2EndStep",RG2END,RS2END,
  "definition2Method",RG2METH,RS2METH,
  "definition2Number",RG2NUMB,RS2NUMB,
  "definition2NumberInCluster",RG2NUCL,RS2NUCL,
  "definition2OperationalForecastCluster",RG2OPCL,RS2OPCL,
  "definition2StartStep",RG2STAR,RS2STAR,
  "definition2Total",RG2TOTL,RS2TOTL,
  "ecmwfClass",RGCLASS,RSCLASS,
  "ecmwfExperimentVersionNumber",RGEXPVR,RSEXPVR,
  "ecmwfLocalDefinitionNumber",RGDEFIN,RSDEFIN,
  "ecmwfStream",RGSTREM,RSSTREM,
  "ecmwfType",RGTYPE,RSTYPE,
  "gribLength",RGLNGTH,RSDUMMY,
  "iAndjHaveTheSameIncrement",RGDUMMY,RSDIJ,
  "iDirectionIncrement",RGDI,RSDI,
  "jDirectionIncrement",RGDJ,RSDJ,
  "jPentagonalResolution",RGJ,RSJ,
  "jkmHaveSamePentagonalResolution",RGDUMMY,RSJKM,
  "kPentagonalResolution",RGK,RSK,
  "latitudeOfThePoleOfStretching",RGLATSP,RSLATSP,
  "latitudeOfTheSouthernPoleOfRotation",RGLATRP,RSLATRP,
  "longitudeOfThePoleOfStretching",RGLONSP,RSLONSP,
  "longitudeOfTheSouthernPoleOfRotation",RGLONRP,RSLONRP,
  "mPentagonalResolution",RGM,RSM,
  "northWestLatitude",RGNWLAT,RSNWLAT,
  "northWestLongitude",RGNWLON,RSNWLON,
  "numberOfBitsPerPackedValue",RGBTSPV,RSBTSPV,
  "numberOfFieldValues",RGNVALU,RSDUMMY,
  "numberOfParallelsBetweenPoleAndEquator",RGGAUSS,RSDUMMY,
  "numberOfPointsAlongMeridian",RGNJ,RSNJ,
  "numberOfPointsAlongParallel",RGNI,RSNI,
  "numberOfProductsInAverage",RGNUMAV,RSNUMAV,
  "numberOfProductsMissingFromAverage",RGNUMMS,RSNUMMS,
  "numberOfUnusedBitsAtEndOfSection4",RGUNUSD,RSDUMMY,
  "numberOfVerticalCoordinateParameters",RGNUMPV,RSDUMMY,
  "originatingCentre",RGCENTR,RSCENTR,
  "parameter",RGPARAM,RSPARAM,
  "pressureLevel",RGLEVEL,RSLEVEL,
  "referenceValue",RGREFVL,RSDUMMY,
  "representationMode",RGREPMO,RSDUMMY,
  "resolutionAndComponentsFlag",RGRESCO,RSDUMMY,
  "scaleFactorUsedToPackFieldValues",RGDSCAL,RSDUMMY,
  "scanningModeFlag",RGSCANM,RSDUMMY,
  "setReducedGaussianGridNumber",RGDUMMY,RSSETQG,
  "setRegularGaussianGridNumber",RGDUMMY,RSSETRG,
  "southEastLatitude",RGSELAT,RSSELAT,
  "southEastLongitude",RGSELON,RSSELON,
  "stretchingFactor",RGSFACT,RSSFACT,
  "subcentreIdentifier",RGSUBID,RSSUBID,
  "subsetJKMHaveSamePentagonalResolution",RGDUMMY,RSTJKM,
  "subsetJPentagonalResolution",RGTJ,RSTJ,
  "subsetKPentagonalResolution",RGTK,RSTK,
  "subsetMPentagonalResolution",RGTM,RSTM,
  "time",RGTIME,RSTIME,
  "timeRangeIndicator",RGTRIND,RSTRIND,
  "timeStep",RGSTEP,RSSTEP,
  "timeStepP1",RGSTEP1,RSSTEP1,
  "timeStepP2",RGSTEP2,RSSTEP2,
  "timeUnit",RGTUNIT,RSTUNIT,
  "topLayer",RGLEVL1,RSDUMMY,
  "typeOfLevel",RGLEVTY,RSLEVTY,
  "unitsDecimalScaleFactor",RGUDECF,RSUDECF
};

int binaryChopI(despatchI * list, int listLength, unsigned char * test);
int binaryChopR(despatchR * list, int listLength, unsigned char * test);

#endif /* End of GETVALUES_H */
