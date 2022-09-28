#ifndef GDECODE_H
#define GDECODE_H
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "common/fortint.h"
#include "common/fortreal.h"
#include "gdecodeStruct.h"
#include "sencode1.h"

#ifdef FORTRAN_NO_UNDERSCORE
#define IGGLAT  igglat
#define GBYTE   gbyte
#define GDECODE gdecode
#define IGLNGTH iglngth
#define IGTABLE igtable
#define IGCENTR igcentr
#define IGPARAM igparam
#define IGLEVEL iglevel
#define IGLEVL1 iglevl1
#define IGLEVL2 iglevl2
#define IGDATE  igdate
#define IGTIME  igtime
#define IGSTEP  igstep
#define IGSTEP1 igstep1
#define IGSTEP2 igstep2
#define IGDEFIN igdefin
#define IGCLASS igclass
#define IGTYPE  igtype
#define IGSTREM igstrem
#define IGEXPVR igexpvr
#define IGNUMPV ignumpv
#define GPV     gpv
#define GPL     gpl
#define IGREPRS igreprs
#define IGNI    igni
#define IGNJ    ignj
#define RGNWLAT rgnwlat
#define RGNWLON rgnwlon
#define IGRESCO igresco
#define RGSELAT rgselat
#define RGSELON rgselon
#define RGDI    rgdi
#define RGDJ    rgdj
#define IGGAUSS iggauss
#define IGSCANM igscanm
#define IGJ     igj
#define IGK     igk
#define IGM     igm
#define IGREPMO igrepmo
#define RGIP    rgip
#define IGTJ    igtj
#define IGTK    igtk
#define IGTM    igtm
#define RGLATRP rglatrp
#define RGLONRP rglonrp
#define RGROTAT rgrotat
#define RGLATSP rglatsp
#define RGLONSP rglonsp
#define RGSFACT rgsfact
#define RGREFVL rgrefvl
#define IGBTSPV igbtspv
#define RGDSCAL rgdscal
#define IGUNUSD igunusd
#define IGNVALU ignvalu
#define GVALUES gvalues
#define GVINIT  gvinit
#define GVECTOR gvector
#define GVEND   gvend
#define IGLEVTY iglevty
#define RGLEVTY rglevty
#define IGNUMAV ignumav
#define RGNUMAV rgnumav
#define IGNUMMS ignumms
#define RGNUMMS rgnumms
#define IGSUBID igsubid
#define RGSUBID rgsubid
#else
#define IGGLAT  igglat_
#define GBYTE   gbyte_
#define GDECODE gdecode_
#define IGLNGTH iglngth_
#define IGTABLE igtable_
#define IGCENTR igcentr_
#define IGPARAM igparam_
#define IGLEVEL iglevel
#define IGLEVL1 iglevl1
#define IGLEVL2 iglevl2
#define IGDATE  igdate_
#define IGTIME  igtime_
#define IGSTEP  igstep_
#define IGSTEP1 igstep1_
#define IGSTEP2 igstep2_
#define IGDEFIN igdefin_
#define IGCLASS igclass_
#define IGTYPE  igtype_
#define IGSTREM igstrem_
#define IGEXPVR igexpvr_
#define IGNUMPV ignumpv_
#define GPV     gpv_
#define GPL     gpl_
#define IGREPRS igreprs_
#define IGNI    igni_
#define IGNJ    ignj_
#define RGNWLAT rgnwlat_
#define RGNWLON rgnwlon_
#define IGRESCO igresco_
#define RGSELAT rgselat_
#define RGSELON rgselon_
#define RGDI    rgdi_
#define RGDJ    rgdj_
#define IGGAUSS iggauss_
#define IGSCANM igscanm_
#define IGJ     igj_
#define IGK     igk_
#define IGM     igm_
#define IGREPMO igrepmo_
#define RGIP    rgip_
#define IGTJ    igtj_
#define IGTK    igtk_
#define IGTM    igtm_
#define RGLATRP rglatrp_
#define RGLONRP rglonrp_
#define RGROTAT rgrotat_
#define RGLATSP rglatsp_
#define RGLONSP rglonsp_
#define RGSFACT rgsfact_
#define RGREFVL rgrefvl_
#define IGBTSPV igbtspv_
#define RGDSCAL rgdscal_
#define IGUNUSD igunusd_
#define IGNVALU ignvalu_
#define GVALUES gvalues_
#define GVINIT  gvinit_
#define GVECTOR gvector_
#define GVEND   gvend_
#define IGLEVTY iglevty_
#define RGLEVTY rglevty_
#define IGNUMAV ignumav_
#define RGNUMAV rgnumav_
#define IGNUMMS ignumms_
#define RGNUMMS rgnumms_
#define IGSUBID igsubid_
#define RGSUBID rgsubid_
#endif

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

#define MOD(a,b) ((a) - ((a)/(b))*(b))
#define MULTIPLE(a,b) ( MOD(a,b) == 0 )

#define ONEBYTEINT(a)   (fortint) ( *(a) )
#define TWOBYTEINT(a)   (fortint) ( (*(a))<<8 | (*((a)+1))<<0 )
#define THREEBYTEINT(a) (fortint) (TWOBYTEINT((a))<<8 | (*((a)+2))<<0 )
#define FOURBYTEINT(a)  (fortint) (THREEBYTEINT((a))<<8 | (*((a)+3))<<0 )
/*
// Section 0
*/
#define g0_length(a)  THREEBYTEINT((a->g0)->totalGRIBlength)
#define g0_edition(a) ONEBYTEINT((a->g0)->editionNumber)
/*
// Section 1
*/
#define g1_length(a)      THREEBYTEINT((a->g1)->sectionLength)
#define g1_table(a)       ONEBYTEINT((a->g1)->tableVersionNumber)
#define g1_centre(a)      ONEBYTEINT((a->g1)->originatingCentre)
#define g1_process(a)     ONEBYTEINT((a->g1)->generatingProcessId)
#define g1_grid(a)        ONEBYTEINT((a->g1)->gridDefinition)
#define g1_flag(a)        ONEBYTEINT((a->g1)->section2and3PresentFlag)
#define g1_parameter(a)   ONEBYTEINT((a->g1)->parameter)
#define g1_typeOfLevel(a) ONEBYTEINT((a->g1)->typeOfLevel)
#define g1_level1(a)      ONEBYTEINT((a->g1)->level1)
#define g1_level2(a)      ONEBYTEINT((a->g1)->level2)
#define g1_year(a)        ONEBYTEINT((a->g1)->year)
#define g1_month(a)       ONEBYTEINT((a->g1)->month)
#define g1_day(a)         ONEBYTEINT((a->g1)->day)
#define g1_hour(a)        ONEBYTEINT((a->g1)->hour)
#define g1_minute(a)      ONEBYTEINT((a->g1)->minute)
#define g1_timeunit(a)    ONEBYTEINT((a->g1)->unitOfTimeRange)
#define g1_P1(a)          ONEBYTEINT((a->g1)->P1)
#define g1_P2(a)          ONEBYTEINT((a->g1)->P2)
#define g1_timerange(a)   ONEBYTEINT((a->g1)->timeRangeIndicator)
#define g1_number(a)      TWOBYTEINT((a->g1)->numberInAverage)
#define g1_missing(a)     ONEBYTEINT((a->g1)->numberMissing)
#define g1_century(a)     ONEBYTEINT((a->g1)->century)
#define g1_subcentre(a)   ONEBYTEINT((a->g1)->subCentreId)
#define g1_scale(a)       TWOBYTEINT((a->g1)->unitsDecimalScaleFactor)
#define g1_definition(a)  ONEBYTEINT((a->g1)->local.mars.definition)
#define g1_class(a)       ONEBYTEINT((a->g1)->local.mars.ecmwfClass)
#define g1_type(a)        ONEBYTEINT((a->g1)->local.mars.type)
#define g1_stream(a)      TWOBYTEINT((a->g1)->local.mars.stream)
#define g1_expver(a)      FOURBYTEINT((a->g1)->local.mars.experimentVersionNumber)

#define centreUsingECMWFLocalDefinition(a) \
            ((g1_subcentre(a)==98)&&(g1_length(a)>=40))

#define ecmwfLocalDefinitionPresent(a) ((g1_centre(a)==98)&&(g1_length(a)>=40))

#define ecmwfLocalDefinition1Present(a) \
                    ( ecmwfLocalDefinitionPresent(a) && (g1_definition(a)==1) )
#define ecmwfLocalDefinition2Present(a) \
                    ( ecmwfLocalDefinitionPresent(a) && (g1_definition(a)==2) )

#define g1_1_number(a) ONEBYTEINT((a->g1)->local.contents.def1.number)
#define g1_1_total(a)  ONEBYTEINT((a->g1)->local.contents.def1.total)

#define g1_2_number(a)  ONEBYTEINT((a->g1)->local.contents.def2.number)
#define g1_2_total(a)   ONEBYTEINT((a->g1)->local.contents.def2.total)
#define g1_2_method(a)  ONEBYTEINT((a->g1)->local.contents.def2.method)
#define g1_2_start(a)   TWOBYTEINT((a->g1)->local.contents.def2.startTimestep)
#define g1_2_end(a)     TWOBYTEINT((a->g1)->local.contents.def2.endTimestep)
#define g1_2_control(a) ONEBYTEINT((a->g1)->local.contents.def2.controlForecastCluster)
#define g1_2_operational(a)   ONEBYTEINT((a->g1)->local.contents.def2.operationalForecastCluster)
#define g1_2_count(a)   ONEBYTEINT((a->g1)->local.contents.def2.numberInCluster)
#define g1_2_domainN(a) THREEBYTEINT((a->g1)->local.contents.def2.domainNorthLatitude)
#define g1_2_domainW(a) THREEBYTEINT((a->g1)->local.contents.def2.domainWestLongitude)
#define g1_2_domainS(a) THREEBYTEINT((a->g1)->local.contents.def2.domainSouthLatitude)
#define g1_2_domainE(a) THREEBYTEINT((a->g1)->local.contents.def2.domainEastLongitude)
/*
// Section 2
*/
#define g2_length(a)           THREEBYTEINT((a->g2)->sectionLength)
#define g2_NV(a)               ONEBYTEINT((a->g2)->NV)
#define g2_PV_PL(a)            ONEBYTEINT((a->g2)->PV_PL)
#define g2_datatype(a)         ONEBYTEINT((a->g2)->dataRepresentationType)
#define g2_ni(a)               TWOBYTEINT((a->g2)->grid.latlon.numberOfPointsAlongParallel)
#define g2_nj(a)               TWOBYTEINT((a->g2)->grid.latlon.numberOfPointsAlongMeridian)
#define g2_firstLat(a)         THREEBYTEINT((a->g2)->grid.latlon.latitudeOfFirstPoint)
#define g2_firstLon(a)         THREEBYTEINT((a->g2)->grid.latlon.longitudeOfFirstPoint)
#define g2_lastLat(a)          THREEBYTEINT((a->g2)->grid.latlon.latitudeOfLastPoint)
#define g2_lastLon(a)          THREEBYTEINT((a->g2)->grid.latlon.longitudeOfLastPoint)
#define g2_resAndComp(a)       ONEBYTEINT((a->g2)->grid.latlon.resolutionAndComponentsFlag)
#define g2_scan(a)             ONEBYTEINT((a->g2)->grid.latlon.scanningMode)
#define g2_di(a)               TWOBYTEINT((a->g2)->grid.latlon.iDirectionIncrement)
#define g2_dj(a)               TWOBYTEINT((a->g2)->grid.latlon.jDirectionIncrement)
#define g2_gaussNumber(a)      TWOBYTEINT((a->g2)->grid.gaussian.numberOfParallelsBetweenPoleAndEquator)
#define g2_latSouthPole(a)     THREEBYTEINT((a->g2)->grid.latlon.latitudeOfSouthPole)
#define g2_lonSouthPole(a)     THREEBYTEINT((a->g2)->grid.latlon.longitudeOfSouthPole)
#define g2_rotOrStretch(a)     FOURBYTEINT((a->g2)->grid.latlon.angleOfRotationOrStretchingFactor)
#define g2_latStretching(a)    THREEBYTEINT((a->g2)->grid.latlon.latitudeOfPoleOfStretching)
#define g2_lonStretching(a)    THREEBYTEINT((a->g2)->grid.latlon.longitudeOfPoleOfStretching)
#define g2_stretchingFactor(a) FOURBYTEINT((a->g2)->grid.latlon.stretchingFactor)
#define g2_J(a)                TWOBYTEINT((a->g2)->grid.spectral.J)
#define g2_K(a)                TWOBYTEINT((a->g2)->grid.spectral.K)
#define g2_M(a)                TWOBYTEINT((a->g2)->grid.spectral.M)
#define g2_repmode(a)          ONEBYTEINT((a->g2)->grid.spectral.representationMode)

#define basicSpectralField(a)               (g2_datatype(a)==50)
#define rotatedSpectralField(a)             (g2_datatype(a)==60)
#define stretchedSpectralField(a)           (g2_datatype(a)==70)
#define rotatedAndStretchedSpectralField(a) (g2_datatype(a)==80)
#define anySpectralField(a) (basicSpectralField(a)||\
                             rotatedSpectralField(a)||\
                             stretchedSpectralField(a)||\
                             rotatedAndStretchedSpectralField(a))

#define basicGaussianGrid(a)               (g2_datatype(a)== 4)
#define rotatedGaussianGrid(a)             (g2_datatype(a)==14)
#define stretchedGaussianGrid(a)           (g2_datatype(a)==24)
#define rotatedAndStretchedGaussianGrid(a) (g2_datatype(a)==34)
#define anyGaussianGrid(a) (basicGaussianGrid(a)||\
                            rotatedGaussianGrid(a)||\
                            stretchedGaussianGrid(a)||\
                            rotatedAndStretchedGaussianGrid(a))

#define basicLatLonGrid(a)               (g2_datatype(a)== 0)
#define rotatedLatLonGrid(a)             (g2_datatype(a)==10)
#define stretchedLatLonGrid(a)           (g2_datatype(a)==20)
#define rotatedAndStretchedLatLonGrid(a) (g2_datatype(a)==30)
#define anyLatLonGrid(a) (basicLatLonGrid(a)||\
                          rotatedLatLonGrid(a)||\
                          stretchedLatLonGrid(a)||\
                          rotatedAndStretchedLatLonGrid(a))

#define generalLatLonGrid(a) (anyGaussianGrid(a)||anyLatLonGrid(a))
#define generalRotatedGrid(a) (rotatedLatLonGrid(a)||\
                               rotatedAndStretchedLatLonGrid(a)||\
                               rotatedGaussianGrid(a)||\
                               rotatedAndStretchedGaussianGrid(a)||\
                               rotatedSpectralField(a)||\
                               rotatedAndStretchedSpectralField(a))
#define generalStretchedGrid(a) (stretchedLatLonGrid(a)||\
                                 rotatedAndStretchedLatLonGrid(a)||\
                                 stretchedGaussianGrid(a)||\
                                 rotatedAndStretchedSpectralField(a)||\
                                 stretchedSpectralField(a)||\
                                 rotatedAndStretchedGaussianGrid(a))
#define generalStretchedAndRotatedGrid(a) (rotatedAndStretchedLatLonGrid(a)||\
                                           rotatedAndStretchedGaussianGrid(a))
#define directionIncrementsGiven(a) ((g2_resAndComp(a)&0x80)&&(g2_di(a)!=0xffff))
/*
// Section 3
*/
#define g3_length(a)             THREEBYTEINT((a->g3)->sectionLength)
#define g3_table(a)              ONEEBYTEINT((a->g3)->tableReference)
#define primaryBitmapPresent(a)  ( g1_flag(a) & 0x40 )
/*
// Section 4
*/
#define g4_length(a)        THREEBYTEINT((a->g4)->sectionLength)
#define g4_flag(a)          ONEBYTEINT((a->g4)->flag)
#define g4_scale(a)         TWOBYTEINT((a->g4)->scaleFactor)
#define g4_reference(a)     FOURBYTEINT((a->g4)->referenceValue)
#define g4_bits(a)          ONEBYTEINT((a->g4)->numberOfBitsPerValue)
#define g4_n(a)             TWOBYTEINT((a->g4)->data.complexSpectral.N)
#define g4_ip(a)            TWOBYTEINT((a->g4)->data.complexSpectral.IP)
#define g4_j(a)             ONEBYTEINT((a->g4)->data.complexSpectral.J)
#define g4_k(a)             ONEBYTEINT((a->g4)->data.complexSpectral.K)
#define g4_m(a)             ONEBYTEINT((a->g4)->data.complexSpectral.M)
#define g4_n1(a)            TWOBYTEINT((a->g4)->data.complexGrid.n1)
#define g4_extendedFlags(a) ONEBYTEINT((a->g4)->data.complexGrid.extendedFlags)
#define g4_n2(a)            TWOBYTEINT((a->g4)->data.complexGrid.n2)
#define g4_p1(a)            TWOBYTEINT((a->g4)->data.complexGrid.p1)
#define g4_p2(a)            TWOBYTEINT((a->g4)->data.complexGrid.p2)

#define gridPoint(a)                      ( (g4_flag(a)&0x80) == 0 )
#define simplePacking(a)                  ( (g4_flag(a)&0x40) == 0 )
#define floatingPoint(a)                  ( (g4_flag(a)&0x20) == 0 )
#define noAdditionalFlags(a)              ( (g4_flag(a)&0x10) == 0 )
#define matrixAtGridPoint(a)              (g4_extendedFlags(a)&0x40)
#define secondaryBitmapPresent(a)         (g4_extendedFlags(a)&0x20)
#define differentWidths(a)                (g4_extendedFlags(a)&0x10)
#define generalExtendedSecondOrder(a)     (g4_extendedFlags(a)&0x08)
#define boustrophedonicOrdering(a)        (g4_extendedFlags(a)&0x04)
#define firstOrderSpatialDifferencing(a)  ( (g4_extendedFlags(a)&0x03) == 1 )
#define secondOrderSpatialDifferencing(a) ( (g4_extendedFlags(a)&0x03) == 2 )
#define thirdOrderSpatialDifferencing(a)  ( (g4_extendedFlags(a)&0x03) == 3 )

#define anyComplexPackedSpectralField(a) \
                                      (anySpectralField(a) && !simplePacking(a))

#ifdef __cplusplus
extern "C" {
#endif
/*
// Function prototypes
*/
void GBYTE(void*,void*,fortint*,fortint*);
fortint IGGLAT(fortint*,fortdouble*,fortint*,fortint*);
double pow(double,double);

fortint findSectionOffsets(unsigned char*,fortint*,fortint*,fortint*,fortint*,fortint*,fortint*);
fortint prepareGrib(gribProduct**,unsigned char*);
fortint convertGRIBFloatToIEEE(unsigned char*);
fortdouble realValue(unsigned char*);
fortdouble referenceValue(gribProduct*);
fortdouble RGREFVL(gribProduct**);
fortint IGBTSPV(gribProduct**);
fortdouble RGDSCAL(gribProduct**);
fortint IGUNUSD(gribProduct**);
fortint GDECODE(gribProduct**,unsigned char*);
fortint IGLNGTH(gribProduct**);
fortint IGDATE(gribProduct**);
fortint IGTIME(gribProduct**);
fortint GSTEP(gribProduct**);
fortint IGTABLE(gribProduct**);
fortint IGCENTR(gribProduct**);
fortint IGPARAM(gribProduct**);
fortint IGLEVEL(gribProduct**);
fortint IGLEVL1(gribProduct**);
fortint IGLEVL2(gribProduct**);
fortint IGDEFIN(gribProduct**);
fortint IGCLASS(gribProduct**);
fortint IGTYPE(gribProduct**);
fortint IGSTREM(gribProduct**);
fortint IGEXPVR(gribProduct**);
fortint IGREPRS(gribProduct**);
fortint IGNUMPV(gribProduct**);
fortint GPV(gribProduct**,fortdouble*,fortint*);
fortint GPL(gribProduct**,fortint*,fortint*);
fortint IGNI(gribProduct**);
fortint IGNJ(gribProduct**);
fortdouble RGNWLAT(gribProduct**);
fortdouble RGNWLON(gribProduct**);
fortdouble RGSELAT(gribProduct**);
fortdouble RGSELON(gribProduct**);
fortint IGRESCO(gribProduct**);
fortdouble RGDI(gribProduct**);
fortdouble RGDJ(gribProduct**);
fortint IGGAUSS(gribProduct**);
fortint IGSCANM(gribProduct**);
fortint IGJ(gribProduct**);
fortint IGK(gribProduct**);
fortint IGM(gribProduct**);
fortint IGREPMO(gribProduct**);
fortdouble RGIP(gribProduct**);
fortint IGTJ(gribProduct**);
fortint IGTK(gribProduct**);
fortint IGTM(gribProduct**);
fortdouble RGLATRP(gribProduct**);
fortdouble RGLONRP(gribProduct**);
fortdouble RGROTAT(gribProduct**);
fortdouble RGLATSP(gribProduct**);
fortdouble RGLONSP(gribProduct**);
fortdouble RGSFACT(gribProduct**);
fortint numberOfValuesInSection4(gribProduct*);
fortint g4_offset(gribProduct*);
fortint IGNVALU(gribProduct**);
fortint GVALUES( gribProduct**,fortdouble*,fortint*,fortint*,fortdouble*);
void GVEND(gribProduct**);
fortint setupIrregularLongitudeIncrements(gribProduct**,fortint);
fortint GVINIT(gribProduct**,fortdouble*);
fortint GVECTOR( gribProduct**,fortdouble*,fortdouble*,fortdouble*);
fortint getSingleMapBit(unsigned char*,fortint);
void * allocateMemory(size_t);
void freeMemory(void*);

#ifdef __cplusplus
}
#endif

#endif /* End of GDECODE_H */
