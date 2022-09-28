C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GRCHK1(KSEC1,KRET)
C
C---->
C**** GRCHK1 - Check parameters for Section 1 of GRIB code.
C
C     Purpose.
C     --------
C
C     Check parameters for Section 1 of GRIB code against
C     valid values for GRIB Code Edition 1.
C
C**   Interface.
C     ----------
C
C     CALL GRCHK1(KSEC1,KRET)
C
C
C     Input Parameters.
C     -----------------
C
C     KSEC1      - Array containing parameters for section
C                  1 of GRIB code.
C
C     Output Parameters.
C     ------------------
C
C     KRET       - Return code.
C                  0   , No error encountered.
C                  1   , Error in GRIB code parameter.
C
C     Method.
C     -------
C
C     Values checked against current code/flag tables
C     and against maximum or minimum permitted values.
C     They are also checked against the current status
C     of the implementation of GRIBEX and ECMWF usage.
C
C     Externals.
C     ----------
C
C     KWCHK1  - Check Washington ensemble products local usage.
C     CHKTAB2 - Checks whether parameter appears in know code tables 2
C
C     Reference.
C     ----------
C
C     WMO Manual on Codes for GRIB code.
C
C     Comments.
C     ---------
C
C     None.
C
C     Author.
C     -------
C
C     J. Hennessy      ECMWF      18.06.91
C
C     Modifications.
C     --------------
C
C     J. Hennessy      ECMWF      30.08.91
C     Checks for bit-map present removed.
C     Check for negative value for century removed.
C
C     J. Hennessy      ECMWF      02.12.91
C     Changes to Table 2 and Parameter number checks.
C
C     J. Hennessy      ECMWF      22.07.92
C     Missing data value (255) allowed for originating centre
C     and parameter number.
C     Additional checks for experimental space view
C     perspective added.
C
C     J. Hennessy      ECMWF      09.11.92
C     Checks for ECMWF local use 1 and 2 added.
C
C     J. Hennessy      ECMWF      24.05.93
C     Checks for ECMWF local use 3 and 4 added.
C
C     J. Hennessy      ECMWF      08.06.93
C     Meteosat 3 and Goes 6 added.
C
C     J.D.Chambers     ECMWF      20.10.93
C     Checks for ECMWF local use 5 added.
C
C     J.D.Chambers     ECMWF      07.02.94
C     Add satellite streams 52,53 and satellite ids 121, 122
C
C     J.D.Chambers     ECMWF      10.05.94
C     Fix test of KSEC1(43) - total forecast number can be 0
C
C     J.D.Chambers     ECMWF      12.05.94
C     Checks for ECMWF local use 7 added.
C
C     J.D.Chambers     ECMWF      09.06.94
C     Allow missing data indicator(255) for date/time/...
C
C     J.D.Chambers     ECMWF      17.06.94
C     Add CLASS = 3 for ERA.
C
C     J.D.Chambers     ECMWF      19.09.94
C     Allow single field in an average.
C     Remove restriction that number of fields missing from an
C     average should be less than the number in the average.
C
C     J.D.Chambers     ECMWF      23.11.94
C     Only check for reserved fields if ECMWF local usage 1.
C
C     J.D.Chambers     ECMWF      02.02.95
C     Allow types 17 and 18 (ensemble mean/standard deviations).
C
C     J.D.Chambers     ECMWF      14.02.95
C     Allow type 52 (sensitivity forecast)
C
C     J.D.Chambers     ECMWF      28.03.95
C     Allow parameters 127 & 128 in code table 2, ECMWF table 128
C
C     J.D.Chambers     ECMWF      09.10.95
C     Check Washington ensemble products local usage.
C
C     J.D.Chambers     ECMWF      27.02.96
C     Allow for ECMWF local use definition 6.
C
C     J.D.Chambers     ECMWF      04.11.96
C     Add CLASS = 4, TYPE = 21, STREAM = 2231/2232/2233 for ECSN.
C
C     J.D.Chambers     ECMWF      08.01.98
C     Add TYPE = 22 (climate simulation, 360 day year) for ECSN.
C
C     J.D.Chambers     ECMWF      13.02.98
C     Add EPS tubes and SSTs.
C
C     J.D.Chambers     ECMWF      April 1998
C     Add wave 2D spectra directions and frequencies
C
C     J.D.Chambers     ECMWF      April 1998
C     Update WMO valid table entries.
C
C     J. Clochard, Meteo France, for ECMWF - June 1999.
C     Adapt CHKTAB2 to new calling syntax.
C
C     J.D.Chambers     ECMWF      April 2000
C     Add CLASS = 6 for DEMETER.
C     Add CLASS = 7 for PROVOST.
C
C     J.D.Chambers     ECMWF      October 2002
C     Add TYPE = 27 for extreme forecast index
C         TYPE = 28 for extreme forecast index control
C
C     J.D.Chambers     ECMWF      March 2003
C     Add TYPE = 33 for 4D variational increments
C
C     J.D.Chambers     ECMWF      April 2003
C     Add CLASS = 8 for ELDAS
C     Add TYPE = 34 for gridded observations
C     Add TYPE = 90-99 for Mars development
C     Add STREAM = 1061-1069 for Mars development
C     Add STREAM = 1100 and 1101 for ELDAS
C
C     J.D.Chambers     ECMWF      April 2003
C     Add type 64 - signal variance
C
C     J.D.Chambers     ECMWF      February 2004
C     Add monthly forecast streams, 1200 - 1211
C
C     J.D.Chambers     ECMWF      February 2004
C     Add monthly forecast streams, 1220 - 1224
C
C     J.D.Chambers     ECMWF      March 2004
C     Add Delayed CutOff streams, 1028 and 1029
C
C     J.D.Chambers     ECMWF      September 2004
C     Add local definition 24
C
C     J.D.Chambers     ECMWF      November 2004
C     Add local definition 25
C
C     J.D.Chambers     ECMWF      January 2005
C     Add streams 1039 and 1084
C
C     S.Curic     ECMWF      March 2005
C     Add STREAM = 1024 Daily archive hindcast
C         STREAM = 1034 Ensemble forecast overlap
C         STREAM = 1039 Ensemble forecast hindcasts (changed name)
C         STREAM = 1084 Ensemble wave hindcast (changed name)
C         STREAM = 1085 Wave hindcast
C         STREAM = 1086 Wave ensemble forecast overlap
C         STREAM = 1087
C     Add TYPE = 29 Probability Boundaries
C         TYPE = 36 Probability Distribution
C         TYPE = 37 Cluster information
C     Add CLASS = 10 Cosmo leps
C         CLASS = 11 Ensembles
C
C     S.Curic     ECMWF      March 2005
C     Add type of level 211 Ocean wave data (ECMWF extention)
C                       212 Wave spectra
C
C     S.Curic     ECMWF      July 2005
C     Add STREAM = 1075 Monthly means of Daily forecast accumulations
C     Add Time range indicator = 120 Average of N Forecast. Each product
C             is an accumulation from forecast lenght P1 to forecast
C              lenght P2, with reference times at intervals P2-P1
C     S.Curic     ECMWF      September 2005
C        211 level type has been replaced with 209 because 211 is
C        in use by NCEP
C     Add CLASS = 12 TIGGE
C
C     S.Curic     ECMWF      January 2006
C     Add CLASS = 14 ERA Interim
C
C     M.Fuentes   ECMWF      February 2006
C     Add TYPE = 65 CV (Cal/Val)
C
C     S.Curic     ECMWF      March 2006
C     Add STREAM = 1030  Ensemble Data Assimilation
C         STREAM = 1076  Monthly Means Daily climatology
C         STREAM = 1088  Ensemble Wave Data Assimilation
C         STREAM = 1089  Daily climatology wave
C
C     S.Curic     ECMWF      April 2006
C     Add STREAM = 1230  Multi-model Multi-annual Forecast
C         STREAM = 1231  Multi-model Multi-annual Forecast means
C         STREAM = 1232  Multi-model Multi-annual Forecast wave
C         STREAM = 1233  Multi-model Multi-annual Forecast wave means
C
C     S.Curic     ECMWF      January 2007
C     Add Satellite identifiers 56, 171
C
C     S.Curic     ECMWF      February 2007
C          STREAM = 1032 efho Ensemble Forecast Hindcast Overlap
C          STREAM = 1033 enfh Ensemble Forecast Hindcasts
C          STREAM = 1078 ewho Ensemble Forecast Wave Hindcast Overlap
C          STREAM = 1079 enwh Ensemble Forecast Wave Hindcasts
C
C     S.Curic     ECMWF      February 2007
C     Add CLASS = 15 SREPS Short-Range Ensemble Prediction System
C
C     S.Curic     ECMWF      July 2007
C     Add TYPE = 38       Unassigned
C         TYPE = 39       Unassigned
C
C     M.Fuentes   ECMWF      July 2007
C     Add STREAM = 1040   efhm Ensemble Forecast Hindcast Means
C
C     S.Curic     ECMWF      July 2007
C     Add CLASS = 16 dt (Data Targeting System)
C
C     S.Curic     ECMWF      July 2008
C     Add TYPE = 32      Climate Distribution (CD)
C         TYPE = 38      Shift Of Tails (SOT)
C     Add STREAM = 1077  Wave Ensemble forecast Hindcast Stat (WEHS)
C
C     S.Curic     ECMWF      September 2008
C     Add STREAM = 1240  Eurosip
C     Add STREAM = 1241  EUROSIP Hindcast Monthly Means
C
C     S.Curic     ECMWF      January 2010
C     Add CLASS = 19 MACC Monitoring Atmos. Composition and Climate
C
C     S.Curic     ECMWF      January 2010
C     Add CLASS = 20 PE Permanent experiments
C
C
C     S.Curic     ECMWF      September 2010
C     Add TYPE = 45     Cluster representative  CR
C
C     S.Curic     ECMWF      November 2010
C     Add satellite streams 172,257
C
C
C     S.Curic     ECMWF      December 2010
C     Add stream 1022  fsob forecast sensitivity to observations
C     Add stream 1023  fsow forecast sensitivity to observations wave
C     Add type  47    taem    time average ensemble mean
C     Add type  48    taes    time average ensemble standard deviation
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0 . Definition of variables. Data statements.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Subroutine arguments
C
      INTEGER KRET, KSEC1
      DIMENSION KSEC1(*)
C
C     Parameters
C
      INTEGER JPAVAC, JPTYP, JPSTR, JP1, JP3, JP4, JP5, JPIDSAT
      PARAMETER (JPAVAC=9)
      PARAMETER (JP1=4)
      PARAMETER (JP3=40)
      PARAMETER (JP4=12)
      PARAMETER (JP5=17)
      PARAMETER (JPTYP=71)
      PARAMETER (JPSTR=126)
      PARAMETER (JPIDSAT = 28)
C
C     Local variables
C
      LOGICAL LECMWF, LKWBC
C
      INTEGER LOOP, NRET, IDSATEL, NUMBER
      INTEGER IAC, IAVAC, IRET, ISTRE, ITAB1
      INTEGER ITAB3, ITAB4, ITAB5, ITEMP, ITYPE
C
      DIMENSION IDSATEL(JPIDSAT)
      DIMENSION ITAB1(JP1)
      DIMENSION ITAB3(JP3)
      DIMENSION ITAB4(JP4)
      DIMENSION ITAB5(JP5)
      DIMENSION IAVAC(JPAVAC)
      DIMENSION ITYPE(JPTYP)
      DIMENSION ISTRE(JPSTR)
C
      CHARACTER*121 HFIRST, HSECOND, HTHIRD, HFOURTH
C
      SAVE ITAB1, ITAB3, ITAB4, ITAB5, IAVAC
      SAVE ITYPE, ISTRE, IDSATEL
C
C     Externals
C
      INTEGER CHKTAB2
      EXTERNAL CHKTAB2
C
C     Valid values given in Code Table 1.
C
      DATA ITAB1  /0,128,64,192/
C
C     Valid values given in Code Table 3.
C
      DATA ITAB3 /1,2,3,4,5,6,7,8,9,20,
     X            100,101,102,103,104,105,106,107,108,109,
     X            110,111,112,113,114,115,116,117,119,
     X            120,121,125,128,141,160,200,201,209,210,212/
C
C     Valid values given in Code Table 4.
C
      DATA ITAB4 /0,1,2,3,4,5,6,7,10,11,12,
     X            254/
C
C     Valid values given in Code Table 5.
C
      DATA ITAB5 /0,1,2,3,4,5,
     X            10,
     X            113,114,115,116,117,118,119,
     X            120,123,124/
C
C     Valid values given in Code Table 5, for averages and
C     accumulations.
C
      DATA IAVAC /113,114,115,116,117,118,119,
     X            123,124/
C
C     Valid values for type.
C

      DATA ITYPE /    1, 2, 3, 4, 5, 6, 7, 8, 9,
     X            10,11,12,13,14,15,16,17,18,19,
     X            20,21,22,23,24,25,26,27,28,29,
     X            30,31,32,33,34,35,36,37,38,39,
     X            40,41,42,45,47,48,
     X            50,51,52,
     X            60,61,62,63,64,65,
     X            70,71,72,
     X            80,81,82,83,
     X            90,91,92,93,94,95,96,97,98,99/
C
C     Valid values for stream.
C
      DATA ISTRE /
     X      50,  51,  52,  53,  54, 55,
     X     119, 120, 121, 122, 130, 140,
     X     150, 151, 152, 201, 250, 251, 252, 253, 254, 255, 256,
     X     1022,1023,
     X     1024,1025,1026,1027,1028,1029,1030,1032,1033,
     X     1034,1035,1036,1037,1038,1039,
     X     1040,1041,1042,1043,1044,1045,1046,1047,
     X     1050,1051,1052,1053,1054,1055,1056,
     X     1060,1061,1062,1063,1064,1065,1066,1067,1068,1069,
     X     1070,1071,1072,1073,1074,1075,1076,1077,1078,1079,
     X     1080,1081,1082,1083,1084,1085,1086,1087,1088,1089,
     X     1090,1091,1092,1093,1094,1095,1096,1097,
     X     1100,1101,1110,
     X     1200,1201,1202,1203,1204,1205,1206,1207,1208,1209,
     X     1210,1211,
     X     1220,1221,1222,1223,1224,
     X     1230,1231,1232,1233,
     X     1240,1241,
     X     2231,2232,2233,
     X     2240,2241,2242,2243/
C
C
C     Valid values for satellite identifiers
C
      DATA IDSATEL/
     X   50, 51, 52, 53, 54, 55, 56,119, 120, 121, 122, 130, 140,
     X  150, 151, 152, 171, 172, 199, 201, 250, 251, 252, 253,
     X   254, 255, 256, 257/
C
C     -----------------------------------------------------------------|
C*    Section 1 . Set initial values.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
C     Reset return code.
      KRET = 0
C
C     -----------------------------------------------------------------|
C*    Section 2 . Check values against code tables and extreme values.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C*    Check Parameter Table version number.
C
      IF( (KSEC1(1).LT.1).OR.(KSEC1(1).GT.254) ) THEN
        WRITE(GRPRSM,9001) KSEC1(1)
        KRET = 1
      ENDIF
C
C*    Check Identification of centre. Code Table 0.
C     Currently only values 1 to 98 inclusive are used.
C     Missing value indicator (255) is allowed.
C
      IF( (KSEC1(2).LT.0).OR.(KSEC1(2).GT.255) ) THEN
        WRITE(GRPRSM,9002) KSEC1(2)
        KRET = 1
      ENDIF
C
      LECMWF = ( KSEC1(2).EQ.98 ).OR.
     X         ( (KSEC1(2).EQ.80).AND.
     X           (KSEC1(24).EQ.1).AND.
     X           (KSEC1(37).EQ.50) )
      LKWBC  = ( (KSEC1(2).EQ.7) .AND. (KSEC1(22).NE.98) )
C
C*    Check Generating process identification number.
C
      IF( (KSEC1(3).LT.1).OR.(KSEC1(3).GT.255) ) THEN
        WRITE(GRPRSM,9003) KSEC1(3)
        KRET = 1
      ENDIF
C
C*    Check Grid definition.
C
      IF( (KSEC1(4).LT.1).OR.(KSEC1(4).GT.255) ) THEN
        WRITE(GRPRSM,9004) KSEC1(4)
        KRET = 1
      ENDIF
C
C*    Check Flag. Code Table 1.
C
      DO LOOP = 1, JP1
         IF( KSEC1(5).EQ.ITAB1(LOOP) ) GOTO 215
      ENDDO
C
      WRITE(GRPRSM,9005) KSEC1(5)
      KRET = 1
C
  215 CONTINUE
C
C*    Cross check that, if uncatalogued grid is specified, Section
C     2, Grid description section, is included.
C
      IF( (KSEC1(4).EQ.255).AND.
     X    ( (KSEC1(5).EQ.0).OR.(KSEC1(5).EQ.64) ) ) THEN
        KRET = 1
        WRITE(GRPRSM,9014)
      ENDIF
C
C*    Check Parameter indicator. Code Table 2.
C
      IF( (KSEC1(6).LT.1).OR.(KSEC1(6).GT.255) ) THEN
        WRITE(GRPRSM,9006) KSEC1(6)
        KRET = 1
      ENDIF
C
C*    Check ECMWF Parameter Table Number and Parameter Indicators.
C
      IF( LECMWF ) THEN
C
        IRET = CHKTAB2(KSEC1,HFIRST,HSECOND,HTHIRD,HFOURTH)
C
        IF( IRET.EQ.20001 ) THEN
C
          IF( MIN( KSEC1(1),KSEC1(6)) .LE. 127 ) THEN
            WRITE(GRPRSM,*)
     X        'GRCHK1: WMO code table 2 version ', KSEC1(1)
          ELSEIF (LECMWF) THEN
            WRITE(GRPRSM,*)
     X        'GRCHK1: ECMWF local table 2 version ', KSEC1(1)
          ELSE
            WRITE(GRPRSM,*)
     X 'GRCHK1: originating centre local table 2 version ', KSEC1(1)
          ENDIF
C
          WRITE(GRPRSM,*)
     X        '        does not contain parameter  ', KSEC1(6)
C
        ENDIF
C
      ENDIF
C
C*    Check International Table useage.
C
      IF( (KSEC1(1).LT.127).AND.(KSEC1(6).GT.127) ) THEN
        WRITE(GRPRSM,9024) KSEC1(6) , KSEC1(1)
      ENDIF
C
C*    Check Indicator of type of level. Code Table 3.
C     Experimental space view uses this field as satellite
C     identification.
C
      IF( (KSEC1(6).EQ.127).AND.(KSEC1(1).EQ.1) ) THEN
C
C       Satellite identification.
C
        NRET = 1
        DO LOOP = 1, JPIDSAT
          IF( KSEC1(7).EQ.IDSATEL(LOOP) ) NRET = 0
        ENDDO
C
        IF( NRET.EQ.1 ) THEN
          WRITE(GRPRSM,*)
     X        'GRCHK1: Invalid satellite identifier: ',KSEC1(7)
          KRET = 1
        ENDIF

      ELSE
C
        IRET = 0
C
C       Type of level.
C
        DO LOOP = 1, JP3
          IF( KSEC1(7).EQ.ITAB3(LOOP) ) GOTO 225
        ENDDO
C
        IRET = 1
C
  225   CONTINUE
C
C       ECMWF uses 200 for pseudo-levels.
C
        IF( (KSEC1(7).EQ.200).AND.LECMWF ) IRET = 0
        IF( IRET.EQ.1 ) THEN
          WRITE(GRPRSM,9015) KSEC1(7)
          KRET = 1
        ENDIF
C

C
      ENDIF
C
C     For certain level types no description is necessary and
C     those fields should be 0.
C
      IF( (KSEC1(7).LT.8).OR.(KSEC1(7).EQ.102) ) THEN
        ITEMP = KSEC1(8) + KSEC1(9)
        IF( ITEMP.NE.0 ) THEN
          KRET = 1
          WRITE(GRPRSM,9023) KSEC1(7) , KSEC1(8) , KSEC1(9)
        ENDIF
      ENDIF
C
C*    Check Year of century.
C
C
      IF( (KSEC1(10).LT.0).OR.(KSEC1(10).GT.100) ) THEN
        IF( KSEC1(10).NE.255 ) THEN
          WRITE(GRPRSM,9008) KSEC1(10)
          KRET = 1
        ENDIF
      ENDIF
C
C*    Month check.
C
      IF( (KSEC1(11).LT.1).OR.(KSEC1(11).GT.12) ) THEN
        IF( KSEC1(10).NE.255 ) THEN
          WRITE(GRPRSM,9009) KSEC1(11)
          KRET = 1
        ENDIF
      ENDIF
C
C*    Day check.
C
      IF( (KSEC1(12).LT.1).OR.(KSEC1(12).GT.31) ) THEN
        IF( KSEC1(10).NE.255 ) THEN
          WRITE(GRPRSM,9010) KSEC1(12)
          KRET = 1
        ENDIF
      ENDIF
C
C*    Hour check.
C
      IF( (KSEC1(13).LT.0).OR.(KSEC1(13).GT.23) ) THEN
        IF( KSEC1(10).NE.255 ) THEN
          WRITE(GRPRSM,9011) KSEC1(13)
          KRET = 1
        ENDIF
      ENDIF
C
C*    Minute check.
C
      IF( (KSEC1(14).LT.0).OR.(KSEC1(14).GT.59) ) THEN
        IF( KSEC1(10).NE.255 ) THEN
          WRITE(GRPRSM,9012) KSEC1(14)
          KRET = 1
        ENDIF
      ENDIF
C
C*    Indicator of unit of time check. Code Table 4.
C
      DO LOOP = 1, JP4
        IF( KSEC1(15).EQ.ITAB4(LOOP) ) GOTO 245
      ENDDO
C
      WRITE(GRPRSM,9013) KSEC1(15)
      KRET = 1
C
  245 CONTINUE
C
C*    Time range indicator check. Code Table 5.
C
      DO LOOP = 1, JP5
        IF( KSEC1(18).EQ.ITAB5(LOOP) ) GOTO 255
      ENDDO
C
      WRITE(GRPRSM,9019) KSEC1(18)
      KRET = 1
C
  255 CONTINUE
C
C*    Cross check Time range indicator and Number averaged or
C     accumulated.
C
      IAC = 0
      DO LOOP = 1, JPAVAC
        IF( KSEC1(18).EQ.IAVAC(LOOP) ) IAC = 1
      ENDDO
C
C     If average or accumulation, check for valid numbers
C     of included and missing values.
C
      IF( IAC.EQ.1. ) THEN
        IF( KSEC1(19).LT.1 ) THEN
          WRITE(GRPRSM,9016) KSEC1(18) , KSEC1(19)
Cjdc      KRET = 1
        ENDIF
        IF( KSEC1(20).LT.0 ) THEN
          WRITE(GRPRSM,9020) KSEC1(18) , KSEC1(20)
Cjdc      KRET = 1
        ENDIF
      ENDIF
C
C*    Century check.
C
C     ECMWF data starts in 20th century.
C commented to allow work Simona
C
cs      IF( (KSEC1(21).LT.20).AND.LECMWF ) THEN
cs        WRITE(GRPRSM,9021) KSEC1(21)
cs        KRET = 1
cs      ENDIF
C
C*    Decimal scale factor check.
C
C     At ECMWF the scale factor is always 0.
C
Cjdc  IF( (KSEC1(23).NE.0).AND.LECMWF ) THEN
Cjdc    WRITE(GRPRSM,9022) KSEC1(23)
Cjdc    KRET = 1
Cjdc  ENDIF
C
C*    Flag field, indicating local ECMWF usage.
C
      IF( (KSEC1(24).NE.0).AND.(KSEC1(24).NE.1) ) THEN
        WRITE(GRPRSM,9025)
        KRET = 1
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3 . Check validity of values for ECMWF local use.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      IF( (KSEC1(24).EQ.1).AND.LECMWF ) THEN
C
C*      Check reserved fields are zero.
C
        DO LOOP = 25, 36
          IF( KSEC1(LOOP).NE.0 ) WRITE(GRPRSM,9026)
        ENDDO
C
C       Check local definition number.
C
        IF( (KSEC1(37).LT.1).OR.(KSEC1(37).GT.254) ) THEN
          KRET = 1
          WRITE(GRPRSM,9101) KSEC1(37)
        ENDIF
C
C       Check Class.
C         1 = operations
C         2 = research
C         3 = ECMWF re-analysis
C         4 = European Climate Support Network (ECSN).
C         5 = ERA 40 (ECMWF 40-year re-analysis)
C         6 = DEMETER
C         7 = PROVOST
C         8 = ELDAS
C         9 = TOST
C        10 = COSMO
C        11 = Ensembles
C        12 = TIGGE
C        14 = ERA Interim
C        15 = SREPS
C        16 = dt (Data Targeting System)
C        17 = la LACE ALADIN
C        18 = yt YOTC
C        19 = MACC
C        99 = ECMWF testing
C
C      1-120 = Member State
C        121 = Generic Member State
C
        IF( (KSEC1(38).LT.1).OR.
     X      ((KSEC1(38).GT.20).AND.
     X       (KSEC1(38).LT.99).OR.(KSEC1(38).GT.121)) ) THEN
          KRET = 1
          WRITE(GRPRSM,9102) KSEC1(38)
        ENDIF
C
C       Check Type.
C
        DO LOOP = 1, JPTYP
          IF( KSEC1(39).EQ.ITYPE(LOOP) ) GOTO 325
        ENDDO
C
        WRITE(GRPRSM,9103 ) KSEC1(39)
        KRET = 1
C
  325   CONTINUE
C
C       Check Stream.
C
         DO LOOP = 1, JPSTR
           IF( KSEC1(40).EQ.ISTRE(LOOP) ) GOTO 335
         ENDDO
C
         WRITE(GRPRSM,9104) KSEC1(40)
         KRET = 1
C
  335    CONTINUE
C
C        No check on the ASCII Version Number.
C
C
C        Check ECMWF local definition 1.
C        (MARS labelling or ensemble forecast data)
C
         IF( KSEC1(37).EQ.1 ) THEN
C
C          Check ensemble forecast number and total forecasts number.
C          (ECMWF ensemble seasonal forecasts, stream 1090, uses
C          2-byte ensemble forecast number)
C
           IF( KSEC1(40).NE.1090 ) THEN
C
             IF( (KSEC1(42).LT.0).OR.(KSEC1(42).GT.255) ) THEN
               KRET = 1
               WRITE(GRPRSM,9105) KSEC1(42)
             ENDIF
C
             IF( (KSEC1(43).LT.0).OR.(KSEC1(43).GT.255) ) THEN
               KRET = 1
               WRITE(GRPRSM,9106) KSEC1(43)
             ENDIF
C
           ELSE
C
             IF( (KSEC1(42).LT.0).OR.(KSEC1(42).GT.65535) ) THEN
               KRET = 1
               WRITE(GRPRSM,9105) KSEC1(42)
             ENDIF
C
           ENDIF
C
         ENDIF
C
C        Check ECMWF local definition 2.
C        (Cluster means and standard deviations)
C
         IF( KSEC1(37).EQ.2 ) THEN
C
C          Check cluster number.
C
           IF( (KSEC1(42).LT.1).OR.(KSEC1(42).GT.255) ) THEN
             KRET = 1
             WRITE(GRPRSM,9107)
           ENDIF
C
C          Check total number of clusters.
C
           IF( (KSEC1(43).LT.1).OR.(KSEC1(43).GT.255) ) THEN
             KRET = 1
             WRITE(GRPRSM,9108)
           ENDIF
C
C          Check clustering method.
C
           IF( (KSEC1(44).LT.1).OR.(KSEC1(44).GT.3) ) THEN
             KRET = 1
             WRITE(GRPRSM,9109)
           ENDIF
C
         ENDIF
C
C        Check ECMWF local definition 6.
C        (Surface temperatures)
C
         IF( KSEC1(37).EQ.6 ) THEN
C
C          Check type for SST field.
C
           IF( (KSEC1(45).LT.0).OR.(KSEC1(45).GT.2) ) THEN
             KRET = 1
             WRITE(GRPRSM,*)
     X         'GRCHK1: Invalid type for SST field = ',KSEC1(45)
           ENDIF
C
C          Check count of ICE fields used.
C
           IF( KSEC1(46).LT.0 ) THEN
             KRET = 1
             WRITE(GRPRSM,*)
     X         'GRCHK1: Invalid count of ICE fields used = ',KSEC1(46)
           ENDIF
C
         ENDIF
C
       ENDIF
C
C        Check ECMWF local definition 18.
C        (Multi-analysis ensemble data)
C
         IF( KSEC1(37).EQ.18 ) THEN
C
C          Check list of WMO centre identifiers
C
           NUMBER = KSEC1(46)
           IF( NUMBER.LT.0 ) THEN
             KRET = 1
             WRITE(GRPRSM,*)
     X         'GRCHK1: WMO id list count invalid = ',NUMBER
           ENDIF
C
           IF( NUMBER.GT.15 ) THEN
             KRET = 1
             WRITE(GRPRSM,*)
     X         'GRCHK1: WMO id list count greater than 15 = ',NUMBER
           ENDIF
C
         ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 4 . Check validity of values for Washington ensemble
C                 products local usage.
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
Cjdc  IF( (KSEC1(24).EQ.1).AND.LKWBC ) CALL KWCHK1(KSEC1,KRET)
C
C     -----------------------------------------------------------------|
C*    Section 9 . Return to calling routine. Format statements.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
C
 9001 FORMAT (1H ,'GRCHK1: Invalid Parameter Table version number - ',
     C             I5)
 9002 FORMAT (1H ,'GRCHK1: Invalid Identification of Centre - ',I5)
 9003 FORMAT (1H ,'GRCHK1: Invalid Generating Process - ',I5)
 9004 FORMAT (1H ,'GRCHK1: Invalid Grid Definition - ',I5)
 9005 FORMAT (1H ,'GRCHK1: Invalid flag field - ',I5,' decimal.')
 9006 FORMAT (1H ,'GRCHK1: Invalid Indicator of parameter - ',I5)
 9008 FORMAT (1H ,'GRCHK1: Invalid year of century - ',I5)
 9009 FORMAT (1H ,'GRCHK1: Invalid month - ',I5)
 9010 FORMAT (1H ,'GRCHK1: Invalid day - ',I5)
 9011 FORMAT (1H ,'GRCHK1: Invalid hour - ',I5)
 9012 FORMAT (1H ,'GRCHK1: Invalid minute - ',I5)
 9013 FORMAT (1H ,'GRCHK1: Invalid Indicator of unit of time - ',I5)
 9014 FORMAT (1H ,'GRCHK1: Uncatalogued grid and no Section 2.')
 9015 FORMAT (1H ,'GRCHK1: Invalid indicator of type of level - ',I5)
 9016 FORMAT (1H ,'GRCHK1: Inconsistent Time Range Indicator',
     C            ' - ',I5,' and number included in averages - ',I5)
 9019 FORMAT (1H ,'GRCHK1: Invalid Time Range Indicator - ',I5)
 9020 FORMAT (1H ,'GRCHK1: Inconsistent Time Range Indicator',
     C            ' - ',I5,' and number missing from averages - ',I5)
 9021 FORMAT (1H ,'GRCHK1: Invalid century of reference time - ',I5)
 9022 FORMAT (1H ,'GRCHK1: Invalid decimal scale factor - ',I5)
 9023 FORMAT (1H ,'GRCHK1: For level type ',I3,' descriptions are',
     C            'invalid - ',I5,3X,I5)
 9024 FORMAT (1H ,'GRCHK1: ** WARNING ** Parameter number ',I3,' is',
     C            ' not defined in International Table number ',I3,'.')
 9025 FORMAT (1H ,'GRCHK1: Local use flag KSEC1(24) should be',
     C                      ' 0 or 1.')
 9026 FORMAT (1H ,'GRCHK1: Reserved fields KSEC1(25->36) should be 0 ',
     C                      'to avoid problems with future releases.')
C
 9101 FORMAT (1H ,'GRCHK1: Invalid local use definition: ',I8)
C
 9102 FORMAT (1H ,'GRCHK1: Invalid class - ',I5)
C
 9103 FORMAT (1H ,'GRCHK1: Invalid type - ',I5)
C
 9104 FORMAT (1H ,'GRCHK1: Invalid stream - ',I5)
C
 9105 FORMAT (1H ,'GRCHK1: Invalid ensemble forecast number - ',I5)
C
 9106 FORMAT (1H ,'GRCHK1: Invalid total number of forecasts - ',I5)
C
 9107 FORMAT (1H ,'GRCHK1: Invalid cluster number - ',I5)
C
 9108 FORMAT (1H ,'GRCHK1: Invalid total number of clusters - ',I5)
C
 9109 FORMAT (1H ,'GRCHK1: Invalid clustering method - ',I5)
C
      END
