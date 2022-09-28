C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GRPRS1(KSEC0,KSEC1)
C
C---->
C**** GRPRS1
C
C     Purpose.
C     --------
C
C     Print the information in the Product Definition
C     Section (Section 1) of decoded GRIB data.
C
C**   Interface.
C     ----------
C
C     CALL GRPRS1(KSEC0,KSEC1)
C
C
C     Input Parameters.
C     -----------------
C
C     KSEC0 - Array of decoded parameters from Section 0.
C     KSEC1 - Array of decoded parameters from Section 1.
C
C
C     Output Parameters.
C     ------------------
C
C     None.
C
C
C     Method.
C     -------
C
C     Flag fields are printed in binary representation.
C     Other fields as integers.
C     Fields printed depend on GRIB Edition.
C
C
C     Externals.
C     ----------
C
C     PRTBIN  - Print binary value in character format.
C     KWPRS1  - Print Washington ensemble products.
C     CHKTAB2 - Looks up a parameter in a given ECMWF local table 2.
C
C     Common block GRPRSCM.
C
C
C     Reference.
C     ----------
C
C     WMO Manual on Codes re GRIB Code.
C     See also routine GRIBEX.
C
C
C     Comments.
C     ---------
C
C     When decoding data from Experimental Edition or Edition 0,
C     routine GRIBEX adds the additional fields available in
C     Edition 1.
C
C     Checks environment variable GRPRS_STREAM for output stream number
C     for printing (default is 6)
C
C
C     Author.
C     -------
C     J. Hennessy    ECMWF 18.06.91
C
C
C     Modifications.
C     --------------
C     J. Hennessy    ECMWF 30.08.91
C     Different print for GRIB Editions up to Edition 1
C     removed.
C
C     J. Hennessy    ECMWF 07.01.92
C     Different print for GRIB Editions up to Edition 1
C     added for centres other than ECMWF.
C
C     J. Hennessy    ECMWF 27.07.92
C     Print added for satellite identifiers.
C
C     J. Hennessy    ECMWF 03.11.92
C     Print of local ECMWF use  1 of section 1 added.
C
C     J. Hennessy    ECMWF 03.12.92
C     Version number changed to character print.
C
C     J. Hennessy    ECMWF 20.05.93
C     Print of local ECMWF use 2,3 and 4 of section 1 added.
C
C     J.D.Chambers   ECMWF 18.10.93
C     Print of local ECMWF use 5 of section 1 added.
C
C     J.D.Chambers   ECMWF 10.05.94
C     Fix print of version number
C
C     J.D.Chambers   ECMWF 02.06.94
C     Fix print of total number of forecasts and forecast number
C     for MARS labelling
C
C     J.D.Chambers   ECMWF 19.01.95
C     Allow printing of ECMWF local use definition 8.
C
C     J.D.Chambers   ECMWF 14.02.95
C     Modify printing of ECMWF local use definition 7.
C
C     J.D.Chambers   ECMWF 09.10.95
C     Add handling of Washington ensemble products.
C
C     J.D.Chambers     ECMWF      27.02.96
C     Allow for ECMWF local use definition 6.
C
C     J.D.Chambers     ECMWF      13.02.98
C     Allow for ECMWF local use definition 10 (EPS tubes).
C
C     J.D.Chambers     ECMWF      16.02.98
C     Allow for ECMWF local use definition 11 (Supplementary data
C     used by analysis).
C
C     J.D.Chambers     ECMWF      April 1998
C     Allow for ECMWF local use definition 13
C
C     J.D.Chambers     ECMWF      November 1998
C     Allow for ECMWF local use definition 12
C
C     J. Clochard, Meteo France, for ECMWF - June 1999.
C     Replace LOCAL2K call by CHKTAB2 call.
C
C     J.D.Chambers   ECMWF    June 2002
C     Add option to set output stream number using environment variable
C     GRPRS_STREAM
C
C     J.D.Chambers     ECMWF      October 2002
C     Allow for ECMWF local use definition 19
C
C     J.D.Chambers     ECMWF      March 2003
C     Allow for ECMWF local use definition 20
C
C     J.D.Chambers     ECMWF      March 2003
C     Allow for ECMWF local use definition 21
C
C     J.D.Chambers     ECMWF      February 2004
C     Allow for ECMWF local use definition 22 and 23
C
C     J.D.Chambers     ECMWF      September 2004
C     Allow for ECMWF local use definition 24
C
C     J.D.Chambers     ECMWF      November 2004
C     Allow for ECMWF local use definition 25
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "common/grprs.h"
#include "grbcom.h"
C
C     Parameter(s)
C
      INTEGER JPBYTES
#ifdef INTEGER_8
      PARAMETER (JPBYTES = 8)
#else
      PARAMETER (JPBYTES = 4)
#endif
C
C     Subroutine arguments
C
      INTEGER KSEC0(*)
      INTEGER KSEC1(*)
C
C     Local variables
C
      INTEGER IPREV, ICURR, ICOUNT, IOFFSET, IOFF2, IOFF3
      INTEGER IBIT, IERR, IOUT, IYEAR, NBYTES
      INTEGER JLOOP, JILOOP
      REAL VALUE
C
      CHARACTER*8 HVERSION
      CHARACTER*121 HFIRST, HSECOND, HTHIRD, HFOURTH
C
C     Externals
C
      INTEGER CHKTAB2
      EXTERNAL CHKTAB2
C
C     -----------------------------------------------------------------|
C*    Section 1 . Print required information.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      WRITE(GRPRSM,9000)
      WRITE(GRPRSM,9001)
      WRITE(GRPRSM,9002)
C
      WRITE(GRPRSM,9100) KSEC1(1)
      WRITE(GRPRSM,9101) KSEC1(2)
      WRITE(GRPRSM,9102) KSEC1(3)
      WRITE(GRPRSM,9103) KSEC1(4)
      IBIT = 8
      CALL PRTBIN (KSEC1(5),IBIT,IOUT,IERR)
      WRITE(GRPRSM,9104) IOUT
      WRITE(GRPRSM,9105) KSEC1(6)
      IERR = CHKTAB2(KSEC1,HFIRST,HSECOND,HTHIRD,HFOURTH)
      IF( IERR .EQ. 0 ) THEN
        IOFF2 = 121
        DO JLOOP = 121, 1, -1
          IF( HSECOND(JLOOP:JLOOP).NE.' ' ) THEN
            IOFF2 = JLOOP
            GOTO 110
          ENDIF
        ENDDO
        GOTO 130
 110    CONTINUE
        IOFF3 = 121
        DO JLOOP = 121, 1, -1
          IF( HTHIRD(JLOOP:JLOOP).NE.' ' ) THEN
            IOFF3 = JLOOP
            GOTO 120
          ENDIF
        ENDDO
        GOTO 130
 120    CONTINUE
        WRITE(GRPRSM,*)'('//HSECOND(1:IOFF2)//': '//HTHIRD(1:IOFF3)//')'
C
 130    CONTINUE
      ENDIF
C
      IF (KSEC1(6).NE.127) THEN
        WRITE(GRPRSM,9106) KSEC1(7)
        WRITE(GRPRSM,9107) KSEC1(8)
        WRITE(GRPRSM,9108) KSEC1(9)
      ELSE
        WRITE(GRPRSM,9206) KSEC1(7)
        WRITE(GRPRSM,9207) KSEC1(8)
      ENDIF
C
      IYEAR = KSEC1(10)
      IF( IYEAR.NE.255 ) THEN
        IYEAR  = ((KSEC1(21)-1)*100 + KSEC1(10))
        WRITE(GRPRSM,9109) KSEC1(10), IYEAR
      ELSE
        WRITE(GRPRSM,*) 'Year of reference time of data (=MISSING)  255'
      ENDIF
      WRITE(GRPRSM,9110) KSEC1(11)
      WRITE(GRPRSM,9111) KSEC1(12)
      WRITE(GRPRSM,9112) KSEC1(13)
      WRITE(GRPRSM,9113) KSEC1(14)
      WRITE(GRPRSM,9114) KSEC1(15)
      WRITE(GRPRSM,9115) KSEC1(16)
      WRITE(GRPRSM,9116) KSEC1(17)
      WRITE(GRPRSM,9117) KSEC1(18)
      WRITE(GRPRSM,9118) KSEC1(19)
      WRITE(GRPRSM,9119) KSEC1(20)
C
C     All ECMWF data in GRIB Editions before Edition 1 is decoded
C     as 20th century data. Other centres are decoded as missing.
C
      IF (KSEC0(2).LT.1.AND.KSEC1(2).NE.98) THEN
        WRITE(GRPRSM,9122)
      ELSE
        WRITE(GRPRSM,9120) KSEC1(21)
      ENDIF
C
C     Print sub-centre
      WRITE(GRPRSM,9123) KSEC1(22)
C
C     Decimal scale factor
      WRITE(GRPRSM,9121) KSEC1(23)
C
C     -----------------------------------------------------------------|
C*    Section 2 . Print local ECMWF information.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Regular MARS labelling, or reformatted Washington EPS products.
C
      IF ((KSEC1(2).EQ.98.AND.KSEC1(24).EQ.1) .OR.
     X    (KSEC1(22).EQ.98.AND.KSEC1(24).EQ.1) .OR.
     X    (KSEC1(2).EQ. 7.AND.KSEC1(22).EQ.98)) THEN
C
C       Parameters common to all definitions.
C
        WRITE(GRPRSM,9301) KSEC1(37)
        IF(KSEC1(37).EQ.1)
     X    WRITE(GRPRSM,*) '(Mars labelling or ensemble forecast)'
        IF(KSEC1(37).EQ.2)
     X    WRITE(GRPRSM,*) '(Cluster means and standard deviations)'
        IF(KSEC1(37).EQ.3)
     X    WRITE(GRPRSM,*) '(Satellite image data)'
        IF(KSEC1(37).EQ.4)
     X    WRITE(GRPRSM,*) '(Ocean model data)'
        IF(KSEC1(37).EQ.5)
     X    WRITE(GRPRSM,*) '(Forecast probability data)'
        IF(KSEC1(37).EQ.6)
     X    WRITE(GRPRSM,*) '(Surface temperature data)'
        IF(KSEC1(37).EQ.7)
     X    WRITE(GRPRSM,*) '(Sensitivity data)'
        IF(KSEC1(37).EQ.8)
     X    WRITE(GRPRSM,*) '(ECMWF re-analysis data)'
        IF(KSEC1(37).EQ.9)
     X    WRITE(GRPRSM,*) '(Singular vectors / ensemble perturbations)'
        IF(KSEC1(37).EQ.10)
     X    WRITE(GRPRSM,*) '(EPS tubes)'
        IF(KSEC1(37).EQ.11)
     X    WRITE(GRPRSM,*) '(Supplementary data used by analysis)'
        IF(KSEC1(37).EQ.13)
     X    WRITE(GRPRSM,*) '(Wave 2D spectra direction and frequency)'
        IF(KSEC1(37).EQ.14)
     X    WRITE(GRPRSM,*) '(Brightness temperature)'
        IF(KSEC1(37).EQ.15)
     X    WRITE(GRPRSM,*) '(Ocean ensemble seasonal forecast)'
        IF(KSEC1(37).EQ.16)
     X    WRITE(GRPRSM,*) '(Seasonal fc monthly mean atmosphere data)'
        IF(KSEC1(37).EQ.17)
     X    WRITE(GRPRSM,*) '(Sst or sea-ice used by analysis)'
        IF(KSEC1(37).EQ.18)
     X    WRITE(GRPRSM,*) '(Multi-analysis ensemble data)'
        IF(KSEC1(37).EQ.19)
     X    WRITE(GRPRSM,*) '(Extreme forecast index data)'
        IF(KSEC1(37).EQ.20)
     X    WRITE(GRPRSM,*) '(4D variational increments)'
        IF(KSEC1(37).EQ.21)
     X    WRITE(GRPRSM,*) '(Sensitive area predictions)'
        IF(KSEC1(37).EQ.23)
     X    WRITE(GRPRSM,*)
     X '(Coupled atmospheric, wave & ocean means (+ hindcast support))'
        IF(KSEC1(37).EQ.24)
     X    WRITE(GRPRSM,*) '(Satellite image simulation)'
        IF(KSEC1(37).EQ.25)
     X    WRITE(GRPRSM,*) '(4DVar model errors)'
        IF(KSEC1(37).EQ.50)
     X    WRITE(GRPRSM,*) '(Member State data)'
        IF(KSEC1(37).EQ.190)
     X    WRITE(GRPRSM,*) '(Multiple ECMWF local definitions)'
        IF(KSEC1(37).EQ.191)
     X    WRITE(GRPRSM,*) '(Free format data descriptor)'
C
        WRITE(GRPRSM,9302) KSEC1(38)
        WRITE(GRPRSM,9303) KSEC1(39)
        WRITE(GRPRSM,9304) KSEC1(40)
        WRITE(HVERSION, '(A8)' ) KSEC1(41)
        WRITE(GRPRSM,9305)
     X    HVERSION(5:5),
     X    HVERSION(6:6),
     X    HVERSION(7:7),
     X    HVERSION(8:8)
C
C       ECMWF Local definition 1.
C       (MARS labelling or ensemble forecast data)
C
        IF (KSEC1(37).EQ.1) THEN
          WRITE(GRPRSM,9310) KSEC1(42)
          IF( KSEC1(40).NE.1090 ) WRITE(GRPRSM,9311) KSEC1(43)
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 2.
C       (Cluster means and standard deviations)
C
        IF (KSEC1(37).EQ.2) THEN
          WRITE(GRPRSM,9312) KSEC1(42)
          WRITE(GRPRSM,9313) KSEC1(43)
          WRITE(GRPRSM,9314) KSEC1(44)
          WRITE(GRPRSM,9315) KSEC1(45)
          WRITE(GRPRSM,9316) KSEC1(46)
          WRITE(GRPRSM,9317) KSEC1(47)
          WRITE(GRPRSM,9318) KSEC1(48)
          WRITE(GRPRSM,9319) KSEC1(49)
          WRITE(GRPRSM,9320) KSEC1(50)
          WRITE(GRPRSM,9321) KSEC1(51)
          WRITE(GRPRSM,9324) KSEC1(52)
          WRITE(GRPRSM,9322) KSEC1(53)
          DO 210 JLOOP = 1,KSEC1(53)
            WRITE(GRPRSM,9323) KSEC1(JLOOP+53)
  210     CONTINUE
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 3.
C       (Satellite image data)
C
        IF (KSEC1(37).EQ.3) THEN
          WRITE(GRPRSM,9306) KSEC1(42)
          WRITE(GRPRSM,9307) KSEC1(43)
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 4.
C       (Ocean model data)
C
        IF (KSEC1(37).EQ.4) THEN
          WRITE(GRPRSM,9310) KSEC1(42)
          IF( KSEC1(40).NE.1090 ) WRITE(GRPRSM,9311) KSEC1(43)
          WRITE(GRPRSM,9401) KSEC1(44)
          WRITE(GRPRSM,9402) KSEC1(45)
C
          WRITE(GRPRSM,*) ' '
          WRITE(GRPRSM,*) 'Coordinate structure definition.'
          WRITE(GRPRSM,*) '--------------------------------'
          WRITE(GRPRSM,9403) KSEC1(46)
          WRITE(GRPRSM,9404) KSEC1(47)
          WRITE(GRPRSM,9405) KSEC1(48)
          WRITE(GRPRSM,9406) KSEC1(49)
          WRITE(GRPRSM,9407) KSEC1(50)
C
          WRITE(GRPRSM,*) ' '
          WRITE(GRPRSM,*) 'Position definition.'
          WRITE(GRPRSM,*) '--------------------'
          WRITE(GRPRSM,9411) KSEC1(51)
          WRITE(GRPRSM,9412) KSEC1(52)
          WRITE(GRPRSM,9413) KSEC1(53)
          WRITE(GRPRSM,9414) KSEC1(54)
          WRITE(GRPRSM,9415) KSEC1(55)
          WRITE(GRPRSM,9416) KSEC1(56)
          WRITE(GRPRSM,9413) KSEC1(57)
          WRITE(GRPRSM,9414) KSEC1(58)
          WRITE(GRPRSM,9415) KSEC1(59)
C
          WRITE(GRPRSM,*) ' '
          WRITE(GRPRSM,*) 'Grid Definition.'
          WRITE(GRPRSM,*) '----------------'
          WRITE(GRPRSM,9421) KSEC1(60)
          WRITE(GRPRSM,9422) KSEC1(61)
          WRITE(GRPRSM,9423) KSEC1(62)
          WRITE(GRPRSM,9424) KSEC1(63)
          WRITE(GRPRSM,9425) KSEC1(64)
          WRITE(GRPRSM,9426) KSEC1(65)
          WRITE(GRPRSM,9427) KSEC1(66)
          WRITE(GRPRSM,9428) KSEC1(67)
          WRITE(GRPRSM,9429) KSEC1(68)
          WRITE(GRPRSM,9430) KSEC1(69)
C
          WRITE(GRPRSM,*) ' '
          WRITE(GRPRSM,*) 'Further information.'
          WRITE(GRPRSM,*) '--------------------'
          WRITE(GRPRSM,9441) KSEC1(70)
C
          WRITE(GRPRSM,*) ' '
          WRITE(GRPRSM,*) 'Auxiliary information.'
          WRITE(GRPRSM,*) '----------------------'
          WRITE(GRPRSM,9451) KSEC1(71)
          WRITE(GRPRSM,9452) KSEC1(72)
          WRITE(GRPRSM,9453) KSEC1(73)
          WRITE(GRPRSM,9454) KSEC1(74)
C
          WRITE(GRPRSM,*) ' '
          WRITE(GRPRSM,*) 'Horizontal coordinate supplement.'
          WRITE(GRPRSM,*) '---------------------------------'
          IF( KSEC1(71).EQ.0 ) THEN
            WRITE(GRPRSM,*) '(None).'
          ELSE
            WRITE(GRPRSM,*) 'Number of items = ', KSEC1(71)
            DO JLOOP = 1,KSEC1(71)
              WRITE(GRPRSM,9461) KSEC1(74+JLOOP)
            ENDDO
          ENDIF
C
          WRITE(GRPRSM,*) ' '
          WRITE(GRPRSM,*) 'Mixed coordinate definition.'
          WRITE(GRPRSM,*) '----------------------------'
          IF( KSEC1(72).EQ.0 ) THEN
            WRITE(GRPRSM,*) '(None).'
          ELSE
            WRITE(GRPRSM,*) 'Number of items = ', KSEC1(72)
            IOFFSET = 74 + KSEC1(71)
            DO JLOOP = 1,KSEC1(72)
              WRITE(GRPRSM,9461) KSEC1(IOFFSET+JLOOP)
            ENDDO
          ENDIF
C
          WRITE(GRPRSM,*) ' '
          WRITE(GRPRSM,*) 'Grid coordinate list.'
          WRITE(GRPRSM,*) '---------------------'
          IF( KSEC1(73).EQ.0 ) THEN
            WRITE(GRPRSM,*) '(None).'
          ELSE
            WRITE(GRPRSM,*) 'Number of items = ', KSEC1(73)
            IOFFSET = 74 + KSEC1(71) + KSEC1(72)
            DO JLOOP = 1,KSEC1(73)
              WRITE(GRPRSM,9461) KSEC1(IOFFSET+JLOOP)
            ENDDO
          ENDIF
C
          WRITE(GRPRSM,*) ' '
          WRITE(GRPRSM,*) 'Auxiliary array.'
          WRITE(GRPRSM,*) '----------------'
          IF( KSEC1(74).EQ.0 ) THEN
            WRITE(GRPRSM,*) '(None).'
          ELSE
            WRITE(GRPRSM,*) 'Number of items = ', KSEC1(74)
            IOFFSET = 74 + KSEC1(71) + KSEC1(72) + KSEC1(73)
            DO JLOOP = 1,KSEC1(74)
              WRITE(GRPRSM,9461) KSEC1(IOFFSET+JLOOP)
            ENDDO
          ENDIF
C
          WRITE(GRPRSM,*) ' '
          WRITE(GRPRSM,*) 'Post-auxiliary array.'
          WRITE(GRPRSM,*) '---------------------'
          IOFFSET = 74 + KSEC1(71) + KSEC1(72) + KSEC1(73) + KSEC1(74)
          IF( KSEC1(IOFFSET+1).EQ.0 ) THEN
            WRITE(GRPRSM,*) '(None).'
          ELSE
            WRITE(GRPRSM,*) 'Number of items = ', KSEC1(IOFFSET+1)
            DO JLOOP = 2, 4
              WRITE(GRPRSM,9461) KSEC1(IOFFSET+JLOOP)
            ENDDO
            IF( KSEC1(IOFFSET+1).GT.4 ) THEN
              WRITE(GRPRSM,9462) KSEC1(IOFFSET+5)
              IF( KSEC1(IOFFSET+6).NE.0 ) THEN
                WRITE(GRPRSM,9463) KSEC1(IOFFSET+6)
                WRITE(GRPRSM,9464) KSEC1(IOFFSET+7)
              ENDIF
            ENDIF
          ENDIF
C
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 5.
C       (Forecast probability data)
C
        IF (KSEC1(37).EQ.5) THEN
          WRITE(GRPRSM,9500) KSEC1(42)
          WRITE(GRPRSM,9501) KSEC1(43)
          WRITE(GRPRSM,9502) KSEC1(44)
          WRITE(GRPRSM,9503) KSEC1(45)
          IF ( KSEC1(45) .NE. 2 ) WRITE(GRPRSM,9504) KSEC1(46)
          IF ( KSEC1(45) .NE. 1 ) WRITE(GRPRSM,9505) KSEC1(47)
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 6.
C       (Surface temperature data)
C
        IF (KSEC1(37).EQ.6) THEN
C
          IYEAR = KSEC1(44)
          IF( IYEAR.GT.100 ) THEN
            IF( IYEAR.LT.19000000 ) IYEAR = IYEAR + 19000000
            WRITE(GRPRSM,9601) IYEAR
          ELSE
            WRITE(GRPRSM,*)
     X        'Date of SST field used               Not given'
          ENDIF
          IF(KSEC1(45).EQ.0) WRITE(GRPRSM,9602) KSEC1(45)
          IF(KSEC1(45).EQ.1) WRITE(GRPRSM,9603) KSEC1(45)
          IF(KSEC1(45).EQ.2) WRITE(GRPRSM,9604) KSEC1(45)
C
          WRITE(GRPRSM,9605) KSEC1(46)
C
          DO 260 JLOOP = 1, KSEC1(46)
            IYEAR = KSEC1(45+(JLOOP*2))
            IF( IYEAR.GT.100 ) THEN
              IF( IYEAR.LT.19000000 ) IYEAR = IYEAR + 19000000
              WRITE(GRPRSM,9606) JLOOP, IYEAR
              WRITE(GRPRSM,9607) JLOOP, KSEC1(46+(JLOOP*2))
            ELSE
              WRITE(GRPRSM,*)
     X          'Date of SST field used               Not given'
            ENDIF
C
  260     CONTINUE
C
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 7.
C       (Sensitivity data)
C
        IF (KSEC1(37).EQ.7) THEN
          IF ( KSEC1(39) .EQ. 51 ) WRITE(GRPRSM,9700) KSEC1(42)
          IF ( KSEC1(39) .NE. 51 ) WRITE(GRPRSM,9701) KSEC1(42)
          IF ( KSEC1(39) .NE. 52 ) WRITE(GRPRSM,9702) KSEC1(43)
          IF ( KSEC1(39) .EQ. 52 ) WRITE(GRPRSM,9703) KSEC1(43)
          WRITE(GRPRSM,9704) KSEC1(44) 
          WRITE(GRPRSM,9705) KSEC1(45)
          GOTO 900
        ENDIF
C
C
C       ECMWF Local definition 8.
C       (ECMWF re-analysis data)
C
        IF (KSEC1(37).EQ.8) THEN
          IF( (KSEC1(40).EQ.1043).OR.
     X        (KSEC1(40).EQ.1070).OR.
     X        (KSEC1(40).EQ.1071) ) THEN
            WRITE(GRPRSM,9801) KSEC1(42)
            DO JLOOP = 43, 54
              JILOOP = JLOOP + 8
              WRITE(GRPRSM,9800) JILOOP, KSEC1(JLOOP)
            ENDDO
          ELSE
            DO JLOOP = 42, 54
              JILOOP = JLOOP + 8
              WRITE(GRPRSM,9800) JILOOP, KSEC1(JLOOP)
            ENDDO
          ENDIF
          GOTO 900
        ENDIF
C
        IF (KSEC1(39).GT.4.AND.KSEC1(39).LT.9) THEN
          WRITE(GRPRSM,9308) KSEC1(42)
          WRITE(GRPRSM,9309) KSEC1(43)
        ENDIF
C
C
C       ECMWF Local definition 9.
C       (Singular vectors and ensemble perturbations)
C
        IF (KSEC1(37).EQ.9) THEN
          IF(KSEC1(39).EQ.60)
     X      WRITE(GRPRSM,9960) KSEC1(42)
          IF(KSEC1(39).EQ.61)
     X      WRITE(GRPRSM,9961) KSEC1(42)
          IF(KSEC1(39).EQ.62)
     X      WRITE(GRPRSM,9962) KSEC1(42)
          IF(KSEC1(39).EQ.62) THEN
            WRITE(GRPRSM,9943) KSEC1(43)
            WRITE(GRPRSM,9944) KSEC1(44)
            WRITE(GRPRSM,9945) KSEC1(45)
            WRITE(GRPRSM,9946) KSEC1(46)
            WRITE(GRPRSM,9947) KSEC1(47)
            WRITE(GRPRSM,9948) KSEC1(48)
            WRITE(GRPRSM,9949) KSEC1(49)
            WRITE(GRPRSM,9950) KSEC1(50)
            WRITE(GRPRSM,9951) KSEC1(51)
            WRITE(GRPRSM,9952) KSEC1(52)
            WRITE(GRPRSM,9953) KSEC1(53)
            WRITE(GRPRSM,9954) KSEC1(54)
            WRITE(GRPRSM,9955) KSEC1(55)
          ENDIF
          GOTO 900
        ENDIF
C
C
C       ECMWF Local definition 10.
C       (EPS tubes)
C
        IF (KSEC1(37).EQ.10) THEN
          WRITE(GRPRSM,1001) KSEC1(42)
          WRITE(GRPRSM,1002) KSEC1(43)
          WRITE(GRPRSM,1003) KSEC1(44)
          WRITE(GRPRSM,1004) KSEC1(45)
          WRITE(GRPRSM,1005) KSEC1(46)
          WRITE(GRPRSM,1006) KSEC1(47)
          WRITE(GRPRSM,1007) KSEC1(48)
          WRITE(GRPRSM,1008) KSEC1(49)
          WRITE(GRPRSM,1009) KSEC1(50)
          WRITE(GRPRSM,1010) KSEC1(51)
          WRITE(GRPRSM,1011) KSEC1(52)
          WRITE(GRPRSM,1012) KSEC1(53)
          WRITE(GRPRSM,1013) KSEC1(54)
          WRITE(GRPRSM,1014) KSEC1(55)
          WRITE(GRPRSM,1015) KSEC1(56)
          WRITE(GRPRSM,1016) KSEC1(57)
          WRITE(GRPRSM,1017) KSEC1(58)
C
          WRITE(GRPRSM,1018)
          DO 280 JLOOP = 1, KSEC1(58)
            WRITE(GRPRSM,1019) KSEC1(58+JLOOP)
  280     CONTINUE
          GOTO 900
        ENDIF
C
C
C       ECMWF Local definition 11.
C       (Supplementary data used by the analysis)
C
        IF (KSEC1(37).EQ.11) THEN
          WRITE(GRPRSM,1101)
          WRITE(GRPRSM,1103) KSEC1(42)
          WRITE(GRPRSM,1104) KSEC1(43)
          WRITE(GRPRSM,1105) KSEC1(44)
          WRITE(HVERSION, '(A8)' ) KSEC1(45)
          WRITE(GRPRSM,1106)
     X      HVERSION(5:5),
     X      HVERSION(6:6),
     X      HVERSION(7:7),
     X      HVERSION(8:8)
          IYEAR = KSEC1(46)
          IF( IYEAR.GT.40 ) THEN
            IYEAR = IYEAR + 1900
          ELSE
            IYEAR = IYEAR + 2000
          ENDIF
          WRITE(GRPRSM,1107) KSEC1(46), IYEAR
          WRITE(GRPRSM,1108) KSEC1(47)
          WRITE(GRPRSM,1109) KSEC1(48)
          WRITE(GRPRSM,1110) KSEC1(49)
          WRITE(GRPRSM,1111) KSEC1(50)
          WRITE(GRPRSM,1112) KSEC1(51)
          WRITE(GRPRSM,1113) KSEC1(52)
          WRITE(GRPRSM,1114) KSEC1(53)
          GOTO 900
        ENDIF
C
C
C       ECMWF Local definition 12.
C
        IF (KSEC1(37).EQ.12) THEN
          WRITE(GRPRSM,1201)
          WRITE(GRPRSM,1203) KSEC1(42)
          WRITE(GRPRSM,1204) KSEC1(43)
          WRITE(GRPRSM,1205) KSEC1(44)
          WRITE(GRPRSM,1206) KSEC1(45)
          WRITE(GRPRSM,1207) KSEC1(46)
          WRITE(GRPRSM,1208) KSEC1(47)
          WRITE(GRPRSM,1209) KSEC1(48)
          WRITE(GRPRSM,1210) KSEC1(49)
          WRITE(GRPRSM,1211)
          IPREV  = KSEC1(50)
          ICURR  = 0
          ICOUNT = 0
          DO JLOOP = 1, KSEC1(49)
            ICURR = KSEC1(49+JLOOP)
            IF( ICURR.NE.IPREV ) THEN
              IF( ICOUNT.EQ.1 ) WRITE(GRPRSM,1212) IPREV
              IF( ICOUNT.EQ.2 ) WRITE(GRPRSM,1213) IPREV
              IF( ICOUNT.GT.2 ) WRITE(GRPRSM,1214) IPREV, ICOUNT
              IPREV  = ICURR
              ICOUNT = 1
            ELSE
              ICOUNT = ICOUNT + 1
            ENDIF
          ENDDO
          IF( ICOUNT.EQ.1 ) WRITE(GRPRSM,1212) IPREV
          IF( ICOUNT.EQ.2 ) WRITE(GRPRSM,1213) IPREV
          IF( ICOUNT.GT.2 ) WRITE(GRPRSM,1214) IPREV, ICOUNT
          GOTO 900
        ENDIF
C
C
C       ECMWF Local definition 13.
C       (Wave 2D spectra direction and frequency)
C
        IF (KSEC1(37).EQ.13) THEN
          WRITE(GRPRSM,1301) KSEC1(44)
          WRITE(GRPRSM,1302) KSEC1(45)
          WRITE(GRPRSM,1303) KSEC1(46)
          WRITE(GRPRSM,1304) KSEC1(47)
          WRITE(GRPRSM,1305) KSEC1(48)
          WRITE(GRPRSM,1306) KSEC1(49)
          WRITE(GRPRSM,1307)
          DO 290 JLOOP = 1, KSEC1(46)
            VALUE = REAL(KSEC1(49+JLOOP))/REAL(KSEC1(48))
            IF( KSEC1(44).EQ.JLOOP ) THEN
              WRITE(GRPRSM,1308) JLOOP, VALUE
            ELSE
              WRITE(GRPRSM,1309) JLOOP, VALUE
            ENDIF
  290     CONTINUE
          WRITE(GRPRSM,1310)
          DO 295 JLOOP = 1, KSEC1(47)
            VALUE = REAL(KSEC1(49+KSEC1(46)+JLOOP))/REAL(KSEC1(49))
            IF( KSEC1(45).EQ.JLOOP ) THEN
              WRITE(GRPRSM,1308) JLOOP, VALUE
            ELSE
              WRITE(GRPRSM,1309) JLOOP, VALUE
            ENDIF
  295     CONTINUE
C
          IF( N13FLAG.GT.0 ) THEN
            WRITE(GRPRSM,1311) KSEC1(50+KSEC1(46)+KSEC1(47))
            WRITE(GRPRSM,1312) KSEC1(51+KSEC1(46)+KSEC1(47))
            IF( N13FLAG.GT.1 ) THEN
              WRITE(GRPRSM,1313) KSEC1(52+KSEC1(46)+KSEC1(47))
              WRITE(GRPRSM,1314) KSEC1(53+KSEC1(46)+KSEC1(47))
              WRITE(GRPRSM,1315) KSEC1(54+KSEC1(46)+KSEC1(47))
              IF( N13FLAG.GT.2 ) THEN
                WRITE(GRPRSM,1316) KSEC1(55+KSEC1(46)+KSEC1(47))
                WRITE(GRPRSM,1317) KSEC1(56+KSEC1(46)+KSEC1(47))
                WRITE(GRPRSM,1318) KSEC1(57+KSEC1(46)+KSEC1(47))
                WRITE(GRPRSM,1319) KSEC1(58+KSEC1(46)+KSEC1(47))
              ENDIF
            ENDIF
          ENDIF
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 14.
C       (Brightness temperature)
C
        IF (KSEC1(37).EQ.14) THEN
          WRITE(GRPRSM,1401) KSEC1(44)
          WRITE(GRPRSM,1402) KSEC1(45)
          WRITE(GRPRSM,1403) KSEC1(46)
          WRITE(GRPRSM,1404)
          DO 296 JLOOP = 1, KSEC1(46)
            VALUE = REAL(KSEC1(46+JLOOP))/REAL(KSEC1(45))
            IF( KSEC1(44).EQ.JLOOP ) THEN
              WRITE(GRPRSM,1405) JLOOP, VALUE
            ELSE
              WRITE(GRPRSM,1406) JLOOP, VALUE
            ENDIF
  296     CONTINUE
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 15.
C       (Ocean ensemble seasonal forecast)
C
        IF (KSEC1(37).EQ.15) THEN
          WRITE(GRPRSM,1501) KSEC1(42)
          WRITE(GRPRSM,1504) KSEC1(43)
          WRITE(GRPRSM,1502) KSEC1(44)
          WRITE(GRPRSM,1503) KSEC1(45)
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 16.
C       (Seasonal forecast monthly mean atmosphere data)
C
        IF (KSEC1(37).EQ.16) THEN
          WRITE(GRPRSM,1501) KSEC1(42)
          WRITE(GRPRSM,1502) KSEC1(44)
          WRITE(GRPRSM,1503) KSEC1(45)
          WRITE(GRPRSM,1601) KSEC1(46)
          WRITE(GRPRSM,1602) KSEC1(47)
          WRITE(GRPRSM,1603) KSEC1(48)
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 17.
C       (Sst or sea-ice used by analysis)
C
        IF (KSEC1(37).EQ.17) THEN
C
          IYEAR = KSEC1(44)
          IF( IYEAR.GT.100 ) THEN
            IF( IYEAR.LT.19000000 ) IYEAR = IYEAR + 19000000
            WRITE(GRPRSM,1701) IYEAR
          ELSE
            WRITE(GRPRSM,*)
     X        'Date of sst/ice field used           Not given'
          ENDIF
          IF(KSEC1(45).EQ.0) WRITE(GRPRSM,1702) KSEC1(45)
          IF(KSEC1(45).EQ.1) WRITE(GRPRSM,1703) KSEC1(45)
          IF(KSEC1(45).EQ.2) WRITE(GRPRSM,1704) KSEC1(45)
C
          WRITE(GRPRSM,1705) KSEC1(46)
C
          DO JLOOP = 1, KSEC1(46)
            IYEAR = KSEC1(45+(JLOOP*2))
            IF( IYEAR.GT.100 ) THEN
              IF( IYEAR.LT.19000000 ) IYEAR = IYEAR + 19000000
              WRITE(GRPRSM,1706) JLOOP, IYEAR
              WRITE(GRPRSM,1707) JLOOP, KSEC1(46+(JLOOP*2))
            ELSE
              WRITE(GRPRSM,*)
     X          'Date of sst/ice field used           Not given'
            ENDIF
C
          ENDDO
          GOTO 900
C
        ENDIF
C
C       ECMWF Local definition 18.
C       (Multi-analysis ensemble data)
C
        IF (KSEC1(37).EQ.18) THEN
          WRITE(GRPRSM,1801) KSEC1(42)
          WRITE(GRPRSM,1802) KSEC1(43)
          WRITE(GRPRSM,1803) KSEC1(44)
          WRITE(HVERSION, '(A8)' ) KSEC1(45)
          WRITE(GRPRSM,1804)
     X      HVERSION(5:5),
     X      HVERSION(6:6),
     X      HVERSION(7:7),
     X      HVERSION(8:8)
          WRITE(GRPRSM,1805) KSEC1(46)
          WRITE(GRPRSM,*) '  WMO centre analyses used:'
          WRITE(GRPRSM,*) '  ------------------------'
          DO JLOOP = 1, KSEC1(46)
            WRITE(HVERSION, '(A8)' ) KSEC1(46+JLOOP)
            WRITE(GRPRSM,1806)
     X      HVERSION(5:5),
     X      HVERSION(6:6),
     X      HVERSION(7:7),
     X      HVERSION(8:8)
          ENDDO
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 19.
C       (Extreme forecast index)
C
        IF (KSEC1(37).EQ.19) THEN
          WRITE(GRPRSM,1901) KSEC1(42)
          WRITE(GRPRSM,1902) KSEC1(43)
          WRITE(GRPRSM,1903) KSEC1(44)
          WRITE(GRPRSM,1904) KSEC1(45)
          WRITE(GRPRSM,1905) KSEC1(46)
          WRITE(GRPRSM,1906) KSEC1(47)
          WRITE(GRPRSM,1907) KSEC1(48)
          WRITE(GRPRSM,1908) KSEC1(49)
          WRITE(GRPRSM,1909) KSEC1(50)
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 20.
C       (4D variational increments)
C
        IF (KSEC1(37).EQ.20) THEN
          WRITE(GRPRSM,2001) KSEC1(42)
          WRITE(GRPRSM,2002) KSEC1(43)
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 21.
C       (Sensitive area predictions)
C
        IF (KSEC1(37).EQ.21) THEN
          WRITE(GRPRSM,2101) KSEC1(42)
          WRITE(GRPRSM,2102) KSEC1(43)
          WRITE(GRPRSM,2103) KSEC1(44)
          WRITE(GRPRSM,2104) KSEC1(45)
          WRITE(GRPRSM,2105) KSEC1(46)
          WRITE(GRPRSM,2106) KSEC1(47)
          WRITE(GRPRSM,2107) KSEC1(48)
          WRITE(GRPRSM,2108) KSEC1(49)
          WRITE(GRPRSM,2109) KSEC1(50)
          WRITE(GRPRSM,2110) KSEC1(51)
          WRITE(GRPRSM,2111) KSEC1(52)
          WRITE(GRPRSM,2112) KSEC1(53)
          WRITE(GRPRSM,2113) KSEC1(54)
          WRITE(GRPRSM,2114) KSEC1(55)
          WRITE(GRPRSM,2115) KSEC1(56)
          WRITE(GRPRSM,2116) KSEC1(57)
          WRITE(HVERSION, '(A8)' ) KSEC1(58)
#ifdef LITTLE_ENDIAN
          WRITE(GRPRSM,2117) HVERSION(5:5)
#else
          WRITE(GRPRSM,2117) HVERSION(8:8)
#endif
          WRITE(GRPRSM,2118) KSEC1(59)
          WRITE(GRPRSM,2119) KSEC1(60)
          WRITE(GRPRSM,2120) KSEC1(61)
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 22.
C       (Couple atmospheric, wave and ocean models)
C
        IF (KSEC1(37).EQ.22) THEN
          WRITE(GRPRSM,1501) KSEC1(42)
          WRITE(GRPRSM,1504) KSEC1(43)
          WRITE(GRPRSM,1502) KSEC1(44)
          WRITE(GRPRSM,1503) KSEC1(45)
          WRITE(GRPRSM,2201) KSEC1(46)
          WRITE(GRPRSM,2202) KSEC1(47)
          WRITE(GRPRSM,2203) KSEC1(48)
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 23.
C       (Couple atmospheric, wave and ocean means)
C
        IF (KSEC1(37).EQ.23) THEN
          WRITE(GRPRSM,1501) KSEC1(42)
          WRITE(GRPRSM,1504) KSEC1(43)
          WRITE(GRPRSM,1502) KSEC1(44)
          WRITE(GRPRSM,1503) KSEC1(45)
          WRITE(GRPRSM,1601) KSEC1(46)
          WRITE(GRPRSM,1602) KSEC1(47)
          WRITE(GRPRSM,1603) KSEC1(48)
          WRITE(GRPRSM,2201) KSEC1(49)
          WRITE(GRPRSM,2202) KSEC1(50)
          WRITE(GRPRSM,2203) KSEC1(51)
          WRITE(GRPRSM,9502) KSEC1(52)
          WRITE(GRPRSM,9503) KSEC1(53)
          IF ( KSEC1(53) .NE. 2 ) WRITE(GRPRSM,9504) KSEC1(54)
          IF ( KSEC1(53) .NE. 1 ) WRITE(GRPRSM,9505) KSEC1(55)
C
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 24.
C       (Couple atmospheric, wave and ocean means)
C
        IF (KSEC1(37).EQ.24) THEN
          WRITE(GRPRSM,2401) KSEC1(42)
          WRITE(GRPRSM,2402) KSEC1(43)
          WRITE(GRPRSM,2403) KSEC1(44)
          WRITE(GRPRSM,2404) KSEC1(45)
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 25.
C       (Couple atmospheric, wave and ocean means)
C
        IF (KSEC1(37).EQ.25) THEN
          WRITE(GRPRSM,2501) KSEC1(42)
          WRITE(GRPRSM,2502) KSEC1(43)
          WRITE(GRPRSM,2503) KSEC1(44)
        ENDIF
C
C       ECMWF Local definition 50.
C       (Member State data)
C
        IF (KSEC1(37).EQ.50) THEN
          WRITE(GRPRSM,5001) KSEC1(42)
          WRITE(GRPRSM,5002) KSEC1(43)
          WRITE(GRPRSM,5003) KSEC1(44)
          WRITE(GRPRSM,5004) KSEC1(45)
          WRITE(GRPRSM,5005) KSEC1(46)
          WRITE(GRPRSM,5006) KSEC1(47)
          WRITE(GRPRSM,5007) KSEC1(48)
          WRITE(GRPRSM,*) '  ECMWF additions'
          WRITE(GRPRSM,*) '  ---------------'
          WRITE(GRPRSM,5008) KSEC1(49)
          WRITE(GRPRSM,5009) KSEC1(50)
          WRITE(GRPRSM,5010) (KSEC1(JLOOP),JLOOP=51,60)
          WRITE(GRPRSM,*) '  Optional data'
          WRITE(GRPRSM,*) '  -------------'
          WRITE(GRPRSM,5010) (KSEC1(JLOOP),JLOOP=61,106)
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 190.
C       (Multiple ECMWF local definitions)
C
        IF (KSEC1(37).EQ.190) THEN
          WRITE(GRPRSM,19001) KSEC1(44)
          NBYTES = 0
          DO JLOOP = 1, KSEC1(44)
            WRITE(GRPRSM,19002) KSEC1(43+JLOOP*2)
            WRITE(GRPRSM,19003) KSEC1(44+JLOOP*2)
            NBYTES = NBYTES + KSEC1(44+2*JLOOP) + 8
          ENDDO
          WRITE(GRPRSM,*) ' '
          WRITE(GRPRSM,*) ' Internal local definitions:'
          WRITE(GRPRSM,*) ' ---------------------------'
          CALL GRPR190(KSEC1)
          GOTO 900
        ENDIF
C
C       ECMWF Local definition 191.
C       (Free format data descriptor)
C
        IF (KSEC1(37).EQ.191) THEN
          WRITE(GRPRSM,19101) KSEC1(44)
          WRITE(GRPRSM,19102) KSEC1(45)
          WRITE(GRPRSM,19103) KSEC1(46)
          WRITE(GRPRSM,19108) KSEC1(51)
          WRITE(GRPRSM,*) '  Data descriptor bytes'
          WRITE(GRPRSM,*) '  ---------------------'
          DO JLOOP = 52, (51+(KSEC1(51)+JPBYTES-1)/JPBYTES)
            WRITE(GRPRSM,19109) KSEC1(JLOOP)
          ENDDO
          GOTO 900
        ENDIF
C
C       ECMWF Local definition not in above selection
C       (eg defined using templates)
C
        IF( NDBG.EQ.0 ) CALL LDEFPRT(GRPRSM,KSEC1)
        GOTO 900
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3 . Print Washington ensemble product information.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
C     Washington EPS products (but not reformatted Washington EPS
C     products.
      IF ((KSEC1(2).EQ.7.AND.KSEC1(24).EQ.1).AND.
     X    (.NOT.KSEC1(22).EQ.98) ) THEN
        CALL KWPRS1 (KSEC0,KSEC1)
      ENDIF
C
C
C     -----------------------------------------------------------------|
C*    Section 9 . Format statements. Return to calling routine.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
C
 9000 FORMAT(' ')
 9001 FORMAT(' Section 1 - Product Definition Section.')
 9002 FORMAT(' ---------------------------------------')
 9100 FORMAT(' Code Table 2 Version Number.         ',I9)
 9101 FORMAT(' Originating centre identifier.       ',I9)
 9102 FORMAT(' Model identification.                ',I9)
 9103 FORMAT(' Grid definition.                     ',I9)
 9104 FORMAT(' Flag (Code Table 1)                   ',I8.8)
 9105 FORMAT(' Parameter identifier (Code Table 2). ',I9)
 9106 FORMAT(' Type of level (Code Table 3).        ',I9)
 9107 FORMAT(' Value 1 of level (Code Table 3).     ',I9)
 9108 FORMAT(' Value 2 of level (Code Table 3).     ',I9)
 9109 FORMAT(' Year of reference time of data.      ',I9,'  (',I4,')' )
 9110 FORMAT(' Month of reference time of data.     ',I9)
 9111 FORMAT(' Day of reference time of data.       ',I9)
 9112 FORMAT(' Hour of reference time of data.      ',I9)
 9113 FORMAT(' Minute of reference time of data.    ',I9)
 9114 FORMAT(' Time unit (Code Table 4).            ',I9)
 9115 FORMAT(' Time range one.                      ',I9)
 9116 FORMAT(' Time range two.                      ',I9)
 9117 FORMAT(' Time range indicator (Code Table 5)  ',I9)
 9118 FORMAT(' Number averaged.                     ',I9)
 9119 FORMAT(' Number missing from average.         ',I9)
 9120 FORMAT(' Century of reference time of data.   ',I9)
 9121 FORMAT(' Units decimal scaling factor.        ',I9)
 9122 FORMAT(' Century of reference time of data.   Not given')
 9123 FORMAT(' Sub-centre identifier.               ',I9)
 9206 FORMAT(' Satellite identifier.                ',I9)
 9207 FORMAT(' Spectral band.                       ',I9)
C
 9301 FORMAT(' ECMWF local usage identifier.        ',I9)
 9302 FORMAT(' Class.                               ',I9)
 9303 FORMAT(' Type.                                ',I9)
 9304 FORMAT(' Stream.                              ',I9)
 9305 FORMAT(' Version number or Experiment identifier.  ',A1,A1,A1,A1)
 9306 FORMAT(' Satellite spectral band.             ',I9)
 9307 FORMAT(' Function code.                       ',I9)
 9308 FORMAT(' Simulation number.                   ',I9)
 9309 FORMAT(' Total number of simulations.         ',I9)
 9310 FORMAT(' Forecast number.                     ',I9)
 9311 FORMAT(' Total number of forecasts.           ',I9)
 9312 FORMAT(' Cluster number.                      ',I9)
 9313 FORMAT(' Total number of clusters.            ',I9)
 9314 FORMAT(' Clustering method.                   ',I9)
 9315 FORMAT(' Start time step when clustering.     ',I9)
 9316 FORMAT(' End time step when clustering.       ',I9)
 9317 FORMAT(' Northern latitude of domain.         ',I9)
 9318 FORMAT(' Western longitude of domain.         ',I9)
 9319 FORMAT(' Southern latitude of domain.         ',I9)
 9320 FORMAT(' Eastern longitude of domain.         ',I9)
 9321 FORMAT(' Operational forecast in cluster      ',I9)
 9322 FORMAT(' Number of forecasts in cluster.      ',I9)
 9323 FORMAT(' Forecast number                      ',I9)
 9324 FORMAT(' Control forecast in cluster          ',I9)
C
C       ECMWF Local definition 4.
C
 9401 FORMAT(' System number.                       ',I9)
 9402 FORMAT(' Method number.                       ',I9)
 9403 FORMAT(' Space unit flag.                     ',I9)
 9404 FORMAT(' Vertical coordinate definition.      ',I9)
 9405 FORMAT(' Horizontal coordinate definition.    ',I9)
 9406 FORMAT(' Time unit flag.                      ',I9)
 9407 FORMAT(' Time coordinate definition.          ',I9)
 9408 FORMAT(' Method used for analysis or forecast.',I9)
C
 9411 FORMAT(' Mixed coordinate field flag.         ',I9)
 9412 FORMAT(' Coordinate 1 flag.                   ',I9)
 9413 FORMAT(' Averaging flag.                      ',I9)
 9414 FORMAT(' Position of level 1.                 ',I9)
 9415 FORMAT(' Position of level 2.                 ',I9)
 9416 FORMAT(' Coordinate 2 flag.                   ',I9)
C
 9421 FORMAT(' Coordinate 3 flag (x-axis)           ',I9)
 9422 FORMAT(' Coordinate 4 flag (y-axis)           ',I9)
 9423 FORMAT(' Coordinate 4 of first grid point.   ',I10)
 9424 FORMAT(' Coordinate 3 of first grid point.   ',I10)
 9425 FORMAT(' Coordinate 4 of last grid point.    ',I10)
 9426 FORMAT(' Coordinate 3 of last grid point.    ',I10)
 9427 FORMAT(' i - increment.                       ',I9)
 9428 FORMAT(' j - increment.                       ',I9)
 9429 FORMAT(' Flag for irregular grid coordinates. ',I9)
 9430 FORMAT(' Flag for normal or staggered grids.  ',I9)
C
 9441 FORMAT(' Further information flag.            ',I9)
C
 9451 FORMAT(' No. entries in horizontal coordinate ',I9)
 9452 FORMAT(' No. entries in mixed coordinate defn.',I9)
 9453 FORMAT(' No. entries in grid coordinate list. ',I9)
 9454 FORMAT(' No. entries in auxiliary array.      ',I9)
C
 9461 FORMAT('  ',I12)
 9462 FORMAT(' Reference date                       ',I9)
 9463 FORMAT(' Climate date - from                  ',I9)
 9464 FORMAT(' Climate date - to                    ',I9)
C
C       ECMWF Local definition 5.
C
 9500 FORMAT(' Forecast probability number          ',I9)
 9501 FORMAT(' Total number of forecast probabilities ',I7)
 9502 FORMAT(' Threshold units decimal scale factor ',I9)
 9503 FORMAT(' Threshold indicator(1=lower,2=upper,3=both) ',I2)
 9504 FORMAT(' Lower threshold value                ',I9)
 9505 FORMAT(' Upper threshold value                ',I9)
C
C       ECMWF Local definition 6.
C
 9601 FORMAT(' Date of SST field used               ',I9)
 9602 FORMAT(' Type of SST field (= climatology)    ',I9)
 9603 FORMAT(' Type of SST field (= 1/1 degree)     ',I9)
 9604 FORMAT(' Type of SST field (= 2/2 degree)     ',I9)
 9605 FORMAT(' Number of ICE fields used:           ',I9)
 9606 FORMAT(' Date of ICE field',I3,'                 ',I9)
 9607 FORMAT(' Satellite number (ICE field',I3,')      ',I9)
C
C       ECMWF Local definition 7.
C
 9700 FORMAT(' Forecast number                      ',I9)
 9701 FORMAT(' Iteration number                     ',I9)
 9702 FORMAT(' Total number of diagnostics          ',I9)
 9703 FORMAT(' No.interations in diag. minimisation ',I9)
 9704 FORMAT(' Domain(0=Global,1=Europe,2=N.Hem.,3=S.Hem.) ',I2)
 9705 FORMAT(' Diagnostic number                    ',I9)
C
C       ECMWF Local definition 8.
C
 9800 FORMAT(' ERA section 1 octet ',I2,'.              ',I9)
 9801 FORMAT(' Interval between reference times     ',I9)
C
C       ECMWF Local definition 9.
C
 9943 FORMAT(' Number of iterations                 ',I9)
 9944 FORMAT(' Number of singular vectors computed  ',I9)
 9945 FORMAT(' Norm used at initial time            ',I9)
 9946 FORMAT(' Norm used at final time              ',I9)
 9947 FORMAT(' Multiplication factor                ',I9)
 9948 FORMAT(' Latitude of north-west corner        ',I9)
 9949 FORMAT(' Longitude of north-west corner       ',I9)
 9950 FORMAT(' Latitude of south-east corner        ',I9)
 9951 FORMAT(' Longitude of south-east corner       ',I9)
 9952 FORMAT(' Accuracy                             ',I9)
 9953 FORMAT(' Number of singular vectors evolved   ',I9)
 9954 FORMAT(' Ritz number one                      ',I9)
 9955 FORMAT(' Ritz number two                      ',I9)
 9960 FORMAT(' Perturbed ensemble forecast number   ',I9)
 9961 FORMAT(' Initial state perturbation number    ',I9)
 9962 FORMAT(' Singular vector number               ',I9)
C
C       ECMWF Local definition 10.
C
 1001 FORMAT(' Tube number                          ',I9)
 1002 FORMAT(' Total number of tubes                ',I9)
 1003 FORMAT(' Central cluster definition           ',I9)
 1004 FORMAT(' Parameter                            ',I9)
 1005 FORMAT(' Type of level                        ',I9)
 1006 FORMAT(' Northern latitude of domain of tubing',I9)
 1007 FORMAT(' Western longitude of domain of tubing',I9)
 1008 FORMAT(' Southern latitude of domain of tubing',I9)
 1009 FORMAT(' Eastern longitude of domain of tubing',I9)
 1010 FORMAT(' Tube number of operational forecast  ',I9)
 1011 FORMAT(' Tube number of control forecast      ',I9)
 1012 FORMAT(' Height/pressure of level             ',I9)
 1013 FORMAT(' Reference step                       ',I9)
 1014 FORMAT(' Radius of central cluster            ',I9)
 1015 FORMAT(' Ensemble standard deviation          ',I9)
 1016 FORMAT(' Dist.of tube extreme to ensemble mean',I9)
 1017 FORMAT(' Number of forecasts in the tube      ',I9)
 1018 FORMAT(' List of ensemble forecast numbers:')
 1019 FORMAT('    ',I9)
C
C       ECMWF Local definition 11.
C
 1101 FORMAT(' Details of analysis which used the supplementary data:')
 1103 FORMAT('   Class                              ',I9)
 1104 FORMAT('   Type                               ',I9)
 1105 FORMAT('   Stream                             ',I9)
 1106 FORMAT('   Version number/experiment identifier:   ',A1,A1,A1,A1)
 1107 FORMAT('   Year                               ',I9,'  (',I4,')')
 1108 FORMAT('   Month                              ',I9)
 1109 FORMAT('   Day                                ',I9)
 1110 FORMAT('   Hour                               ',I9)
 1111 FORMAT('   Minute                             ',I9)
 1112 FORMAT('   Century                            ',I9)
 1113 FORMAT('   Originating centre                 ',I9)
 1114 FORMAT('   Sub-centre                         ',I9)
C
C       ECMWF Local definition 12.
C
 1201 FORMAT(' (Mean, average, etc)') 
 1203 FORMAT(' Start date of the period              ', I8)
 1204 FORMAT(' Start time of the period                  ', I4.4)
 1205 FORMAT(' Finish date of the period             ', I8)
 1206 FORMAT(' Finish time of the period                 ', I4.4)
 1207 FORMAT(' Verifying date of the period          ', I8)
 1208 FORMAT(' Verifying time of the period              ', I4.4)
 1209 FORMAT(' Code showing method                   ', I8)
 1210 FORMAT(' Number of different time intervals used  ', I5)
 1211 FORMAT(' List of different time intervals used:')
 1212 FORMAT('  - interval ',I5.4, ' used       once')
 1213 FORMAT('  - interval ',I5.4, ' used       twice')
 1214 FORMAT('  - interval ',I5.4, ' used ', I5, ' times')

C
C       ECMWF Local definition 13.
C
 1301 FORMAT(' Direction number                     ',I9)
 1302 FORMAT(' Frequency number                     ',I9)
 1303 FORMAT(' Total number of directions           ',I9)
 1304 FORMAT(' Total number of frequencies          ',I9)
 1305 FORMAT(' Scale factor applied to directions   ',I9)
 1306 FORMAT(' Scale factor applied to frequencies  ',I9)
 1307 FORMAT(' List of directions:')
 1308 FORMAT(' ', I2.2,':', F15.7, '   <-- this field value')
 1309 FORMAT(' ', I2.2,':', F15.7)
 1310 FORMAT(' List of frequencies:')
 1311 FORMAT(' System number (65535 = missing)      ',I9)
 1312 FORMAT(' Method number (65535 = missing)      ',I9)
 1313 FORMAT(' Reference date                       ',I9)
 1314 FORMAT(' Climate date (from)                  ',I9)
 1315 FORMAT(' Climate date (to)                    ',I9)
 1316 FORMAT(' Leg base Date                        ',I9)
 1317 FORMAT(' Leg base Time                        ',I9)
 1318 FORMAT(' Leg base Number                      ',I9)
 1319 FORMAT(' Ocean Atmosphere Coupling flag       ',I9)
C
C       ECMWF Local definition 14.
C
 1401 FORMAT(' Channel number                       ',I9)
 1402 FORMAT(' Scale factor applied to frequencies  ',I9)
 1403 FORMAT(' Total number of frequencies          ',I9)
 1404 FORMAT(' List of frequencies:')
 1405 FORMAT(' ', I3,':', F15.9, '   <-- this channel')
 1406 FORMAT(' ', I3,':', F15.9)
C
C       ECMWF Local definition 15.
C
 1501 FORMAT(' Ensemble member number               ',I9)
 1502 FORMAT(' System number                        ',I9)
 1503 FORMAT(' Method number                        ',I9)
 1504 FORMAT(' Total number of ensembles            ',I9)
C
C       ECMWF Local definition 16.
C
 1601 FORMAT(' Verifying month                      ',I9)
 1602 FORMAT(' Averaging period                     ',I9)
 1603 FORMAT(' Forecast month (0 = not present)     ',I9)
C
C       ECMWF Local definition 17.
C
 1701 FORMAT(' Date of sst/ice field used           ',I9)
 1702 FORMAT(' Type of sst/ice field (= climatology)',I9)
 1703 FORMAT(' Type of sst/ice field (= 1/1 degree) ',I9)
 1704 FORMAT(' Type of sst/ice field (= 2/2 degree) ',I9)
 1705 FORMAT(' Number of ICE fields used:           ',I9)
 1706 FORMAT(' Date of ICE field',I3,'                 ',I9)
 1707 FORMAT(' Satellite number (ICE field',I3,')      ',I9)
C
C       ECMWF Local definition 18.
C
 1801 FORMAT(' Ensemble forecast number             ',I9)
 1802 FORMAT(' Number of forecasts in ensemble      ',I9)
 1803 FORMAT(' Data origin                          ',I9)
 1804 FORMAT(' Model identifier of forecast used         ',A1,A1,A1,A1)
 1805 FORMAT(' Number of analyses in start analysis ',I9)
 1806 FORMAT('   ',A1,A1,A1,A1)
C
C       ECMWF Local definition 19.
C
 1901 FORMAT(' Zero, for Mars compatibility         ',I9)
 1902 FORMAT(' Ensemble size                        ',I9)
 1903 FORMAT(' Power of 10 used to scale weight (F) ',I9)
 1904 FORMAT(' Weight*10**F applied to climate month 1',I7)
 1905 FORMAT(' First month used for climate month 1 ',I9)
 1906 FORMAT(' Last month used for climate month 1  ',I9)
 1907 FORMAT(' First month used for climate month 2 ',I9)
 1908 FORMAT(' Last month used for climate month 2  ',I9)
 1909 FORMAT(' EFI order                            ',I9)
C
C       ECMWF Local definition 20.
C
 2001 FORMAT(' Iteration                            ',I9)
 2002 FORMAT(' Total number of iterations           ',I9)
C
C       ECMWF Local definition 21.
C
 2101 FORMAT(' Forecast/sing.vector/flight number   ',I9)
 2102 FORMAT(' Number of iterations                 ',I9)
 2103 FORMAT(' Number of singular vectors computed  ',I9)
 2104 FORMAT(' Norm used at initial time            ',I9)
 2105 FORMAT(' Norm used at final time              ',I9)
 2106 FORMAT(' Lat-lon multiplication factor        ',I9)
 2107 FORMAT(' Area north-west latitude             ',I9)
 2108 FORMAT(' Area north-west longitude            ',I9)
 2109 FORMAT(' Area south-east latitude             ',I9)
 2110 FORMAT(' Area south-east longitude            ',I9)
 2111 FORMAT(' Accuracy                             ',I9)
 2112 FORMAT(' Number of singular vectors evolved   ',I9)
 2113 FORMAT(' Ritz number 1                        ',I9)
 2114 FORMAT(' Ritz number 2                        ',I9)
 2115 FORMAT(' Observation offset period            ',I9)
 2116 FORMAT(' Forecast lead time                   ',I9)
 2117 FORMAT(' Domain                                       ',A1)
 2118 FORMAT(' Method number                        ',I9)
 2119 FORMAT(' Total forecasts in ensemble          ',I9)
 2120 FORMAT(' Shape of verification region (flag)  ',I9)
C
C       ECMWF Local definition 22 and 23.
C
 2201 FORMAT(' Reference date                       ',I9)
 2202 FORMAT(' Climate date (from)                  ',I9)
 2203 FORMAT(' Climate date (to)                    ',I9)
C
C       ECMWF Local definition 24
C
 2401 FORMAT(' Satellite identifier  (WMO table C-5)',I9)
 2402 FORMAT(' Instrument identifier (WMO table C-8)',I9)
 2403 FORMAT(' Channel number                       ',I9)
 2404 FORMAT(' Function code                        ',I9)
C
C       ECMWF Local definition 25
C
 2501 FORMAT(' Component index                      ',I9)
 2502 FORMAT(' Number of components                 ',I9)
 2503 FORMAT(' Model error type                     ',I9)
C
C       ECMWF Local definition 50.
C
 5001 FORMAT(' Number                               ',I9)
 5002 FORMAT(' Total                                ',I9)
 5003 FORMAT(' Model identifier                     ',I9)
 5004 FORMAT(' Latitude of NW corner (degrees*10^6) ',I9)
 5005 FORMAT(' Longitude of NW corner(degrees*10^6) ',I9)
 5006 FORMAT(' Latitude of SE corner (degrees*10^6) ',I9)
 5007 FORMAT(' Longitude of SE corner(degrees*10^6) ',I9)
 5008 FORMAT(' Original parameter number            ',I9)
 5009 FORMAT(' Original parameter table number      ',I9)
 5010 FORMAT(10(1X,Z8.8))
C
C       ECMWF Local definition 190.
C
19001 FORMAT(' Number of ECMWF local definitions    ',I9)
19002 FORMAT(' ECMWF local definition number        ',I9)
19003 FORMAT(' Number of bytes in local definition  ',I9)
#ifdef INTEGER_8
19004 FORMAT(3X,Z16.16)
#else
19004 FORMAT(3X,Z8.8)
#endif
C
C       ECMWF Local definition 191.
C
19101 FORMAT(' Format version major number          ',I9)
19102 FORMAT(' Format version minor number          ',I9)
19103 FORMAT(' Original sub-centre identifier       ',I9)
19108 FORMAT(' Number of bytes of data descriptor   ',I9)
#ifdef INTEGER_8
19109 FORMAT(3X,Z16.16)
#else
19109 FORMAT(3X,Z8.8)
#endif
C
      END
