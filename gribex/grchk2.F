C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GRCHK2 (KSEC1,KSEC2,PSEC2,KRET)
C
C---->
C**** GRCHK2 - Check parameters for Section 2 of GRIB Code.
C
C     Purpose.
C     --------
C
C     Check parameters for Section 2 of GRIB code against
C     valid values for GRIB Edition 1.
C
C**   Interface.
C     ----------
C
C     CALL GRCHK2 (KSEC1,KSEC2,PSEC2,KRET)
C
C
C     Input Parameters.
C     -----------------
C
C     KSEC1      - Array containing integer parameters for
C                  section 1 of GRIB code.
C
C     KSEC2      - Array containing integer parameters for
C                  section 2 of GRIB code.
C
C     PSEC2      - Array containing real parameters for
C                  section 2 of GRIB code.
C
C     Output Parameters.
C     ------------------
C
C     KRET       - Return code.
C                  0   , No error encountered.
C                  1   , Error in GRIB Code parameter.
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
C     None.
C
C     Reference.
C     ----------
C
C     WMO Manual on Codes for GRIB Code.
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
C     Polar stereographic representation type allowed.
C
C     J. Hennessy      ECMWF      01.10.91
C     Space view or orthographic representation allowed.
C     Additional parameters KSEC1 and PSEC2 added.
C
C     J.D.Chambers     ECMWF      27.04.94
C     Allow complex packing for spherical harmonics
C
C     J.D.Chambers     ECMWF      03.04.95
C     Allow Lambert conformal.
C
C     J.D.Chambers     ECMWF      July 2004
C     Allow Mercator
C
C----<
C     ----------------------------------------------------------------
C     Section 0 . Definition of variables. Data statements.
C     ----------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Parameters
C
      INTEGER JP6, JP6X, JP8
      PARAMETER (JP6=23)
      PARAMETER (JP6X=17)
      PARAMETER (JP8=8)
C
C     Subroutine arguments
C
      INTEGER KSEC1, KSEC2, KRET
      DIMENSION KSEC1(*)
      DIMENSION KSEC2(*)
      REAL PSEC2
      DIMENSION PSEC2(*)
C
C     Local variables
C
      LOGICAL LQUASI
      INTEGER LOOP
C
      INTEGER ITAB6, ITAB6X, ITAB8
      DIMENSION ITAB6(JP6)
      DIMENSION ITAB6X(JP6X)
      DIMENSION ITAB8(JP8)
C
C     Valid values given in Code Table 6.
C
      DATA ITAB6 /  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
     X             10, 13, 14,
     X             20, 24, 30, 34, 50, 60, 70, 80, 90,
     X            192 /
C
C     Code Table 6 values currently supported by GRIBEX.
C
      DATA ITAB6X /0,1,3,4,5,10,14,20,24,30,34,50,60,70,80,90,192/
C
C     Valid values given in Code Table 8.
C
      DATA ITAB8 /0,128,64,192,32,160,96,224/
C
C     ----------------------------------------------------------------
C     Section 1 . Set initial values.
C     ----------------------------------------------------------------
C
  100 CONTINUE
C
C     Reset return code.
      KRET = 0
C
C     ----------------------------------------------------------------
C     Section 2 . Check values against code tables and extreme values.
C     ----------------------------------------------------------------
C
  200 CONTINUE
C
C     Number of vertical coordinate parameters.
C
      IF (KSEC2(12).LT.0.OR.KSEC2(12).GT.255) THEN
        KRET = 1
        WRITE(GRPRSM,9019) KSEC2(12)
      ENDIF
C
C     Check Data Representation Type.
C
      DO LOOP = 1,JP6
        IF (KSEC2(1).EQ.ITAB6(LOOP)) GO TO 202
      ENDDO
C
      KRET = 1
      WRITE(GRPRSM,9001) KSEC2(1)
C
  202 CONTINUE
C
C     Check Data Representation Type currently supported.
C
      DO LOOP = 1,JP6X
        IF (KSEC2(1).EQ.ITAB6X(LOOP)) GO TO 204
      ENDDO
C
      KRET = 1
      WRITE(GRPRSM,9002) KSEC2(1)
      GO TO 900
C
  204 CONTINUE
C
C     Earth flag.
C
      IF (KSEC2(18).NE.0.AND.KSEC2(18).NE.64) THEN
        KRET = 1
        WRITE(GRPRSM,9005) KSEC2(18)
      ENDIF
C
C     Check ECMWF usage.(0 except for space view perspective)
C
      IF (KSEC2(18).NE.0.AND.KSEC1(2).EQ.98.AND.KSEC2(1).NE.90) THEN
        KRET = 1
        WRITE(GRPRSM,9007)
      ENDIF
C
C     Components flag.
C
      IF (KSEC2(19).NE.0.AND.KSEC2(19).NE.8) THEN
        KRET = 1
        WRITE(GRPRSM,9006) KSEC2(19)
      ENDIF
C
C     Check ECMWF usage.
C
      IF(KSEC2(19).NE.0.AND.KSEC2(19).NE.8.AND.KSEC1(2).EQ.98) THEN
        KRET = 1
        WRITE(GRPRSM,9008)
      ENDIF
C
      LQUASI = (KSEC2(17) .EQ. 1)
C
C     ----------------------------------------------------------------
C     Section 3. Checks on latitude/longitude grids.
C     ----------------------------------------------------------------
C
  300 CONTINUE
C
      IF ( KSEC2(1).EQ.0  .OR. KSEC2(1).EQ.10 .OR.
     X     KSEC2(1).EQ.20 .OR. KSEC2(1).EQ.30 ) THEN
C
C       Number of points along a parallel.
C
        IF ( KSEC2(2).LT.1. OR. KSEC2(2).GT.65535 ) THEN
          IF ( .NOT. LQUASI ) THEN
            KRET = 1
            WRITE(GRPRSM,9022) KSEC2(2)
          ENDIF
        ENDIF
C
C       Number of points along a meridian.
C
        IF ( KSEC2(3).LT.1 .OR. KSEC2(3).GT.65535 ) THEN
          IF ( .NOT. LQUASI ) THEN
            KRET = 1
            WRITE(GRPRSM,9023) KSEC2(3)
          ENDIF
        ENDIF
C
C       Latitude of first grid point.
C
        IF (KSEC2(4).LT.-90000.OR.KSEC2(4).GT.90000) THEN
          KRET = 1
          WRITE(GRPRSM,9015) KSEC2(4)
        ENDIF
C
C       Longitude of first grid point.
C
        IF (KSEC2(5).LT.-360000.OR.KSEC2(5).GT.360000) THEN
          KRET = 1
          WRITE(GRPRSM,9016) KSEC2(5)
        ENDIF
C
C       Resolution flag.
C
        IF (KSEC2(6).NE.0.AND.KSEC2(6).NE.128) THEN
          KRET = 1
          WRITE(GRPRSM,9003) KSEC2(6)
        ENDIF
C
C       Latitude of last grid point.
C
        IF (KSEC2(7).LT.-90000.OR.KSEC2(7).GT.90000) THEN
          KRET = 1
          WRITE(GRPRSM,9020) KSEC2(7)
        ENDIF
C
C       Longitude of last grid point.
C
        IF (KSEC2(8).LT.-360000.OR.KSEC2(8).GT.360000) THEN
          KRET = 1
          WRITE(GRPRSM,9021) KSEC2(8)
        ENDIF
C
C       Direction increments, if included.
C
        IF (KSEC2(6).EQ.128) THEN
          IF (KSEC2(9).LT.1.OR.KSEC2(9).GT.65535) THEN
            IF( .NOT. LQUASI ) THEN
              KRET = 1
              WRITE(GRPRSM,9024) KSEC2(9)
            ENDIF
          ENDIF
C
          IF (KSEC2(10).LT.1.OR.KSEC2(10).GT.65535) THEN
            IF( .NOT. LQUASI ) THEN
              KRET = 1
              WRITE(GRPRSM,9025) KSEC2(10)
            ENDIF
          ENDIF
        ENDIF
C
C       Scanning mode flag.
C
        DO LOOP = 1, JP8
          IF (KSEC2(11).EQ.ITAB8(LOOP)) GO TO 302
        ENDDO
C
        KRET = 1
        WRITE(GRPRSM,9004) KSEC2(11)
C
  302   CONTINUE
C
C       Regular / Quasi-regular grid check.
C
        IF (KSEC2(17).NE.0.AND.KSEC2(17).NE.1) THEN
          KRET = 1
          WRITE(GRPRSM,9009) KSEC2(17)
        ENDIF
C
        GO TO 900
      ENDIF
C
C     ----------------------------------------------------------------
C     Section 4. Checks on Gaussian grids.
C     ----------------------------------------------------------------
C
  400 CONTINUE
C
      IF ( KSEC2(1).EQ.4  .OR. KSEC2(1).EQ.14 .OR.
     X     KSEC2(1).EQ.24 .OR. KSEC2(1).EQ.34) THEN
C
C       Latitude of first grid point.
C
        IF (KSEC2(4).LT.-90000.OR.KSEC2(4).GT.90000) THEN
          KRET = 1
          WRITE(GRPRSM,9015) KSEC2(4)
        ENDIF
C
C       Longitude of first grid point.
C
        IF (KSEC2(5).LT.-360000.OR.KSEC2(5).GT.360000) THEN
          KRET = 1
          WRITE(GRPRSM,9016) KSEC2(5)
        ENDIF
C
C       Latitude of last grid point.
C
        IF (KSEC2(7).LT.-90000.OR.KSEC2(7).GT.90000) THEN
          KRET = 1
          WRITE(GRPRSM,9020) KSEC2(7)
        ENDIF
C
C       Longitude of last grid point.
C
        IF (KSEC2(8).LT.-360000.OR.KSEC2(8).GT.360000) THEN
          KRET = 1
          WRITE(GRPRSM,9021) KSEC2(8)
        ENDIF
C
C       i-direction increment, if included.
C
        IF (KSEC2(6).EQ.128) THEN
          IF (KSEC2(9).LT.1.OR.KSEC2(9).GT.65535) THEN
            KRET = 1
            WRITE(GRPRSM,9024) KSEC2(9)
          ENDIF
        ENDIF
C
C       Number of parallels beween pole and equator.
C
        IF (KSEC2(10).LT.1.OR.KSEC2(10).GT.65535) THEN
          KRET = 1
          WRITE(GRPRSM,9026) KSEC2(10)
        ENDIF
C
C       Increment flag.
C
        IF (KSEC2(6).NE.0.AND.KSEC2(6).NE.128) THEN
          KRET = 1
          WRITE(GRPRSM,9003) KSEC2(6)
        ENDIF
C
C       Scanning mode flag.
C
        DO LOOP = 1, JP8
          IF (KSEC2(11).EQ.ITAB8(LOOP)) GO TO 402
        ENDDO
C
        KRET = 1
        WRITE(GRPRSM,9004) KSEC2(11)
C
  402   CONTINUE
C
C       Regular / Quasi-regular grid check.
C
        IF (KSEC2(17).NE.0.AND.KSEC2(17).NE.1) THEN
          KRET = 1
          WRITE(GRPRSM,9009) KSEC2(17)
        ENDIF
C
C       Cross-check increments flag and quasi-regular indicator.
C
        IF ( LQUASI .AND.KSEC2(6).EQ.128) THEN
          KRET = 1
          WRITE(GRPRSM,9011)
        ENDIF
C
        GO TO 900
      ENDIF
C
C     ----------------------------------------------------------------
C     Section 5. Checks on Polar Stereographic data.
C
C     ----------------------------------------------------------------
C
  500 CONTINUE
C
      IF (KSEC2(1).EQ.5) THEN
C
C       Number of points along X-axis.
C
        IF (KSEC2(2).LT.1.OR.KSEC2(2).GT.65535) THEN
          KRET = 1
          WRITE(GRPRSM,9027) KSEC2(2)
        ENDIF
C
C       Number of points along Y-axis.
C
        IF (KSEC2(3).LT.1.OR.KSEC2(3).GT.65535) THEN
          KRET = 1
          WRITE(GRPRSM,9028) KSEC2(3)
        ENDIF
C
C       Latitude of first grid point.
C
        IF (KSEC2(4).LT.-90000.OR.KSEC2(4).GT.90000) THEN
          KRET = 1
          WRITE(GRPRSM,9015) KSEC2(4)
        ENDIF
C
C       Longitude of first grid point.
C
        IF (KSEC2(5).LT.-360000.OR.KSEC2(5).GT.360000) THEN
          KRET = 1
          WRITE(GRPRSM,9016) KSEC2(5)
        ENDIF
C
C       Orientation of the grid.
C
        IF (KSEC2(7).LT.-360000.OR.KSEC2(7).GT.360000) THEN
          KRET = 1
          WRITE(GRPRSM,9017) KSEC2(7)
        ENDIF
C
C       Grid lengths.
C
        IF (KSEC2(9).LT.1.OR.KSEC2(9).GT.16777215) THEN
          KRET = 1
          WRITE(GRPRSM,9029) KSEC2(9)
        ENDIF
C
        IF (KSEC2(10).LT.1.OR.KSEC2(10).GT.16777215) THEN
          KRET = 1
          WRITE(GRPRSM,9030) KSEC2(10)
        ENDIF
C
C       Projection centre.
C
C       The use of 1 by the Polar Stereographic projection in this
C       octet is inconsistent with Lambert conformal et al
C       representation where bit 1 is set 1 to indicate North pole.
C
        IF (KSEC2(13).NE.0.AND.KSEC2(13).NE.1) THEN
          KRET = 1
          WRITE(GRPRSM,9018) KSEC2(13)
        ENDIF
C
        GO TO 900
      ENDIF
C
C     ----------------------------------------------------------------
C     Section 6. Checks on spherical harmonic data.
C     ----------------------------------------------------------------
C
  600 CONTINUE
C
      IF ( KSEC2(1).EQ.50 .OR. KSEC2(1).EQ.60 .OR.
     X     KSEC2(1).EQ.70 .OR. KSEC2(1).EQ.80 ) THEN
C
C       Spectral data representation type.
C
        IF (KSEC2(5).NE.1) THEN
          KRET = 1
          WRITE(GRPRSM,9012) KSEC2(5)
        ENDIF
C
C       Spectral data representation mode.
C
        IF (KSEC2(6).NE.1.AND.KSEC2(6).NE.2) THEN
          KRET = 1
          WRITE(GRPRSM,9013) KSEC2(6)
        ENDIF
C
        GO TO 900
      ENDIF
C
C     ----------------------------------------------------------------
C     Section 7 . Checks on space view perspective.
C     ----------------------------------------------------------------
C
  700 CONTINUE
C
      IF (KSEC2(1).EQ.90) THEN
        GO TO 900
      ENDIF
C
C     ----------------------------------------------------------------
C     Section 8. Checks on Lambert conformal data.
C     ----------------------------------------------------------------
C
  800 CONTINUE
C
      IF (KSEC2(1).EQ.3) THEN
C
C       Number of points along X-axis.
C
        IF (KSEC2(2).LT.1.OR.KSEC2(2).GT.65535) THEN
          KRET = 1
          WRITE(GRPRSM,9027) KSEC2(2)
        ENDIF
C
C       Number of points along Y-axis.
C
        IF (KSEC2(3).LT.1.OR.KSEC2(3).GT.65535) THEN
          KRET = 1
          WRITE(GRPRSM,9028) KSEC2(3)
        ENDIF
C
C       Latitude of first grid point.
C
        IF (KSEC2(4).LT.-90000.OR.KSEC2(4).GT.90000) THEN
          KRET = 1
          WRITE(GRPRSM,9015) KSEC2(4)
        ENDIF
C
C
C       Longitude of first grid point.
C
        IF (KSEC2(5).LT.-360000.OR.KSEC2(5).GT.360000) THEN
          KRET = 1
          WRITE(GRPRSM,9016) KSEC2(5)
        ENDIF
C
C       Orientation of the grid.
C
        IF (KSEC2(7).LT.-360000.OR.KSEC2(7).GT.360000) THEN
          KRET = 1
          WRITE(GRPRSM,9017) KSEC2(7)
        ENDIF
C
C       Grid lengths.
C
        IF (KSEC2(9).LT.1.OR.KSEC2(9).GT.16777215) THEN
          KRET = 1
          WRITE(GRPRSM,9029) KSEC2(9)
        ENDIF
C
        IF (KSEC2(10).LT.1.OR.KSEC2(10).GT.16777215) THEN
          KRET = 1
          WRITE(GRPRSM,9030) KSEC2(10)
        ENDIF
C
C       Projection centre.
C
        IF (KSEC2(13).NE.0.AND.KSEC2(13).NE.64.AND.KSEC2(13).NE.
     X       128.AND.KSEC2(13).NE.192) THEN
          KRET = 1
          WRITE(GRPRSM,9018) KSEC2(13)
        ENDIF
C
C       Latitude Latin 1.
C
        IF (KSEC2(14).LT.-90000.OR.KSEC2(14).GT.90000) THEN
          KRET = 1
          WRITE(GRPRSM,9015) KSEC2(14)
        ENDIF
C
C       Latitude Latin 2.
C
        IF (KSEC2(15).LT.-90000.OR.KSEC2(15).GT.90000) THEN
          KRET = 1
          WRITE(GRPRSM,9015) KSEC2(15)
        ENDIF
C
C       Latitude of southern pole.
C
        IF (KSEC2(20).LT.-90000.OR.KSEC2(20).GT.90000) THEN
          KRET = 1
          WRITE(GRPRSM,9015) KSEC2(20)
        ENDIF
C
C       Longitude of southern pole.
C
        IF (KSEC2(21).LT.-360000.OR.KSEC2(21).GT.360000) THEN
          KRET = 1
          WRITE(GRPRSM,9016) KSEC2(21)
        ENDIF
C
        GO TO 900
      ENDIF
C
C     ----------------------------------------------------------------
C     Section 9 . Return to calling routine. Format statements.
C     ----------------------------------------------------------------
C
  900 CONTINUE
C
 9001 FORMAT (1H ,'GRCHK2 : Invalid data representation type - ',I3)
 9002 FORMAT (1H ,'GRCHK2 : Unsupported data representation type - ',I3)
 9003 FORMAT (1H ,'GRCHK2 : Invalid increments flag - ',I3)
 9004 FORMAT (1H ,'GRCHK2 : Invalid scanning mode flag - ',I3)
 9005 FORMAT (1H ,'GRCHK2 : Invalid earth flag - ',I3)
 9006 FORMAT (1H ,'GRCHK2 : Invalid components flag - ',I3)
 9007 FORMAT (1H ,'GRCHK2 : Earth flag - ECMWF usage is 0.')
 9008 FORMAT (1H ,'GRCHK2 : Components flag - ECMWF usage is 0.')
 9009 FORMAT (1H ,'GRCHK2 : Invalid quasi / regular indicator - ',I3)
 9011 FORMAT (1H ,'GRCHK2 : Quasi-regular Gaussian grid cannot have',
     C            ' direction increments included.')
 9012 FORMAT (1H ,'GRCHK2 : Invalid spectral representation type - ',I3)
 9013 FORMAT (1H ,'GRCHK2 : Invalid spectral representation mode - ',I3)
 9014 FORMAT (1H ,'GRCHK2 : Complex spectral representation mode ',
     C            'not catered for.')
 9015 FORMAT (1H ,'GRCHK2 : Invalid latitude of first grid point - ',
     C               I10)
 9016 FORMAT (1H ,'GRCHK2 : Invalid longitude of first grid point - ',
     C               I10)
 9017 FORMAT (1H ,'GRCHK2 : Invalid orientation of the grid - ',
     C               I10)
 9018 FORMAT (1H ,'GRCHK2 : Invalid projection centre flag - ',I3)
 9019 FORMAT (1H ,'GRCHK2 : Invalid number of vertical coordinate ',
     C            'parameters - ',I8)
 9020 FORMAT (1H ,'GRCHK2 : Invalid latitude of last grid point - ',
     C               I10)
 9021 FORMAT (1H ,'GRCHK2 : Invalid longitude of last grid point - ',
     C               I10)
 9022 FORMAT (1H ,'GRCHK2 : Invalid number of points along a parallel',
     C              ' - ', I10)
 9023 FORMAT (1H ,'GRCHK2 : Invalid number of points along a meridian',
     C              ' - ', I10)
 9024 FORMAT (1H ,'GRCHK2 : Invalid i-direction increment - ',I10)
 9025 FORMAT (1H ,'GRCHK2 : Invalid j-direction increment - ',I10)
 9026 FORMAT (1H ,'GRCHK2 : Invalid number of parallels - ',I10)
 9027 FORMAT (1H ,'GRCHK2 : Invalid number of points along X-axis',
     C              ' - ', I10)
 9028 FORMAT (1H ,'GRCHK2 : Invalid number of points along Y-axis',
     C              ' - ', I10)
 9029 FORMAT (1H ,'GRCHK2 : Invalid X-direction grid length - ',I10)
 9030 FORMAT (1H ,'GRCHK2 : Invalid Y-direction grid length - ',I10)
C
      RETURN
C
      END
