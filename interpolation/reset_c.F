C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION RESET_C(KSEC1,KSEC2,PSEC2,KSEC4)
C
C---->
C***  RESET_C
C
C     Purpose
C     -------
C
C     Reset interpolation common block variables according to the
C     GRIB sections 1, 2 and 4 values.
C 
C
C     Interface
C     ---------
C
C     IRET = RESET_C(KSEC1,KSEC2,PSEC2,KSEC4)
C
C
C     Input
C     -----
C
C     KSEC1   - GRIB section 1 array (integers).
C     KSEC2   - GRIB section 2 array (integers).
C     PSEC2   - GRIB section 2 array (reals).
C     KSEC4   - GRIB section 4 array (integers).
C
C
C     Externals
C     ---------
C
C     IGGLAT  - Computes gaussian latitudes for a given truncation.
C
C
C     Modifications
C     -------------
C
C     None.
C
C
C     Author
C     ------
C
C     J.D.Chambers          ECMWF    August, 1994
C
C----<
C
      IMPLICIT NONE
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "grfixed.h"
#include "current.h"
C
C     Function arguments
C
      INTEGER KSEC1, KSEC2, KSEC4
      DIMENSION KSEC1(*), KSEC2(*), KSEC4(*)
      REAL PSEC2
      DIMENSION PSEC2(*)
C
C     Local variables
C
      INTEGER JJPR, JJERR, ITEMP
      INTEGER I, IRET
      LOGICAL LSAME, LPQUASI
      CHARACTER*1 HTYPE
C
C     Externals
C
      INTEGER IGGLAT
      EXTERNAL IGGLAT
C
C-----------------------------------------------------------------------
C     Section 1.   Initialise
C-----------------------------------------------------------------------
C
  100 CONTINUE
C
      RESET_C = 0
C
      LPQUASI = ( KSEC2(17).EQ.1 )
C
C     Section 1, Word 1 - version number of WMO code table
C
      IF( NITABLE.NE.KSEC1(1) ) LCHANGE = .TRUE.
      NITABLE = KSEC1(1)
      IF( .NOT.LNOTABL ) NOTABLE = NITABLE
C
C     Section 1, Word 6 - parameter indicator (Code Table 2)
C
      NIPARAM = KSEC1(6)
      IF( .NOT.LNOPARA ) NOPARAM = NIPARAM
C
C     Section 1, Word 7 - Type of level indicator (Code Table 3)
C
      NILEVT = KSEC1(7)
      IF( .NOT.LNOLEVT ) NOLEVT = NILEVT
C
C     By default, upper air level does not use land-sea mask.
C     (pressure level, sigma_level, model level, missing level)
C
      IF( (NOLEVT.EQ.100) .OR.
     X    (NOLEVT.EQ.108) .OR.
     X    (NOLEVT.EQ.109) .OR.
     X    (NOLEVT.EQ.255) ) THEN
        LSM     = .FALSE.
        LSMSET  = .TRUE.
        LSMCHNG = .FALSE.
      ELSE
        LSM     = .TRUE.
        LSMSET  = .FALSE.
        LSMCHNG = .TRUE.
      ENDIF
C
C     Section 1, Word 8 - Height or pressure of level (Code Table 3)
C
      NILEVEL = KSEC1(8)
      IF( .NOT.LNOLEVL ) NOLEVEL = NILEVEL
C
C     Setup ECMWF local definition number and stream if applicable
C
      IF( KSEC1(24).EQ.1 ) THEN
        NILOCAL = KSEC1(37)
        NISTREM = KSEC1(40)
      ELSE
        NILOCAL = 0
        NISTREM = 0
      ENDIF
C
C     Section 2, Word   1    Data representation type (Code Table 6).
C
      IF( NIREPR.NE.KSEC2(1) ) THEN
        LCHANGE = .TRUE.
        NIGAUSO = 0
        HIGAUST = 'X'
        IF( .NOT.LNOGAUS ) THEN
          NOGAUSO = 0
          HOGAUST = 'X'
        ENDIF
      ENDIF
      NIREPR = KSEC2(1)
      IF( .NOT.LNOREPR ) NOREPR = NIREPR
C
C     Date of the input field (YYYYMMDD).
C
      NIDATE = (KSEC1(21)-1)*1000000
     X          + KSEC1(10)*10000 + KSEC1(11)*100 + KSEC1(12)
      NODATE = NIDATE
C
C     Stretching factor (if any)
C
      IF( (NIREPR.EQ.JPSTRGG).OR.(NIREPR.EQ.JPSTRSH) ) THEN
        RISTRET = PSEC2(2)
        IF( NIREPR.EQ.NOREPR ) THEN
          IF( NIREPR.EQ.JPSTRGG ) NOREPR = JPGAUSSIAN
          IF( NIREPR.EQ.JPSTRSH ) NOREPR = JPSPHERE
        ENDIF
        IF( NIREPR.EQ.JPSTRGG ) NIREPR = JPGAUSSIAN
        IF( NIREPR.EQ.JPSTRSH ) NIREPR = JPSPHERE
      ELSE
        RISTRET = 0.0
      ENDIF
C
C-----------------------------------------------------------------------
C      Section 2.  Latitude/longitude grid or equidistant cylindrical
C                  or Plate Carree.
C-----------------------------------------------------------------------
C
  200 CONTINUE
C
      IF ( (NIREPR.EQ.JPREGULAR).OR.(NIREPR.EQ.JPREGROT) ) THEN
C
C       Check if quasi-regular lat/long
C
        IF( LPQUASI ) NIREPR = JPREDLL
        IF( .NOT.LNOREPR ) NOREPR = NIREPR
C
        NIRESO  = 0
        NIGAUSS = 0
C
C       Latitude/longitude grid or equidistant cylindrical
C       or Plate Carree.
C
C       Section 2, Word   2    Ni - Number of points along a parallel.
C       Section 2, Word   3    Nj - Number of points along a meridian.
C
        IF( NIREPR.EQ.JPREDLL ) THEN
          NIWE = 0
        ELSE
          IF( NIWE.NE.KSEC2(2) ) LCHANGE = .TRUE.
          NIWE = KSEC2(2)
        ENDIF
C
        IF( NINS.NE.KSEC2(3) ) LCHANGE = .TRUE.
        NINS = KSEC2(3)
C
C       Section 2, Word   4    La1 - Latitude of first grid point.
C       Section 2, Word   5    Lo1 - Longitude of first grid point.
C
        IF( NIAREA(1).NE.(KSEC2(4)*JPMICRO) ) LCHANGE = .TRUE.
        NIAREA(1) = KSEC2(4) * JPMICRO
        IF( NIAREA(2).NE.(KSEC2(5)*JPMICRO) ) LCHANGE = .TRUE.
        NIAREA(2) = KSEC2(5) * JPMICRO
C
C       Section 2, Word   7    La2 - Latitude of last grid point.
C       Section 2, Word   8    Lo2 - Longitude of last grid point.
C
        IF( NIAREA(3).NE.(KSEC2(7)*JPMICRO) ) LCHANGE = .TRUE.
        NIAREA(3) = KSEC2(7) * JPMICRO
        IF( NIAREA(4).NE.(KSEC2(8)*JPMICRO) ) LCHANGE = .TRUE.
        NIAREA(4) = KSEC2(8) * JPMICRO
C
C       Section 2, Word   9    Di - i direction increment.
C       Section 2, Word  10    Dj - j direction increment.
C
        IF( NIREPR.EQ.JPREDLL ) THEN
          NIGRID(1) = 0
        ELSE
          IF( NIGRID(1).NE.(KSEC2(9)*JPMICRO) ) LCHANGE = .TRUE.
          NIGRID(1) = KSEC2(09) * JPMICRO
        ENDIF
C
        IF( NIGRID(2).NE.(KSEC2(10)*JPMICRO) ) LCHANGE = .TRUE.
        NIGRID(2) = KSEC2(10) * JPMICRO
C
C       Section 2, Word  11    Scanning mode flags (Code Table 8).
C
        IF( NISCNM.NE.KSEC2(11) ) LCHANGE = .TRUE.
        NISCNM = KSEC2(11)
C
C       Pick up regular gaussian grid definition if option 'gaussian'
C       used.
C
        IF( NOREPR.EQ.JPNOTYPE ) THEN
          IF( (NOGAUSO.NE.NOGAUSS).OR.(HOGAUST.NE.'F') ) THEN
            HTYPE = 'F'
            CALL JGETGG(NOGAUSS,HTYPE,ROGAUSS,NOLPTS,IRET)
            IF( IRET.NE.0 ) THEN
              CALL INTLOG(JP_ERROR,
     X          'RESET_C: JGETGG failed, NOGAUSS = ',NOGAUSS)
              RESET_C = IRET
              GOTO 900
            ENDIF
            NOGAUSO = NOGAUSS
            HOGAUST = 'F'
          ENDIF
          NOREPR = JPGAUSSIAN
        ENDIF
C
C       If reduced lat/long output and no user specified grid, use
C       input reduced lat/long definition for number of points at
C       each latitude
C
        IF( LPQUASI .AND. (.NOT.LNOREPR ) ) THEN
          NOREDLL = KSEC2(3)
          NONS = NINS
          DO 210 I = 1, NINS
            MILLEN(I) = KSEC2(22 + I)
 210      CONTINUE
          DO 220 I = 1, NONS
            NOLPTS(I) = MILLEN(I)
 220      CONTINUE
        ENDIF
C
C       If reduced lat/long input, read input reduced lat/long definition
C       for number of points at each latitude and set east longitude,
C       unless it is a subarea (eg Mediterranean).
C
        IF( NIREPR.EQ.JPREDLL ) THEN
          DO I = 1, NINS
            MILLEN(I) = KSEC2(22 + I)
          ENDDO
C
C         Calculate east longitude if field is global.
C         Use southern latitude to guess if it is ...
C
          IF( NIAREA(3).LT.0 ) THEN
            ITEMP = 36000000 - 36000000/MILLEN(NINS/2)
            IF( NIAREA(4).NE.ITEMP ) LCHANGE = .TRUE.
            NIAREA(4) = 36000000 - 36000000/MILLEN(NINS/2)
          ENDIF
        ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C      Section 3.  Spherical Harmonic Coefficients.
C-----------------------------------------------------------------------
C
  300 CONTINUE
C
      IF( (NIREPR.EQ.JPSPHERE).OR.
     X    (NIREPR.EQ.JPSTRSH).OR.
     X    (NIREPR.EQ.JPSPHROT) ) THEN
C
        NIGAUSS   = 0
        NIGRID(1) = 0
        NIGRID(2) = 0
C
C       Section 2, Word   2    J - Pentagonal resolution parameter.
C
        IF( NIRESO.NE.KSEC2(2) ) LCHANGE = .TRUE.
        NIRESO = KSEC2(2)
        IF( .NOT.LNORESO ) THEN
          IF( NORESO.NE.NIRESO ) LCHANGE = .TRUE.
          NORESO = NIRESO
        ENDIF
C
C       Pick up regular gaussian grid definition if option 'gaussian'
C       used.
C
        IF( NOREPR.EQ.JPNOTYPE ) THEN
          IF( (NOGAUSO.NE.NOGAUSS).OR.(HOGAUST.NE.'F') ) THEN
            HTYPE = 'F'
            CALL JGETGG(NOGAUSS,HTYPE,ROGAUSS,NOLPTS,IRET)
            IF( IRET.NE.0 ) THEN
              CALL INTLOG(JP_ERROR,
     X          'RESET_C: JGETGG failed, NOGAUSS = ',NOGAUSS)
              RESET_C = IRET
              GOTO 900
            ENDIF
            NOGAUSO = NOGAUSS
            HOGAUST = 'F'
          ENDIF
          NOREPR = JPGAUSSIAN
        ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C      Section 4.  Regular gaussian grid.
C-----------------------------------------------------------------------
C
  400 CONTINUE
C
      IF( (NIREPR.EQ.JPGAUSSIAN) .AND. (.NOT.LPQUASI) ) THEN
C
        NIRESO    = 0
        NIGRID(1) = 0
        NIGRID(2) = 0
C
C       Section 2, Word   2    Ni - Number of points along a parallel.
C       Section 2, Word   3    Nj - Number of points along a meridian.
C
        IF( NIWE.NE.KSEC2(2) ) LCHANGE = .TRUE.
        NIWE = KSEC2(2)
        IF( NINS.NE.KSEC2(3) ) LCHANGE = .TRUE.
        NINS = KSEC2(3)
C
C       Section 2, Word   4    La1 - Latitude of first grid point.
C       Section 2, Word   5    Lo1 - Longitude of first grid point.
C
        IF( NIAREA(1).NE.(KSEC2(4)*JPMICRO) ) LCHANGE = .TRUE.
        NIAREA(1) = KSEC2(4) * JPMICRO
        IF( NIAREA(2).NE.(KSEC2(5)*JPMICRO) ) LCHANGE = .TRUE.
        NIAREA(2) = KSEC2(5) * JPMICRO
C
C       Section 2, Word   7    La2 - Latitude of last grid point.
C       Section 2, Word   8    Lo2 - Longitude of last grid point.
C
        IF( NIAREA(3).NE.(KSEC2(7)*JPMICRO) ) LCHANGE = .TRUE.
        NIAREA(3) = KSEC2(7) * JPMICRO
        IF( NIAREA(4).NE.(KSEC2(8)*JPMICRO) ) LCHANGE = .TRUE.
        NIAREA(4) = KSEC2(8) * JPMICRO
C
C       Section 2, Word  10    N - Number of parallels between Pole and
C                   the Equator.
C
        IF( NIGAUSS.NE.KSEC2(10) ) LCHANGE = .TRUE.
        NIGAUSS = KSEC2(10)
C
C       Pick up gaussian definitions for input grid
C
        IF( (NIGAUSO.NE.NIGAUSS).OR.(HIGAUST.NE.'F') ) THEN
          HTYPE = 'F'
          CALL JGETGG(NIGAUSS,HTYPE,RIGAUSS,MILLEN,IRET)
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'RESET_C: JGETGG failed, NIGAUSS = ',NIGAUSS)
            RESET_C = IRET
            GOTO 900
          ENDIF
          NIGAUSO = NIGAUSS
          HIGAUST = 'F'
        ENDIF
C
C       Use same gaussian number for output unless user has
C       deliberately set a value
C
        IF( .NOT.LNOGAUS )  THEN
          IF( NOGAUSS.NE.NIGAUSS ) LCHANGE = .TRUE.
          NOGAUSS = NIGAUSS
        ENDIF
C
C       Fixup output type if 'gaussian' used to describe output and
C       not 'reduced' or 'regular'.
C
        LSAME = (NOREPR.EQ.JPNOTYPE) .AND. (NOGAUSS.EQ.NIGAUSS)
C
C       Pick up regular gaussian grid definition corresponding
C       to the input field if option 'gaussian' used.
C
        IF( (NOREPR.EQ.JPNOTYPE).AND.(.NOT.LSAME) ) THEN
          IF( (NOGAUSO.NE.NOGAUSS).OR.(HOGAUST.NE.'F') ) THEN
            HTYPE = 'F'
            CALL JGETGG(NOGAUSS,HTYPE,ROGAUSS,NOLPTS,IRET)
            IF( IRET.NE.0 ) THEN
              CALL INTLOG(JP_ERROR,
     X          'RESET_C: JGETGG failed, NOGAUSS = ',NOGAUSS)
              RESET_C = IRET
              GOTO 900
            ENDIF
            NOGAUSO = NOGAUSS
            HOGAUST = 'F'
          ENDIF
          NOREPR = JPGAUSSIAN
        ENDIF
C
C       Section 2, Word  11    Scanning mode flags (Code Table 8).
C
        IF( NISCNM.NE.KSEC2(11) ) LCHANGE = .TRUE.
        NISCNM = KSEC2(11)
C
      ENDIF
C
C-----------------------------------------------------------------------
C      Section 5.  Quasi-regular (reduced) gaussian grid.
C-----------------------------------------------------------------------
C
  500 CONTINUE
C
      IF( (NIREPR.EQ.JPGAUSSIAN) .AND. LPQUASI ) THEN
C
        NIRESO    = 0
        NIGRID(1) = 0
        NIGRID(2) = 0
C
C       Set representation(s) to "quasi-regular gaussian"
C
        NIREPR = JPQUASI
        IF( .NOT.LNOREPR ) NOREPR = NIREPR
C
C       Section 2, Word   2    Ni - Number of points along a parallel.
C                   Cannot be used for quasi-regular grids.
C       Section 2, Word   3    Nj - Number of points along a meridian.
C
        IF( NIWE.NE.0 ) LCHANGE = .TRUE.
        NIWE = 0
        IF( NINS.NE.KSEC2(3) ) LCHANGE = .TRUE.
        NINS = KSEC2(3)
C
C       Section 2, Word   4    La1 - Latitude of first grid point.
C       Section 2, Word   5    Lo1 - Longitude of first grid point.
C
        IF( NIAREA(1).NE.(KSEC2(4)*JPMICRO) ) LCHANGE = .TRUE.
        NIAREA(1) = KSEC2(4) * JPMICRO
        IF( NIAREA(2).NE.(KSEC2(5)*JPMICRO) ) LCHANGE = .TRUE.
        NIAREA(2) = KSEC2(5) * JPMICRO
C
C       Section 2, Word   7    La2 - Latitude of last grid point.
C       Section 2, Word   8    Lo2 - Longitude of last grid point.
C
        IF( NIAREA(3).NE.(KSEC2(7)*JPMICRO) ) LCHANGE = .TRUE.
        NIAREA(3) = KSEC2(7) * JPMICRO
        IF( NIAREA(4).NE.(KSEC2(8)*JPMICRO) ) LCHANGE = .TRUE.
        NIAREA(4) = KSEC2(8) * JPMICRO
C
C       Section 2, Word  10    N - Number of parallels between Pole and
C                   the Equator.
C
        IF( NIGAUSS.NE.KSEC2(10) ) THEN
          LCHANGE = .TRUE.
          JJPR = 1
          JJERR = 1
          IRET = IGGLAT( KSEC2(10)*2, RIGAUSS , JJPR, JJERR)
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_ERROR, 'RESET_C: IGGLAT failed',JPQUIET)
            RESET_C = IRET
            GOTO 900
          ENDIF
        ENDIF
        NIGAUSS = KSEC2(10)
C
C       Fixup output type if 'gaussian' used to describe output and
C       not 'reduced' or 'regular'.
C
        LSAME = (NOREPR.EQ.JPNOTYPE) .AND. (NOGAUSS.EQ.NIGAUSS)
C
C       Pick up regular gaussian grid definition corresponding
C       to the output field if option 'gaussian' used.
C
        IF( (NOREPR.EQ.JPNOTYPE).AND.(.NOT.LSAME) ) THEN
          IF( (NOGAUSO.NE.NOGAUSS).OR.(HOGAUST.NE.'F') ) THEN
            HTYPE = 'F'
            CALL JGETGG(NOGAUSS,HTYPE,ROGAUSS,NOLPTS,IRET)
            IF( IRET.NE.0 ) THEN
              CALL INTLOG(JP_ERROR,
     X          'RESET_C: JGETGG failed, NOGAUSS = ',NOGAUSS)
              RESET_C = IRET
              GOTO 900
            ENDIF
            NOGAUSO = NOGAUSS
            HOGAUST = 'F'
          ENDIF
          NOREPR = JPGAUSSIAN
        ENDIF
C
C       Section 2, Word  11    Scanning mode flags (Code Table 8).
C
        IF ( NISCNM .NE. KSEC2(11) ) LCHANGE = .TRUE.
        NISCNM = KSEC2(11)
C
C       Section 2, Word  23-nn
C       Number of points along each parallel in a quasi-regular grid.
C       Number of parallels is given by Nj above.
C
C       Pick up gaussian definitions for input grid
C
        IF( (NIGAUSO.NE.NIGAUSS).OR.(HIGAUST.NE.'F') ) THEN
          HTYPE = 'F'
          CALL JGETGG(NIGAUSS,HTYPE,RIGAUSS,MILLEN,IRET)
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'RESET_C: JGETGG failed, NIGAUSS = ',NIGAUSS)
            RESET_C = IRET
            GOTO 900
          ENDIF
          NIGAUSO = NIGAUSS
          HIGAUST = 'F'
        ENDIF
        DO 510 I = 1, NINS
          MILLEN(I) = KSEC2(22 + I)
 510    CONTINUE
C
C       Use same grid for output, unless user requested otherwise.
C
        IF( (.NOT.LNOGAUS) .OR. LSAME ) THEN
          IF( NOGAUSS.NE.NIGAUSS ) LCHANGE = .TRUE.
          NOGAUSS = NIGAUSS
          NONS = NINS
          DO 520 I = 1, NONS
            NOLPTS(I) = MILLEN(I)
            ROGAUSS(I) = RIGAUSS(I)
 520      CONTINUE
          NOGAUSO = NIGAUSO
          HOGAUST = HIGAUST
        ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C      Section 6.  Polar Stereographic grid.
C-----------------------------------------------------------------------
C
  600 CONTINUE
C
      IF ( NIREPR .EQ. 5 ) THEN
C
        NIRESO  = 0
        NIGAUSS = 0
C
C       Section 2, Word   2    Nx - Number of points along X-axis.
C       Section 2, Word   3    Ny - Number of points along Y-axis.
C
        IF( NIWE.NE.0 ) LCHANGE = .TRUE.
        NIWE = KSEC2(2)
        IF( NINS.NE.KSEC2(3) ) LCHANGE = .TRUE.
        NINS = KSEC2(3)
C
C       Section 2, Word   4    La1 - Latitude of first grid point.
C       Section 2, Word   5    Lo1 - Longitude of first grid point.
C
        IF( NIAREA(1).NE.(KSEC2(4)*JPMICRO) ) LCHANGE = .TRUE.
        NIAREA(1) = KSEC2(4) * JPMICRO
        IF( NIAREA(2).NE.(KSEC2(5)*JPMICRO) ) LCHANGE = .TRUE.
        NIAREA(2) = KSEC2(5) * JPMICRO
C
C       Section 2, Word   7    La2 - Latitude of last grid point.
C       Section 2, Word   8    Lo2 - Longitude of last grid point.
C
        IF( NIAREA(3).NE.(KSEC2(7)*JPMICRO) ) LCHANGE = .TRUE.
        NIAREA(3) = KSEC2(7) * JPMICRO
        IF( NIAREA(4).NE.(KSEC2(8)*JPMICRO) ) LCHANGE = .TRUE.
        NIAREA(4) = KSEC2(8) * JPMICRO
C
C       Section 2, Word   9    Dx - X-direction increment.
C       Section 2, Word  10    Dy - Y-direction increment.
C
        IF( NIGRID(1).NE.KSEC2(9) ) LCHANGE = .TRUE.
        NIGRID(1) = KSEC2(09)
        IF( NIGRID(2).NE.KSEC2(10) ) LCHANGE = .TRUE.
        NIGRID(2) = KSEC2(10)
C
C       Section 2, Word  11    Scanning mode flags (Code Table 8).
C
        IF( NISCNM.NE.KSEC2(11) ) LCHANGE = .TRUE.
        NISCNM = KSEC2(11)
C
      ENDIF
C
C-----------------------------------------------------------------------
C      Section 7.  Setup number of bits used for packing ouput.
C-----------------------------------------------------------------------
C
  700 CONTINUE
C
C     Section 4, Word 2 - Number  of bits used for each packed value.
C
C     Use same number of bits for packing output values for unless
C     user has deliberately set a value.
C
      NIACC = KSEC4(2)
      IF( .NOT.LNOACC ) THEN
        IF( NOACC.NE.NIACC ) LCHANGE = .TRUE.
        NOACC = NIACC
      ENDIF
C
C     If default is implied for the output grid, use the input grid
C
      IF( ( (NIREPR.EQ.JPREGULAR).OR.
     X      (NIREPR.EQ.JPREDLL)  .OR.
     X      (NIREPR.EQ.JPREGROT)     ) .AND.
     X    ( (NOREPR.EQ.JPREGULAR).OR.
     X      (NOREPR.EQ.JPREDLL)  .OR.
     X      (NOREPR.EQ.JPREGROT)     ) .AND.
     X      .NOT.LNOGRID ) THEN
        NOGRID(1) = NIGRID(1)
        NOGRID(2) = NIGRID(2)
      ENDIF
C
C-----------------------------------------------------------------------
C      Section 9.  Closedown.
C-----------------------------------------------------------------------
C
  900 CONTINUE
      RETURN
      END
