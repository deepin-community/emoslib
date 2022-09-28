C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION ISCRSZ()
C---->
C**** ISCRSZ
C
C     Purpose
C     -------
C
C     Calculate size of scratch space required for output field of
C     interpolation.
C
C
C     Interface
C     ---------
C
C     ISIZE = ISCRSZ()
C
C
C     Input
C     -----
C
C     Values in common blocks "nofld.common".
C
C
C     Output
C     ------
C
C     Size as number of values (words).
C     Returns 0 if output type not recognised.
C
C
C     Method
C     ------
C
C     Depends on output grid type.
C
C
C     Externals
C     ---------
C
C     INTLOG  - Log error message
C     JNORSGG - Calculate gaussian latitude
C     NUMPTWE - Calculate number of grid pts in range from west to east
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     Feb 1995
C
C----<
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nofld.common"
#include "grfixed.h"
C
C     Local variables
C
      INTEGER LOOP
      REAL PNORTH, PWEST, PSOUTH, PEAST, TEMP !, GRID
C
C     Externals
C
      EXTERNAL JNORSGG, NUMPTWE
      INTEGER JNORSGG, NUMPTWE
C
C     Statement functions
C
      REAL X, Y
      LOGICAL SOUTHOF
      SOUTHOF(X,Y) = ((X) - (Y)).GT.-1E-4
C
C     -----------------------------------------------------------------|
C     Section 1.  Initialise.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      ISCRSZ = 0
C
C     -----------------------------------------------------------------|
C*    Section 2.   Spherical harmonic field
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IF( (NOREPR.EQ.JPSPHERE).OR.
     X    (NOREPR.EQ.JPSTRSH).OR.
     X    (NOREPR.EQ.JPSPHROT) ) THEN
cs        ISCRSZ = (NORESO+1) * (NORESO+4)
        ISCRSZ = (NORESO+1) * (NORESO+2)
        CALL INTLOG(JP_DEBUG,'ISCRSZ: spherical harmonic',JPQUIET)
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3.   Regular lat/long field
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      IF( (NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPREGROT) ) THEN
C
        TEMP = ABS(FLOAT(NOAREA(1)) - FLOAT(NOAREA(3)))
        NONS  = NINT( TEMP / FLOAT(NOGRID(2)) ) + 1
        TEMP = FLOAT(NOAREA(4)) - FLOAT(NOAREA(2))
        NOWE  =  NINT( TEMP / FLOAT(NOGRID(1)) ) + 1
C
        ISCRSZ = NONS * NOWE
        CALL INTLOG(JP_DEBUG,'ISCRSZ: number of latitudes = ', NONS)
        CALL INTLOG(JP_DEBUG,'ISCRSZ: number of longitudes = ', NOWE)
        CALL INTLOG(JP_DEBUG,'ISCRSZ: Regular lat/long',JPQUIET)
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 4.   Regular gaussian field
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
      IF( (NOREPR.EQ.JPGAUSSIAN).OR.(NOREPR.EQ.JPFGGROT) ) THEN
C
        PNORTH = FLOAT( NOAREA(1) ) / PPMULT
        PSOUTH = FLOAT( NOAREA(3) ) / PPMULT
        NO1NS = JNORSGG( PNORTH, ROGAUSS, NOGAUSS, 1)
        NONS  = JNORSGG( PSOUTH, ROGAUSS, NOGAUSS, 1) - NO1NS + 1
        TEMP = FLOAT(NOAREA(4)) - FLOAT(NOAREA(2))
        TEMP = (TEMP * FLOAT(NOGAUSS) ) / FLOAT(JP90)
CS added for grib2
        LGLOBL = (NONS.EQ.NOGAUSS*2.AND.NOAREA(2).EQ.0)
        NOWE = NINT(TEMP) + 1
        IF( LGLOBL ) NOWE = 4*NOGAUSS
C
        ISCRSZ = NONS * NOWE
        CALL INTLOG(JP_DEBUG,'ISCRSZ: number of latitudes = ', NONS)
        CALL INTLOG(JP_DEBUG,'ISCRSZ: number of longitudes = ', NOWE)
        CALL INTLOG(JP_DEBUG,'ISCRSZ: regular gaussian',JPQUIET)
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 5.   Reduced (quasi-regular) gaussian field
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
      IF( (NOREPR.EQ.JPQUASI).OR.(NOREPR.EQ.JPQGGROT) ) THEN
C
        PNORTH = FLOAT( NOAREA(1) ) / PPMULT
        PWEST  = FLOAT( NOAREA(2) ) / PPMULT
        PSOUTH = FLOAT( NOAREA(3) ) / PPMULT
        PEAST  = FLOAT( NOAREA(4) ) / PPMULT
        NO1NS = JNORSGG( PNORTH, ROGAUSS, NOGAUSS, 1)
        NONS  = JNORSGG( PSOUTH, ROGAUSS, NOGAUSS, 1) - NO1NS + 1
CS added for grib2
        LGLOBL = (NONS.EQ.NOGAUSS*2.AND.NOAREA(2).EQ.0)
        NOWE = 0
C
        NOPCNT = 0
        DO LOOP = 1, NOGAUSS*2
          IF( SOUTHOF(PNORTH,ROGAUSS(LOOP)).AND.
     X        SOUTHOF(ROGAUSS(LOOP),PSOUTH) ) THEN
C
C           EMOS-186: disregard area cropping east/west-wise, it seems
C           NUMPTWE assumptions don't hold for RGG/octahedral
C           GRID = 360.0 / REAL(NOLPTS(LOOP))
C           NOPCNT = NOPCNT + NUMPTWE(PWEST,PEAST,GRID)
            NOPCNT = NOPCNT + NOLPTS(LOOP)
C
          ENDIF
        ENDDO
C
        ISCRSZ = NOPCNT
        CALL INTLOG(JP_DEBUG,'ISCRSZ: number of latitudes = ', NONS)
        CALL INTLOG(JP_DEBUG,'ISCRSZ: quasi-regular gaussian',JPQUIET)
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9.   Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      CALL INTLOG(JP_DEBUG,
     X  'ISCRSZ: number of points in output field = ',ISCRSZ)
C
      RETURN
      END
