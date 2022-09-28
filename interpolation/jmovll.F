C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JMOVLL( PZFA, PNORTH, PWEST, PLATINC, PLONINC, PSTART,
     X                   KROWS, KCOLS, KLON, KJPWR, POUTF, KFLAG)
C
C---->
C**** JMOVLL
C
C     Purpose
C     _______
C
C     This routine moves grid point data from array PZFA to array POUTF.
C
C
C     Interface
C     _________
C
C     CALL JMOVLL( PZFA, PNORTH, PWEST,PLATINC, PLONINC, PSTART, 
C    X             KROWS, KCOLS, KLON, KJPWR, POUTF, KFLAG)
C
C
C     Input parameters
C     ________________
C
C     PZFA      -  Input array of grid points arranged in pairs of
C                  north and south latitude bands
C     PNORTH    -  Northernmost latitude for field in POUTF (degrees)
C     PWEST     -  Westernmost longitude for field in POUTF (degrees)
C     PLATINC   -  Grid interval between latitudes in degrees in POUTF
C     PLONINC   -  Grid interval between longitudes in degrees in POUTF
C     PSTART    -  Current first latitude in PZFA for moving (degrees)
C     KROWS     -  Number of latitude rows to store
C     KCOLS     -  Number of longitude points to store from latitude
C     KLON      -  Number of longitude points in generated latitude row
C     KJPWR     -  Multiplication factor applied to ensure that number
C                  of longitude points > twice*spherical truncation
C     KFLAG     -  = 1 if only the North hemisphere lats. to be moved
C                  = 2 if only the South hemisphere lats. to be moved
C                  = 3 if both North/South hemisphere lats. to be moved
C
C
C     Output parameters
C     ________________
C
C     POUTF      - output array of grid points
C
C
C     Common block usage
C     __________________
C
C     JDCNDBG
C
C
C     Method
C     ______
C
C     Moves latitude rows of points from PZFA into the correct 
C     geographical positions in POUTF.  POUTF may already be 
C     partially full.  The distribution is symmetrical north-south.
C
C
C     Externals
C     _________
C
C     INTLOG   - Logs messages
C     INTLOGR  - Logs messages with real value.
C
C
C     Reference
C     _________
C
C
C     E.C.M.W.F. Research Department technical memorandum no. 56
C                "The forecast and analysis post-processing package"
C                May 1982. J.Haseler.
C
C
C     Comments
C     ________
C
C     POUTF is a 2-dimensional array, with first dimension = KCOLS.
C
C     For calculation purposes, the number of longitude points
C     has to be greater than 2*(output truncation) to ensure that the
C     fourier transform is exact (see Reference, page 10). 
C     Parameter JPLONO is set to 860 to ensure that PZFA will have
C     enough slots for the KLON points for all values of regular
C     grid intervals from N1 to N720.
C
C     When filling the output array POUTF, the longitude points have
C     have to be taken selectively to avoid the intermediate generated
C     points, picking up values only at the required longitudes.  The
C     magnification factor is KJPWR (a power of 2).
C
C     PZFA has values stored:
C       North lat1:  x, 1, x, 2, x, 3, x, ... , KLON, ...
C       South lat1:  x, 1, x, 2, x, 3, x, ... , KLON, ...
C       North lat2:  x, 1, x, 2, x, 3, x, ... , KLON, ...
C       South lat2:  x, 1, x, 2, x, 3, x, ... , KLON, ...
C       ... for KROWS latitudes.
C
C     The 'x' represents the extra values generated because of the
C     need for the number of points along a latitude line to be more
C     than twice the truncation of the spherical harmonics.
C
C
C     Author
C     ______
C
C     J.D.Chambers      ECMWF      Nov 1993
C
C
C     Modifications
C     _____________
C
C     None
C
C----<
C
      IMPLICIT NONE
C     _______________________________________________________
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
#include "jparams.h"
#include "parim.h"
C
C     Subroutine arguments
C
      INTEGER KROWS, KCOLS, KLON, KJPWR, KFLAG
      REAL PZFA, PNORTH, PWEST, PLATINC, PLONINC, PSTART, POUTF
      DIMENSION PZFA( JPLONO+2, 64), POUTF(KCOLS,*)
C
C     Parameters
      INTEGER JPROUTINE
      PARAMETER( JPROUTINE = 31900 )
C
C     Local variables
C
      INTEGER IOFFN, IOFFS, LOOPRW, LOOPCL, NEXT
      INTEGER LONGIT, N360, NSTART
C
C     _______________________________________________________
C
C
C*    Section 1. Initialization
C     _______________________________________________________
C
  100 CONTINUE

      IF ( NDBG .GT. 1) THEN
        CALL INTLOG(JP_DEBUG,'JMOVLL - Input parameters:',JPQUIET)
        CALL INTLOGR(JP_DEBUG,
     X    'JMOVLL - Northern latitude for output = ', PNORTH)
        CALL INTLOGR(JP_DEBUG,
     X    'JMOVLL - Western longitude for output = ', PWEST)
        CALL INTLOGR(JP_DEBUG,
     X    'JMOVLL -  Latitude grid interval (deg) = ', PLATINC)
        CALL INTLOGR(JP_DEBUG,
     X    'JMOVLL - Longitude grid interval (deg) = ', PLONINC)
        CALL INTLOGR(JP_DEBUG,
     X    'JMOVLL - First latitude for moving = ', PSTART)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVLL - Number of latitudes to store = ', KROWS)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVLL - Number of longitude pts per row = ', KCOLS)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVLL - No. long.pts per generated row = ', KLON)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVLL - Multiplication factor applied = ', KJPWR)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVLL - Move hemisphere(1=N,2=S,3=both) = ', KFLAG)
C
      ENDIF
C
C     Find start offset in storage array
      IOFFN  = NINT( (PNORTH - PSTART)/PLATINC ) + 1
      IOFFS  = NINT( (PNORTH + PSTART)/PLATINC ) + 2
      LONGIT = NINT( (360.0 + PWEST)/PLONINC )
      N360 = NINT( 360.0/PLONINC )
      NSTART = MOD( LONGIT, N360)*KJPWR
      IF ( NDBG .GT. 1) THEN
        CALL INTLOG(JP_DEBUG,
     X    'JMOVLL - Offset in storage array(north) = ', IOFFN)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVLL - Offset in storage array(south) = ', IOFFS)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVLL - Start longitude = ', LONGIT)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVLL - Modulus for longitude 360deg. = ', N360)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVLL - Start point index = ', NSTART)
      ENDIF
C     _______________________________________________________
C
C
C*    Section 2. Store the points for northern latitudes, southern 
C*               latitudes or both.
C     _______________________________________________________
C
 200  CONTINUE
C
C     Store Northern latitudes ..
C     .. skipping intermediate values - see calculation of NEXT below
C
      IF ( KFLAG .NE. 2 ) THEN
C
        IF ( NDBG .GT. 1) CALL INTLOG(JP_DEBUG,
     X    'JMOVLL - Storing North latitudes',JPQUIET)
C
        DO 222 LOOPRW = 1, KROWS*2, 2
          DO 220 LOOPCL = 1, KCOLS
            NEXT = 2 + MOD( NSTART + (LOOPCL-1)*KJPWR , KLON)
            POUTF( LOOPCL, IOFFN + LOOPRW/2 ) = PZFA( NEXT, LOOPRW)
 220      CONTINUE
 222    CONTINUE
      ENDIF
C
C     Store Southern latitudes ..
C     .. skipping intermediate values - see calculation of NEXT below
C
      IF ( KFLAG .NE. 1 ) THEN
C
        IF ( NDBG .GT. 1) CALL INTLOG(JP_DEBUG,
     X    'JMOVLL - Storing South latitudes',JPQUIET)
C
        DO  242 LOOPRW = 2, KROWS*2, 2
          DO 240 LOOPCL = 1, KCOLS
            NEXT = 2 + MOD( NSTART + (LOOPCL-1)*KJPWR , KLON)
            POUTF( LOOPCL, IOFFS - LOOPRW/2 ) = PZFA( NEXT, LOOPRW)
 240      CONTINUE
 242    CONTINUE
C
      ENDIF
C
C     _______________________________________________________
C
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
  900 CONTINUE
C
C
      RETURN
      END
