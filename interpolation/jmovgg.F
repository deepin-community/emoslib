C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JMOVGG( PZFA, PWEST, KGNUM, KSTART, KROWS,
     X                   KCOLS, KLON, KJPWR, KOFSET, POUTF, KFLAG)
C---->
C
C**** JMOVGG
C
C     Purpose
C     _______
C
C     This routine moves gaussian grid point data from array PZFA to 
C     array POUTF.
C
C
C     Interface
C     _________
C
C     CALL JMOVGG( PZFA, PWEST, KGNUM, KSTART, KROWS,
C    X             KCOLS, KLON, KJPWR, KOFSET, POUTF, KFLAG)
C
C
C     Input parameters
C     ________________
C
C     PZFA      -  Input array of grid points arranged in pairs of
C                  north and south latitude bands
C     PWEST     -  Westernmost longitude for field in POUTF (degrees)
C     KGNUM     -  Gaussian number for field in POUTF
C     KSTART    -  Current first latitude number in PZFA for moving
C     KROWS     -  Number of latitude rows to store
C     KCOLS     -  Number of longitude points to store from latitude
C     KLON      -  Number of longitude points in generated latitude row
C     KJPWR     -  Multiplication factor applied to ensure that number 
C                  of longitude points > twice*spherical truncation
C     KOFSET    -  Array of offsets for start of each latitude row in
C                  the output array
C     KFLAG     -  = 1 if only the North hemisphere lats. to be moved
C                  = 2 if only the South hemisphere lats. to be moved
C                  = 3 if both North/South hemisphere lats. to be moved
C
C
C     Output parameters
C     ________________
C
C     POUTF      - Output array of grid points
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
C     J.D.Chambers      ECMWF      Jan 1994
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
C
#include "jparams.h"
#include "parim.h"
C
C     Subroutine arguments
      INTEGER KSTART, KGNUM
      REAL PZFA, PWEST, POUTF
      DIMENSION PZFA( JPLONO+2, 64), POUTF(*)
      INTEGER KROWS, KCOLS, KLON, KJPWR, KOFSET, KFLAG
      DIMENSION KOFSET(*)
C
C     Parameters
      INTEGER JPROUTINE
      PARAMETER( JPROUTINE = 31800 )
C
C     Local variables
      INTEGER IOFFN, IOFFS, NXTLAT, NXTLON, NEXTPT
      INTEGER LONGIT, N360, NSTART
      REAL PINTVL
C
C     _______________________________________________________
C
C*    Section 1. Initialization
C     _______________________________________________________
C
  100 CONTINUE
C
      IF ( NDBG .GT. 1) THEN
        CALL INTLOG(JP_DEBUG,'JMOVGG - Input parameters:',JPQUIET)
        CALL INTLOGR(JP_DEBUG,
     X    'Western longitude for output = ', PWEST)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVGG -  Gaussian number for field = ', KGNUM)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVGG - First latitude for moving = ', KSTART)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVGG - Number of latitudes to store = ', KROWS)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVGG - Number of longitude pts per row = ', KCOLS)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVGG - No. long.pts per generated row = ', KLON)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVGG - Multiplication factor applied = ', KJPWR)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVGG - Move hemisphere(1=N,2=S,3=both) = ', KFLAG)
C
        CALL INTLOG(JP_DEBUG,
     X    'JMOVGG - Offsets(20) for each lat. in output array:',JPQUIET)
C
        DO 101 NDBGLP = 1, 20
          CALL INTLOG(JP_DEBUG,' ',KOFSET(NDBGLP))
  101   CONTINUE
      ENDIF
C
C     Calculate which longitude point to start from in row
      N360   = 4*KGNUM
      PINTVL = 360.0/FLOAT(N360)
      LONGIT = NINT( (360.0 + PWEST)/PINTVL )
      NSTART = MOD( LONGIT, N360)*KJPWR
      IF ( NDBG .GT. 1) THEN
        CALL INTLOGR(JP_DEBUG,
     X    'Longitude interval along row = ', PINTVL)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVGG -  Start longitude = ', LONGIT)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVGG -  Modulus for longitude 360deg. = ', N360)
        CALL INTLOG(JP_DEBUG,
     X    'JMOVGG -  Start point index = ', NSTART)
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
C     .. skipping intermediate values - see calculation of NEXTPT below
C
      IF ( KFLAG .NE. 2 ) THEN
C
        IF ( NDBG .GT. 1) CALL INTLOG(JP_DEBUG,
     X    'JMOVGG -  Storing North latitudes',JPQUIET)
C
        DO NXTLAT = 1, KROWS*2, 2
C
C         Find start offset in storage array
          IOFFN  = KOFSET(KSTART + NXTLAT/2) - 1
          DO NXTLON = 1, KCOLS
            NEXTPT = 2 + MOD( NSTART + (NXTLON-1)*KJPWR , KLON)
            POUTF( NXTLON + IOFFN ) = PZFA( NEXTPT, NXTLAT)
          ENDDO
        ENDDO
      ENDIF
C
C     Store Southern latitudes ..
C     .. skipping intermediate values - see calculation of NEXTPT below
C
      IF ( KFLAG .NE. 1 ) THEN
C
        IF ( NDBG .GT. 1) CALL INTLOG(JP_DEBUG,
     X    'JMOVGG -  Storing South latitudes',JPQUIET)
C
        DO NXTLAT = 2, KROWS*2, 2
C
C         Find start offset in storage array
          IOFFS  = KOFSET(2*KGNUM - KSTART + 1 - NXTLAT/2 + 1) - 1
          DO NXTLON = 1, KCOLS
            NEXTPT = 2 + MOD( NSTART + (NXTLON-1)*KJPWR , KLON)
            POUTF( NXTLON + IOFFS ) = PZFA( NEXTPT, NXTLAT)
          ENDDO
        ENDDO
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
