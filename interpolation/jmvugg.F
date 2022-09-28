C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JMVUGG(PZFA, KNORTH, KSOUTH, PWEST, KGNUM, KSTART,
     X                  KDIFF, KROWS, KCOLS, KLON, KJPWR, KOFSET, POUTF)
C
C---->
C**** JMVUGG
C
C     Purpose
C     _______
C
C     This routine moves gaussian grid point data from array PZFA to
C     array POUTF. It handles the case when the number of latitudes
C     is different in the two hemispheres.
C
C
C     Interface
C     _________
C
C     CALL JMVUGG(PZFA, KNORTH, KSOUTH, PWEST, KGNUM, KSTART,
C    X            KDIFF, KROWS, KCOLS, KLON, KJPWR, KOFSET, POUTF)
C
C
C     Input parameters
C     ________________
C
C     PZFA      -  Input array of grid points arranged in pairs of
C                  north and south latitude bands
C     KNORTH    -  Northernmost latitude row number for field in POUTF
C     KSOUTH    -  Southernmost latitude row number for field in POUTF
C     PWEST     -  Westernmost longitude for field in POUTF (degrees)
C     KGNUM     -  Gaussian number for field in POUTF
C     KSTART    -  Current first latitude number in PZFA
C     KDIFF     -  Offset to  first latitude row in PZFA for moving
C     KROWS     -  Number of latitude rows to store
C     KCOLS     -  Number of longitude points to store from latitude
C     KLON      -  Number of longitude points in generated latitude row
C     KJPWR     -  Multiplication factor applied to ensure that number
C                  of longitude points > twice*spherical truncation
C     KOFSET    -  Array of offsets for start of each latitude row in
C                  the output array
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
C     partially full.  The distribution is not symmetrical north-south.
C
C
C     Externals
C     _________
C
C     JMOVGG    - Move points of symmetric grid to array
C     INTLOG   - Logs messages
C     INTLOGR  - Logs messages with real value.
C
C
C     Reference
C     _________
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
C     _______________________________________________________
C
C
#include "jparams.h"
#include "parim.h"
C
C     Subroutine arguments
      INTEGER KNORTH, KSOUTH, KGNUM, KSTART, KDIFF, POUTF
      INTEGER KROWS, KCOLS, KLON, KJPWR, KOFSET
      DIMENSION KOFSET(*)
      REAL PZFA, PWEST
      DIMENSION PZFA( JPLONO+2, 64), POUTF(*)
C
C     Parameters
      INTEGER JPROUTINE
      PARAMETER( JPROUTINE = 32000 )
C
C     Local variables
      INTEGER INROWS, NROWS, NFLAG
      INTEGER LATEST
      INTEGER NTOP, NBASE
C
C     _______________________________________________________
C
C*    Section 1. Initialization.
C     _______________________________________________________
C
  100 CONTINUE
C
      IF ( NDBG .GT. 1) THEN
        CALL INTLOG(JP_DEBUG,'JMVUGG - Input parameters:',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'JMVUGG - Northern latitude for output = ', KNORTH)
        CALL INTLOG(JP_DEBUG,
     X    'JMVUGG - Southern latitude for output = ', KSOUTH)
        CALL INTLOGR(JP_DEBUG,
     X    'JMVUGG - Western longitude for output = ', PWEST)
        CALL INTLOG(JP_DEBUG,
     X    'JMVUGG - Gaussian number for field = ', KGNUM)
        CALL INTLOG(JP_DEBUG,
     X    'JMVUGG - First latitude for moving = ', KSTART)
        CALL INTLOG(JP_DEBUG,
     X    'JMVUGG - Offset to first lat. for moving = ', KDIFF)
        CALL INTLOG(JP_DEBUG,
     X    'JMVUGG - Number of latitudes to store = ', KROWS)
        CALL INTLOG(JP_DEBUG,
     X    'JMVUGG - Number of longitude pts per row = ', KCOLS)
        CALL INTLOG(JP_DEBUG,
     X    'JMVUGG - No. long.pts per generated row = ', KLON)
        CALL INTLOG(JP_DEBUG,
     X    'JMVUGG - Multiplication factor applied = ', KJPWR)
        CALL INTLOG(JP_DEBUG,
     X    'JMVUGG - Offsets(20) of each lat. in output array:',JPQUIET)
C
        DO 101 NDBGLP = 1, 20
          CALL INTLOG(JP_DEBUG,' ',KOFSET(NDBGLP))
  101   CONTINUE
      ENDIF
C
      LATEST = 1
      NTOP   = KSTART + KDIFF
      NBASE  = KSTART + (KROWS - 1)
      INROWS = KROWS
      NROWS  = INROWS
C
      IF ( NDBG .GT. 1) THEN
        CALL INTLOG(JP_DEBUG,
     X    'JMVUGG - Current northernmost lat = ', NTOP)
        CALL INTLOG(JP_DEBUG,
     X    'JMVUGG - Current southernmost lat = ', NBASE)
      ENDIF
C     _______________________________________________________
C
C*    Section 2. Processing more North than South, or more South
C                than North.
C     _______________________________________________________
C
  200 CONTINUE
C
C     See if overall there are more rows in Northern Hemisphere than
C     Southern
      IF ( KNORTH .LE. (2*KGNUM - KSOUTH) ) THEN
C
        IF ( NDBG .GT. 1) CALL INTLOG(JP_DEBUG,
     X    'JMVUGG - More rows in North Hemis. than South',JPQUIET)
C
C       Yes, so see if there are any rows in the current batch for the
C       south as well as the north
        IF ( NBASE .LT. (2*KGNUM - KSOUTH) ) THEN
C
C         None for south in current batch, move all rows to north.
          NFLAG = 1
C
          IF ( NDBG .GT. 1) CALL INTLOG(JP_DEBUG,
     X      'JMVUGG - No rows for S, move rows to N = ', NROWS)
C
          CALL JMOVGG( PZFA, PWEST, KGNUM, KSTART, NROWS, KCOLS,
     X                 KLON, KJPWR, KOFSET, POUTF, NFLAG)
        ELSE
C
C         Some for south.
C
C         Move rows for north only first.
          IF ( (2*KGNUM-KSOUTH) .GT. (NTOP-1) ) THEN
            NROWS = (2*KGNUM-KSOUTH) - NTOP + 1
            NFLAG = 1
C
            IF ( NDBG .GT. 1) CALL INTLOG(JP_DEBUG,
     X        'JMVUGG - Move North rows only first = ', NROWS)
C
            CALL JMOVGG( PZFA, PWEST, KGNUM, KSTART, NROWS, KCOLS,
     X                   KLON, KJPWR, KOFSET, POUTF, NFLAG)
            NTOP   = KSTART + NROWS
            LATEST = LATEST + (2 * NROWS)
            NROWS  = INROWS - NROWS
          ENDIF
C
C         Then move rows for both.
          IF ( NROWS .GT. 0 ) THEN
            NFLAG = 3
C
            IF ( NDBG .GT. 1) CALL INTLOG(JP_DEBUG,
     X        'JMVUGG - Move common lat.(N & S) = ', NROWS)
C
            CALL JMOVGG( PZFA( 1, LATEST), PWEST, KGNUM, NTOP,
     X                   NROWS, KCOLS, KLON, KJPWR, KOFSET, POUTF,NFLAG)
          ENDIF
        ENDIF
C
C     Overall there are more rows in Southern Hemisphere than Northern
      ELSE
C
            IF ( NDBG .GT. 1) CALL INTLOG(JP_DEBUG,
     X        'JMVUGG - More rows in South Hemis. than North',JPQUIET)
C
C
C       So see if there are any rows in the current batch for the
C       north as well as the south
        IF ( NBASE .LT. KNORTH ) THEN
C
C         None for north in current batch, move all rows to south.
          NFLAG = 2
C
          IF ( NDBG .GT. 1) CALL INTLOG(JP_DEBUG,
     X      'JMVUGG - No rows for N, move rows to S = ', NROWS)
C
          CALL JMOVGG( PZFA, PWEST, KGNUM, KSTART, NROWS, KCOLS,
     X                 KLON, KJPWR, KOFSET, POUTF, NFLAG)
        ELSE
C
C         Some for north.  Move rows for south only first.
          NROWS = KNORTH - NTOP + KDIFF
          IF ( NROWS .GT. 0 ) THEN
C
            IF ( NDBG .GT. 1) CALL INTLOG(JP_DEBUG,
     X        'JMVUGG - Move South rows only first = ', NROWS)
C
            NFLAG = 2
            CALL JMOVGG( PZFA, PWEST, KGNUM, KSTART, NROWS, KCOLS,
     X                   KLON, KJPWR, KOFSET, POUTF, NFLAG)
            LATEST = LATEST + (2 * NROWS)
            NROWS = INROWS - NROWS
          ELSE
            NROWS = INROWS
          ENDIF
C
C         Then move rows for both.
          NFLAG = 3
          IF ( NROWS .GT. 0 ) THEN
C
            IF ( NDBG .GT. 1) CALL INTLOG(JP_DEBUG,
     X        'JMVUGG - Move common lats(N & S) = ', NROWS)
C
            CALL JMOVGG( PZFA( 1, LATEST), PWEST, KGNUM, NTOP,
     X                   NROWS, KCOLS, KLON, KJPWR, KOFSET, POUTF,NFLAG)
          ENDIF
        ENDIF
      ENDIF
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
