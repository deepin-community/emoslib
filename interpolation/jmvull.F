C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JMVULL( PZFA, PNORTH, PSOUTH, PWEST, PLATINC, PLONINC,
     X                   PSTART,PDIFF, KROWS, KCOLS, KLON, KJPWR, POUTF)
C
C---->
C**** JMVULL
C
C     Purpose
C     _______
C
C     This routine moves grid point data from array PZFA to array POUTF.
C     It handles the case when the number of latitudes is different in
C     the two hemispheres.
C
C
C     Interface
C     _________
C
C     CALL JMVULL( PZFA, PNORTH, PSOUTH, PWEST, PLATINC, PLONINC,
C    X                   PSTART,PDIFF, KROWS, KCOLS, KLON, KJPWR, POUTF)
C
C
C     Input parameters
C     ________________
C
C     PZFA      -  Input array of grid points arranged in pairs of
C                  north and south latitude bands
C     PNORTH    -  Northernmost latitude for field in POUTF (degrees)
C     PSOUTH    -  Southernmost latitude for field in POUTF (degrees)
C     PWEST     -  Westernmost longitude for field in POUTF (degrees)
C     PLATINC   -  Grid interval between latitudes in degrees in POUTF
C     PLONINC   -  Grid interval between longitudes in degrees in POUTF
C     PSTART    -  Current first latitude in PZFA (degrees)
C     PDIFF     -  Offset to first latitude in PZFA for moving (degrees)
C     KROWS     -  Number of latitude rows to store
C     KCOLS     -  Number of longitude points to store from latitude
C     KLON      -  Number of longitude points in generated latitude row
C     KJPWR     -  Multiplication factor applied to ensure that number
C                  of longitude points > twice*spherical truncation
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
C     partially full.  The distribution is not symmetrical north-south.
C
C
C     Externals
C     _________
C
C     JMOVLL    - Move points of symmetric grid to array
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
C
C     _______________________________________________________
C
#include "jparams.h"
#include "parim.h"
C
C     Subroutine arguments
      INTEGER KROWS, KCOLS, KLON, KJPWR
      REAL PZFA, PNORTH, PWEST, PSOUTH, PLATINC, PLONINC, PSTART
      REAL PDIFF, POUTF
      DIMENSION PZFA( JPLONO+2, 64), POUTF( KCOLS, *)
C
C     Parameters
      INTEGER JPROUTINE
      PARAMETER( JPROUTINE = 32100 )
C
C     Local variables
      INTEGER INROWS, NROWS, NFLAG
      INTEGER LATEST
      REAL ZTOP, ZBASE
C
C     _______________________________________________________
C
C*    Section 1. Initialization.
C     _______________________________________________________
C
  100 CONTINUE
C
      IF ( NDBG .GT. 1) THEN
        CALL INTLOG(JP_DEBUG,'JMVULL - Input parameters:',JPQUIET)
        CALL INTLOGR(JP_DEBUG,
     X    'JMVULL - Northern latitude for output = ', PNORTH)
        CALL INTLOGR(JP_DEBUG,
     X    'JMVULL - Southern latitude for output = ', PSOUTH)
        CALL INTLOGR(JP_DEBUG,
     X    'JMVULL - Western longitude for output = ', PWEST)
        CALL INTLOGR(JP_DEBUG,
     X    'JMVULL - Latitude grid interval (deg) = ', PLATINC)
        CALL INTLOGR(JP_DEBUG,
     X    'JMVULL - Longitude grid interval (deg) = ', PLONINC)
        CALL INTLOGR(JP_DEBUG,
     X    'JMVULL - First latitude for moving = ', PSTART)
        CALL INTLOGR(JP_DEBUG,
     X    'JMVULL - Offset to first lat. for moving = ', PDIFF)
        CALL INTLOG(JP_DEBUG,
     X    'JMVULL - Number of latitudes to store = ', KROWS)
        CALL INTLOG(JP_DEBUG,
     X    'JMVULL - Number of longitude pts per row = ', KCOLS)
        CALL INTLOG(JP_DEBUG,
     X    'JMVULL - No. long.pts per generated row = ', KLON)
        CALL INTLOG(JP_DEBUG,
     X    'JMVULL - Multiplication factor applied = ', KJPWR)
      ENDIF
C
      LATEST = 1
      ZTOP   = PSTART
      ZBASE  = PSTART - (KROWS - 1)*PLATINC
      INROWS = KROWS
      NROWS = INROWS
C
      IF ( NDBG .GT. 1) THEN
        CALL INTLOGR(JP_DEBUG,
     X    'JMVULL - Current northernmost lat = ', ZTOP)
        CALL INTLOGR(JP_DEBUG,
     X    'JMVULL - Current southernmost lat = ', ZBASE)
      ENDIF
C     _______________________________________________________
C
C*    Section 2. Processing more North than South, or more South
C                than North.
C     _______________________________________________________
C
  200 CONTINUE
Cjdc  IF ( ZTOP .GE. (-PSOUTH) ) THEN
      IF( (PNORTH.GT.(-PSOUTH)) .AND. (ZTOP .GE. (-PSOUTH)) ) THEN
C
C     Move excess northern latitudes ...
        NROWS = NINT( ( ZTOP + PSOUTH )/PLATINC )
        NROWS = MIN( NROWS, KROWS )
        NFLAG = 1
C
        IF ( NDBG .GT. 1) CALL INTLOG(JP_DEBUG,
     X    'JMVULL - Move excess north latitudes = ', NROWS)
C
        CALL JMOVLL( PZFA, PNORTH, PWEST, PLATINC, PLONINC, ZTOP, NROWS,
     X               KCOLS, KLON, KJPWR, POUTF, NFLAG)
C
      ELSE
C
C     Move excess southern latitudes ...
        NROWS = NINT( ( ZTOP - PNORTH )/PLATINC )
        NROWS = MIN( NROWS, KROWS )
        NFLAG = 2
C
        IF ( NDBG .GT. 1) CALL INTLOG(JP_DEBUG,
     X    'JMVULL - Move excess south latitudes = ', NROWS)
C
          CALL JMOVLL(PZFA, PNORTH, PWEST, PLATINC, PLONINC, PSTART,
     X              NROWS, KCOLS, KLON, KJPWR, POUTF, NFLAG)
      ENDIF
C
      ZTOP   = ZTOP - (NROWS * PLATINC)
      LATEST = LATEST + (2 * NROWS)
      NROWS  = INROWS - NROWS
C     _______________________________________________________
C
C*    Section 3. Processing latitudes common to both North and South.
C     _______________________________________________________
C
  300 CONTINUE
C
C     Move rows common to both if any left ...
      IF ( NROWS .GT. 0 ) THEN
C
        IF ( NDBG .GT. 1) CALL INTLOG(JP_DEBUG,
     X    'JMVULL - Move common latitudes(N & S) = ', NROWS)
C
        NFLAG = 3
        CALL JMOVLL( PZFA( 1, LATEST), PNORTH, PWEST, PLATINC, PLONINC,
     X               ZTOP, NROWS, KCOLS, KLON, KJPWR, POUTF, NFLAG)
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
