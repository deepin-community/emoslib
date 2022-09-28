C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION WV2DINT(KNUM,NUMPTS,KE_W,KN_S,RESON,OLDWAVE,
     X                         NEWWAVE,NORTH,WEST,KNSPEC,PMISS,RNS)
C
C---->
C*****WV2DINT*
C
C     PURPOSE
C     -------
C
C     To interpolate a wave field quasi-regular latitude
C     longitude grid to a regular latitude/longitude grid.
C
C
C     INTERFACE
C     ---------
C
C     IRET = WV2DINT(KNUM,NUMPTS,KE_W,KN_S,RESON,OLDWAVE,
C                    NEWWAVE,NORTH,WEST,KNSPEC,PMISS,RNS)
C
C     Input arguments
C     ---------------
C
C     KNUM    - No. of meridians from North to South pole (input field)
C     NUMPTS  - Array giving number of points along each latitude
C               (empty latitudes have entry 0)
C     KE_W    - First dimension of new array
C               = Number of points E-W in new grid
C     KN_S    - Second dimension of new array
C               = Number of points N-S in new grid
C     RESON   - Output grid resolution (degrees)
C     OLDWAVE - Original wave field
C     NORTH   - Output grid northernmost latitude (degree)
C     WEST    - Output grid westernmost  longitude (degree)
C     KNSPEC  - Number of 2D spectra values at each wave point
C     PMISS   - Missing data value
C     RNS     - Difference in degrees in NS disrection
C
C     Output arguments
C     ----------------

C     NEWWAVE - New wave field
C
C     Function returns 0 if the interpolation was OK.
C
C
C     METHOD
C     ------
C
C     The nearest neighbouring grid point value is assigned to
C     the interpolated point.
C
C     The output (sub) is the same as the input area.
C
C
C     EXTERNALS
C     ---------
C
C     INTLOG  - Log error message.
C
C
C     REFERENCE
C     ---------
C
C     Based on:
C         SUBROUTINE INTERPOLATE
C         Peter Janssen     ECMWF    September 1995
C     and:
C         SUBROUTINE EXPOINT
C         Heinz Gunther     ECMWF    December  1989
C
C
C     Author.
C     -------
C
C     J.D.Chambers      ECMWF    November 1996
C
C     J.D.Chambers      ECMWF    September 1998
C     Modified to handle subarea input/output.
C     The output (sub) is the same as the input area.
C
C
C----<
C
      IMPLICIT NONE
C
#include "parim.h"
C
C     Parameters
      INTEGER JPROUTINE
      PARAMETER ( JPROUTINE = 19410 )
      INTEGER JPLLMAX
      PARAMETER ( JPLLMAX = 1801 )
C                             `--> allow upto 0.1 degree resolution for
C                                  Mediterranean, 0.1 for Global
      INTEGER JPNMOUT
      PARAMETER ( JPNMOUT = 1800 )
C                             `--> allow upto 0.1 degree resolution for
C                                  Mediterranean, 0.1 for Global
C
C     Subroutine arguments
C
      INTEGER KNUM, NUMPTS, KE_W, KN_S, KNSPEC
      DIMENSION NUMPTS(*)
      REAL RESON, OLDWAVE, NEWWAVE, NORTH, WEST, PMISS
      REAL RNS
      DIMENSION OLDWAVE(*)
      DIMENSION NEWWAVE(KE_W,KN_S)
C
C     Local arguments
C
      INTEGER NLAT, K, NEWCOL, NEWROW
      INTEGER I_N, I_S
      REAL DELAT
      REAL DIST_N, DIST_S
      REAL LAT
      DIMENSION LAT(JPNMOUT+1)
      INTEGER INDEX
      DIMENSION INDEX(JPLLMAX)
C
      REAL OLDLATS(JPLLMAX), OLDEAST
      INTEGER NEXT, NUMNEW(JPLLMAX)
      INTEGER IRET, W251IDX
      EXTERNAL W251IDX
      INTEGER XKNUM,XKE_W,XKN_S
      DATA XKNUM/-1/,XKE_W/-1/,XKN_S/-1/
      REAL XRESON,XNORTH,XWEST
      DATA XRESON/-999.0/,XNORTH/-999.0/,XWEST/-999.0/
      INTEGER IFIRST, NEWIDX(3600*1801)
      DATA IFIRST/0/
      REAL DEPS
      DATA DEPS/0.00005/
C
      SAVE IFIRST,XKNUM,XKE_W,XKN_S,XRESON,XNORTH,XWEST,NEWIDX
C
C
C ---------------------------------------------------------------------
C*    Section 1. Initalisation.
C ---------------------------------------------------------------------
C
  100 CONTINUE
C
      WV2DINT = 0
      CALL INTLOG(JP_DEBUG,
     X   'WV2DINT: Wave interpolation requested.',JPQUIET)
C
C     Check only new-style 2D wave spectra (parameter 251)
C
      IF( KNSPEC.GT.1 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'WV2DINT: Only single-value 2D spectra field allowed',JPQUIET)
        CALL INTLOG(JP_ERROR,
     X    'WV2DINT: Value given = ',KNSPEC)
        WV2DINT = JPROUTINE + 1
        GOTO 900
      ENDIF
C
C     Check reduced latitude/longitude grid specification
C
      IF( KNUM.GT.JPLLMAX ) THEN
        CALL INTLOG(JP_ERROR,
     X    'WV2DINT: Number of latitudes in input lat/long grid = ',KNUM)
        CALL INTLOG(JP_ERROR,
     X    'WV2DINT: And is greater than allowed maximum = ',JPLLMAX)
        WV2DINT = JPROUTINE + 2
        GOTO 900
      ENDIF
C
C     Ensure working array dimensions are adequate for required output.
C
      IF( (KE_W.GT.JPNMOUT*2).OR.(KN_S.GT.JPNMOUT+1) ) THEN
        CALL INTLOG(JP_ERROR,
     X    'WV2DINT: Internal array dimensions are too small',JPQUIET)
        CALL INTLOG(JP_ERROR,
     X    'WV2DINT: for given lat/long output field.',JPQUIET)
        WV2DINT = JPROUTINE + 3
        GOTO 900
      ENDIF
C
C     Set up index for latitude lines in the input reduced lat/lon array
C
      INDEX(1) = 0
      DO K = 2, KNUM
        INDEX(K) = INDEX(K-1) + NUMPTS(K-1)
      ENDDO
C
C     Calculate latitudes and longitudes of output grid points.
C
C
      DO K = 1, KN_S
        LAT(K) = NORTH - FLOAT(K-1)*RESON
        NUMNEW(K)  = KE_W ! 0
      ENDDO

      DELAT  = RNS/(KNUM-1)

      DO k=1,KNUM
          OLDLATS(K) = NORTH - FLOAT(K-1)*DELAT
      END DO
C
      IF( ABS((KE_W*RESON)-360.0).LT.DEPS ) THEN
        OLDEAST = WEST + KE_W*RESON
      ELSE
        OLDEAST = WEST + (KE_W-1)*RESON
      ENDIF
C
C     Initialise all points with 'missing data' indicator
C
      DO NEWROW = 1, KN_S
        DO NEWCOL = 1, KE_W
          NEWWAVE(NEWCOL,NEWROW) = PMISS
        ENDDO
      ENDDO
C
C ---------------------------------------------------------------------
C*    Section 2. Interpolation.
C ---------------------------------------------------------------------
C
  200 CONTINUE
C
C     Only calculate the indices on the first time through
C
      IF( (IFIRST.EQ.0 )     .OR.
     X    ( KNUM.NE.XKNUM )  .OR.
     X    ( KE_W.NE.XKE_W )  .OR.
     X    ( KN_S.NE.XKN_S )  .OR.
     X    ( RESON.NE.XRESON ).OR.
     X    ( NORTH.NE.XNORTH ).OR.
     X    ( WEST.NE.XWEST ) ) THEN
        IRET = W251IDX(KNUM,NUMPTS,OLDLATS,WEST,OLDEAST,
     X                 KN_S,NUMNEW,LAT,NEWIDX)
        IFIRST = 1
        XKNUM  = KNUM
        XKE_W  = KE_W
        XKN_S  = KN_S
        XRESON = RESON
        XNORTH = NORTH
        XWEST  = WEST
      ENDIF
C
C      DELAT  = 180.0/(KNUM-1)
      DELAT  = RNS/(KNUM-1)
      NEXT = 0
C
      DO 220 NEWROW = 1, KN_S
C
C       Set up the distance between new row and the old rows to N and S.
C
        NLAT   = NINT((NORTH - LAT(NEWROW))/DELAT) + 1
        I_N    = NLAT
        I_S    = MIN(I_N+1,KNUM*2)
        DIST_N = ((NORTH - DELAT*(I_N-1)) - LAT(NEWROW)) / DELAT
        DIST_S = 1.0 - DIST_N
C
C       Check if the new interpolated row lies between 2 old rows which
C       have data points
C
        IF( (NUMPTS(I_N).GT.0).AND.(NUMPTS(I_S).GT.0) ) THEN
C
C         Yes, use the calculated indices
C
          DO 210 NEWCOL = 1, KE_W
C
            NEXT = NEXT + 1
C
            NEWWAVE(NEWCOL,NEWROW) = OLDWAVE(NEWIDX(NEXT))
C
  210     CONTINUE
        Else
           NEXT = NEXT + KE_W
        ENDIF
C
  220 CONTINUE
C
C ---------------------------------------------------------------------
C*    Section 9. Closedown.
C ---------------------------------------------------------------------
C
  900 CONTINUE
C
      IF( WV2DINT.EQ.0 ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'WV2DINT: Wave interpolation successful.',JPQUIET)
      ELSE
        CALL INTLOG(JP_DEBUG,
     X    'WV2DINT: Wave interpolation failed.',JPQUIET)
      ENDIF
C
      RETURN
      END
