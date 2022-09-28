C
C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION WVQLIN2(KNUM,NUMPTS,KE_W,KN_S,RESON,OLDWAVE,
     X                         NEWWAVE,NORTH,WEST,KPARAM,PMISS)
C
C---->
C*****WVQLIN2*
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
C     IRET = WVQLIN2(KNUM,NUMPTS,KE_W,KN_S,RESON,OLDWAVE,
C    X               NEWWAVE,NORTH,WEST,KPARAM,PMISS)
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
C     NORTH   - Input and output grid northernmost latitude (degree)
C     WEST    - Input and output grid westernmost  longitude (degree)
C     KPARAM  - Field parameter code
C     PMISS   - Missing value indicator
C
C     Output arguments
C     ----------------
C
C     NEWWAVE - New wave field
C
C     Function returns 0 if the interpolation was OK.
C
C
C     METHOD
C     ------
C
C     A bi-linear interpolation is only done if all four neighbouring
C     points are sea.
C     The nearest neighbouring grid point value is taken if one of the
C     neighbouring points is missing.
C     The interpolated point value is set to the 'missing' indicator
C     value if the point is outside the input grid.
C
C     A field representing a 'direction' is resolved into components
C     each of which is interpolated separately.
C
C     The output area is the same as the input area.
C
C
C     EXTERNALS
C     ---------
C
C     WVQLID2 - Calculate the indices of neighbouring points
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
C     J.D.Chambers      ECMWF    September 1996
C
C     J.D.Chambers      ECMWF    September 1998
C     Modified to handle subarea input/output.
C     The output area is the same as the input area.
C
C     J.D.Chambers      ECMWF    November 2000
C     Modified to input grid resolution to be different from the
C     output grid resolution.
C
C----<
C
      IMPLICIT NONE
C
#include "parim.h"
C
C     Parameters
C
      INTEGER JPROUTINE, JPLLMAX, JPNMOUT,
     X        JPNW, JPNE, JPSW, JPSE, JPN, JPS,
     X        JPDISNW, JPDISNE, JPDISSW, JPDISSE
      PARAMETER( JPROUTINE = 19400 )
      PARAMETER( JPLLMAX   =  1801 ) ! allow up to 0.1 degree resolution
      PARAMETER( JPNMOUT   =  1800 ) ! allow up to 0.1 degree resolution
      PARAMETER( JPNW=1, JPNE=2, JPSW=3, JPSE=4, JPN=5, JPS=6,
     X           JPDISNW=7, JPDISNE=8, JPDISSW=9, JPDISSE=10 )
C
C     Arguments
C
      INTEGER KNUM, NUMPTS(*), KE_W, KN_S, KPARAM
      REAL RESON, NORTH, WEST, PMISS
      REAL RNS !FIXME: Unused! Difference in degrees in NS disrection
      REAL OLDWAVE(*), NEWWAVE(*)
C
C     Local variables
C
      INTEGER NLAT, K, I, NEWROW, NEWCOL, I1N, I1S, I_N, I_S, I2N, I2S
      REAL*4 DELONGN, DELONGS, DELAT, DI1N, DI1S, DI2N, DI2S
      REAL*4 DIST_N, DIST_S
      REAL*4 DISNW, DISNE, DISSW, DISSE
      REAL*4 U1, U2, ZXIN, ZXIS
      REAL LON(JPNMOUT*2), LAT(JPNMOUT+1)
      LOGICAL INGRID, LDIREC
      INTEGER NW_PT, NE_PT, SW_PT, SE_PT
      REAL*4 NWVALUE, NEVALUE, SWVALUE, SEVALUE
      REAL*4 RAD
      REAL*4 CNW_PT, CNE_PT, CSW_PT, CSE_PT
      REAL*4 SNW_PT, SNE_PT, SSW_PT, SSE_PT
      REAL*4 C1, C2, S1, S2, CC, SS
C
      DATA RAD/0.017453293/
C
      REAL OLDLATS(JPLLMAX), OLDEAST
      REAL OLDNS
      INTEGER ISTART, IEND
      INTEGER LOCATE
      INTEGER NEXT, NUMNEW(JPLLMAX)
      INTEGER IRET
      INTEGER XKNUM,XKE_W,XKN_S
      DATA XKNUM/-1/,XKE_W/-1/,XKN_S/-1/
      REAL XRESON,XNORTH,XWEST
      DATA XRESON/-999.0/,XNORTH/-999.0/,XWEST/-999.0/
      INTEGER IFIRST, NEWIDX(4,3600*1801)
      DATA IFIRST/0/
      REAL*4 DISTNEW(10,3600*1801)
C
      INTEGER IOS
      INTEGER OLDKNUM
      DATA OLDKNUM/0/
      INTEGER INDEX(JPLLMAX)
      REAL DEPS
      DATA DEPS/0.00005/
C     Inline function - tests TRUE if A is a missing value
C     EPS      Tolerance on missing data flag
      REAL EPS
      DATA EPS/0.1/
      LOGICAL LSW,LSE,LNE,LNW,LTEMP
C
      SAVE IFIRST,XKNUM,XKE_W,XKN_S,XRESON,XNORTH,XWEST,NEWIDX,DISTNEW
C
C     Externals
C
      INTEGER WVQLID2
      LOGICAL IS_WAVE_DIRECTION
      EXTERNAL WVQLID2, IS_WAVE_DIRECTION
C
C     -----------------------------------------------------------------|
C*    Section 1. Initalisation.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      WVQLIN2 = 0
      CALL INTLOG(JP_DEBUG,
     X   'WVQLIN2: Wave interpolation requested.',JPQUIET)
C
C     Check reduced latitude/longitude grid specification
C
      IF( KNUM.GT.JPLLMAX ) THEN
        CALL INTLOG(JP_ERROR,
     X    'WVQLIN2: Number of latitudes in input lat/long grid = ',KNUM)
        CALL INTLOG(JP_ERROR,
     X    'WVQLIN2: And is greater than allowed maximum = ',JPLLMAX)
        WVQLIN2 = JPROUTINE + 1
        GOTO 900
      ENDIF
C
C     Ensure working array dimensions are adequate for required output.
C
      IF( (KE_W.GT.JPNMOUT*2).OR.(KN_S.GT.JPNMOUT+1) ) THEN
        CALL INTLOG(JP_ERROR,
     X    'WVQLIN2: Internal array dimensions are too small',JPQUIET)
        CALL INTLOG(JP_ERROR,
     X    'WVQLIN2: for given lat/long output field.',JPQUIET)
        WVQLIN2 = JPROUTINE + 3
        GOTO 900
      ENDIF
C
C     Set up index for latitude lines in the input reduced lat/lon array
C
      INDEX(1) = 0
      DO K = 2, KNUM
        INDEX(K) = INDEX(K-1) + NUMPTS(K-1)
      ENDDO

      DO K = 1, KN_S
        LAT(K)     = NORTH - FLOAT(K-1)*RESON
        NUMNEW(K)  = KE_W
      ENDDO
C
C     Calculate latitudes and longitudes of output grid points.
C
      DO I = 1,KE_W
        LON(I) = FLOAT(I-1)*RESON
      ENDDO
C
C     Calculate latitudes for input (old) grid.
C     (Input and output area are same)
C
      OLDNS =  RNS/REAL(KNUM-1)
      DO K = 1, KNUM
        OLDLATS(K) = NORTH - FLOAT(K-1)*OLDNS
      ENDDO
C
C     Find first and last latitudes in input grid which are not empty
C
      ISTART = 1
      DO K = 1, KNUM/2
        IF( NUMPTS(K).EQ.0 ) ISTART = ISTART + 1
      ENDDO
C
      IEND = KNUM
      DO K = KNUM, (KNUM/2) + 1, -1
        IF( NUMPTS(K).EQ.0 ) IEND = IEND - 1
      ENDDO
C
C     For the regular output grid, calculate its latitudes and fill
C     in the number of points along each latitude.
C
C
C     Setup East depending on whether or not there is wrap-around
C
      IF( ABS((KE_W*RESON)-360.0).LT.DEPS ) THEN
        OLDEAST = WEST + KE_W*RESON
      ELSE
        OLDEAST = WEST + (KE_W-1)*RESON
      ENDIF
C
C     Initialise all points with 'missing data' indicator
C
      NEXT  = 0
      DO NEWROW = 1, KN_S
        DO NEWCOL = 1, KE_W
          NEXT = NEXT + 1
          NEWWAVE(NEXT) = PMISS
        ENDDO
      ENDDO
C
C     Wave direction parameters need special handling
      LDIREC = IS_WAVE_DIRECTION(KPARAM)
C
C     -----------------------------------------------------------------|
C*    Section 2. Interpolation.
C     -----------------------------------------------------------------|
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
C
        IRET =  WVQLID2(KNUM,NUMPTS,OLDLATS,WEST,OLDEAST,
     X                  KN_S,NUMNEW,LAT,NEWIDX,DISTNEW)
        IFIRST = 1
        XKNUM  = KNUM
        XKE_W  = KE_W
        XKN_S  = KN_S
        XRESON = RESON
        XNORTH = NORTH
        XWEST  = WEST
      ENDIF
C
      DELAT = OLDNS
      NEXT  = 0
C
      DO 220 NEWROW = 1, KN_S
C
C       Set up the distance between new row and the old rows to N and S.
C
        NLAT  = NINT((NORTH - LAT(NEWROW))/DELAT) + 1
        I_N   = NLAT
        I_S   = MIN(I_N+1,KNUM*2)
        DIST_N  = ((NORTH - DELAT*(I_N-1)) - LAT(NEWROW)) / DELAT
        DIST_S  = 1.0 - DIST_N
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
            NW_PT = NEWIDX(JPNW,NEXT)
            NE_PT = NEWIDX(JPNE,NEXT)
            SW_PT = NEWIDX(JPSW,NEXT)
            SE_PT = NEWIDX(JPSE,NEXT)
            NWVALUE = OLDWAVE(NW_PT)
            NEVALUE = OLDWAVE(NE_PT)
            SWVALUE = OLDWAVE(SW_PT)
            SEVALUE = OLDWAVE(SE_PT)
C
C           Test if any one of the four neighbouring points is missing.
C
              LTEMP = .FALSE.
              LSE = .FALSE.
             IF(ABS(SEVALUE-PMISS) .LT. EPS)   LSE = .TRUE.
              LSW = .FALSE.
             IF(ABS(SWVALUE-PMISS) .LT. EPS)   LSW = .TRUE.
              LNE = .FALSE.
             IF(ABS(NEVALUE-PMISS) .LT. EPS)   LNE = .TRUE.
              LNW = .FALSE.
             IF(ABS(NWVALUE-PMISS) .LT. EPS)   LNW = .TRUE.
C
            IF( (NW_PT.EQ.0 .OR. LNW) .OR.
     X          (NE_PT.EQ.0 .OR. LNE) .OR.
     X          (SW_PT.EQ.0 .OR. LSW) .OR.
     X          (SE_PT.EQ.0 .OR. LSE) ) THEN
C             If so, take nearest grid point value.
C
              DISNW = DISTNEW(JPDISNW,NEXT)
              DISNE = DISTNEW(JPDISNE,NEXT)
              DISSW = DISTNEW(JPDISSW,NEXT)
              DISSE = DISTNEW(JPDISSE,NEXT)
C
              IF( (DISNW.LE.DISNE).AND.
     X            (DISNW.LE.DISSW).AND.
     X            (DISNW.LE.DISSE) ) THEN
cs                NEWWAVE(NEXT) = NWVALUE
                LOCATE = NW_PT
                LTEMP  = LNW
C
              ELSE IF( (DISNE.LE.DISSW).AND.
     X                 (DISNE.LE.DISSE) ) THEN
cs                NEWWAVE(NEXT) = NEVALUE
                LOCATE = NE_PT
                LTEMP  = LNE
C
              ELSE IF( (DISSW.LE.DISSE) ) THEN
cs                NEWWAVE(NEXT) = SWVALUE
                LOCATE = SW_PT
                LTEMP  = LSW
C
              ELSE
cs                NEWWAVE(NEXT) = SEVALUE
                LOCATE = SE_PT
                LTEMP  = LSE
C
              ENDIF

              IF( .NOT.LTEMP.AND.LOCATE.NE.0) THEN
c              IF( LOCATE.NE.0) THEN
                  NEWWAVE(NEXT) = OLDWAVE(LOCATE)
              ENDIF

C
            ELSE
C
C             Use bi-linear interpolation from four
C             neighbouring sea points.
C
              DI1N = DISTNEW(JPNW,NEXT)
              DI2N = DISTNEW(JPNE,NEXT)
              DI1S = DISTNEW(JPSW,NEXT)
              DI2S = DISTNEW(JPSE,NEXT)
              DIST_N  = DISTNEW(JPN,NEXT)
              DIST_S  = DISTNEW(JPS,NEXT)
C
              NW_PT = OLDWAVE(NW_PT)
              NE_PT = OLDWAVE(NE_PT)
              SW_PT = OLDWAVE(SW_PT)
              SE_PT = OLDWAVE(SE_PT)
C
              IF( .NOT. LDIREC ) THEN
                U1 = NWVALUE*DI2N + NEVALUE*DI1N
                U2 = SWVALUE*DI2S + SEVALUE*DI1S
                NEWWAVE(NEXT) = U1*DIST_S + U2*DIST_N
C
              ELSE
C
C               Fields representing a 'direction': resolve into
C               components and interpolate each separately.
C
                CNW_PT = COS(NWVALUE*RAD)
                CNE_PT = COS(NEVALUE*RAD)
                CSW_PT = COS(SWVALUE*RAD)
                CSE_PT = COS(SEVALUE*RAD)
                SNW_PT = SIN(NWVALUE*RAD)
                SNE_PT = SIN(NEVALUE*RAD)
                SSW_PT = SIN(SWVALUE*RAD)
                SSE_PT = SIN(SEVALUE*RAD)
                C1 = CNW_PT*DI2N + CNE_PT*DI1N
                C2 = CSW_PT*DI2S + CSE_PT*DI1S
                CC = C1*DIST_S + C2*DIST_N
                S1 = SNW_PT*DI2N + SNE_PT*DI1N
                S2 = SSW_PT*DI2S + SSE_PT*DI1S
                SS = S1*DIST_S + S2*DIST_N
                IF( SS.LT.0.0 ) THEN
                  NEWWAVE(NEXT) = ATAN2(SS,CC)/RAD + 360.0
                ELSE
                  NEWWAVE(NEXT) = ATAN2(SS,CC)/RAD
                ENDIF
              ENDIF
            ENDIF
  210     CONTINUE
        ELSE
           NEXT = NEXT + KE_W
        ENDIF
C
  220 CONTINUE
C
C     -----------------------------------------------------------------|
C*    Section 9. Closedown.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      IF( WVQLIN2.EQ.0 ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'WVQLIN2: Wave interpolation successful.',JPQUIET)
      ELSE
        CALL INTLOG(JP_DEBUG,
     X    'WVQLIN2: Wave interpolation failed.',JPQUIET)
      ENDIF
c     call abort()
C
      RETURN
      END
