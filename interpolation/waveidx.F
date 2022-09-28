C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION WAVEIDX(KOLDNUM,NUMOLD,OLDLATS,OLDWEST,OLDEAST,
     X                         KNEWNUM, NUMNEW, NEWLATS,
     X                         NEWIDX, DISTNEW)
C
C---->
C*****WAVEIDX*
C
C     PURPOSE
C     -------
C
C     Determines which nearest-neighbour values of an input global wave
C     2D-spectra grid field to use for an output global wave 2D-spectra
C     grid field.
C
C
C     INTERFACE
C     ---------
C
C     IRET = WAVEIDX(KOLDNUM,NUMOLD,OLDLATS,OLDWEST,OLDEAST,
C    X               KNEWNUM, NUMNEW, NEWLATS,
C    X               NEWIDX, DISTNEW)
C
C     Input arguments
C     ---------------
C
C     KOLDNUM - No. of meridians from North to South pole (input field)
C     NUMOLD  - Array giving number of points along each latitude
C               (empty latitudes have entry 0)
C     OLDLATS - input field latitudes
C     OLDWEST - western longitude of the input field (degrees)
C     OLDEAST - eastern longitude of the input field (degrees)
C
C     KNEWNUM - No. of meridians from North to South pole (output field)
C     NUMNEW  - Array giving number of points along each latitude
C               (empty latitudes have entry 0)
C     NEWLATS - output field latitudes
C
C     Output arguments
C     ----------------
C
C     NEWIDX  - Indices of four neighbours
C     DISTNEW - Distances of current 'box' edges from new point
C
C     Function returns 0 if the interpolation was OK.
C
C
C     METHOD
C     ------
C
C     The indices of the nearest four neighbouring grid points of the
C     input field is assigned to each point of the output grid.
C     The indices are zero if the output grid point is 'missing'.
C
C     The input field can be regular or quasi-regular, and can be
C     global or a subarea.
C     The output field can be regular or quasi-regular.
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
C     None
C
C
C     Author.
C     -------
C
C     J.D.Chambers      ECMWF    January  1998
C
C
C----<
C
      IMPLICIT NONE
C
#include "parim.h"
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER ( JPROUTINE = 19410 )
      INTEGER JPLLMAX, JPNMOUT
      PARAMETER ( JPLLMAX = 1801 ) ! allow up to 0.1 degree resolution
      PARAMETER ( JPNMOUT = 3600 ) ! allow up to 0.1 degree resolution
C
      INTEGER JPNW, JPNE, JPSW, JPSE, JPN, JPS
      INTEGER JPDISNW, JPDISNE, JPDISSW, JPDISSE
      PARAMETER (JPNW   = 1)
      PARAMETER (JPNE   = 2)
      PARAMETER (JPSW   = 3)
      PARAMETER (JPSE   = 4)
      PARAMETER (JPN    = 5)
      PARAMETER (JPS    = 6)
      PARAMETER (JPDISNW = 7)
      PARAMETER (JPDISNE = 8)
      PARAMETER (JPDISSW = 9)
      PARAMETER (JPDISSE = 10)
C
C     Function arguments
C
      INTEGER KOLDNUM, NUMOLD, KNEWNUM, NUMNEW
      DIMENSION NUMOLD(*), NUMNEW(*)
      REAL OLDWEST, OLDEAST, OLDLATS, NEWLATS
      DIMENSION OLDLATS(*), NEWLATS(*)
      INTEGER NEWIDX
      DIMENSION NEWIDX(4,*)
      REAL*4 DISTNEW
      DIMENSION DISTNEW(10,*)
C
C     Local arguments
C
      INTEGER NEWCOL, NEWROW, INCOL, LOOP, NEXT, NUMMAX
      INTEGER I_NW, I_SW, I_N, I_S, I_NE, I_SE
      REAL*8 DELONGN, DELONGS, DIFF, RESOL, DEPS
      REAL*8 DIST_NW, DIST_SW, DIST_NE, DIST_SE, DIST_N, DIST_S
      REAL*8 ZXIN, ZXIS, LON, OWEST, OEAST
      DIMENSION LON(JPNMOUT*2)
      LOGICAL LINGNS, LINGWE
      INTEGER INDEXI
      DIMENSION INDEXI(JPLLMAX)
      LOGICAL LGLOBAL
      DATA DEPS/0.0005/
      REAL*8 DINC
      LOGICAL LFULLG, LGWRAP
C
C ---------------------------------------------------------------------
C*    Section 1. Initalisation.
C ---------------------------------------------------------------------
C
  100 CONTINUE
C
      WAVEIDX = 0
      CALL INTLOG(JP_DEBUG,
     X   'WAVEIDX: Wave interpolation requested.',JPQUIET)
C
C     Check latitude/longitude grid specification
C
      IF( KOLDNUM.GT.JPLLMAX ) THEN
        CALL INTLOG(JP_ERROR,
     X    'WAVEIDX: Number of latitudes in input grid = ',KOLDNUM)
        CALL INTLOG(JP_ERROR,
     X    'WAVEIDX: And is greater than allowed maximum = ',JPLLMAX)
        WAVEIDX = JPROUTINE + 1
        GOTO 900
      ENDIF
C
C     Set up INDEXI for latitude lines in input lat/lon array
C
      INDEXI(1) = 0
      DO LOOP = 2, KOLDNUM
        INDEXI(LOOP) = INDEXI(LOOP-1) + NUMOLD(LOOP-1)
      ENDDO
C
      OEAST = OLDEAST
      OWEST = OLDWEST
C
C     Check whether the input field is global or a subarea
C
      NUMMAX = NUMOLD(1)
      DO LOOP = 2, KOLDNUM
        IF( NUMOLD(LOOP).GT.NUMMAX ) NUMMAX = NUMOLD(LOOP)
      ENDDO
      IF( NUMMAX.LE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'WAVEIDX: Input wave field has no points',JPQUIET)
        WAVEIDX = JPROUTINE + 2
        GOTO 900
      ENDIF
C
      LGWRAP = ABS((OEAST-OWEST) - DBLE(360.0)).LT.DEPS
      DINC = 360.0/DBLE(NUMMAX)
      LFULLG = ABS((DMOD((OEAST-OWEST+DBLE(360.0)),DBLE(360.0))
     X              +DINC)-DBLE(360.0)).LT.DEPS
C
      LGLOBAL = LFULLG.OR.LGWRAP
C
C ---------------------------------------------------------------------
C*    Section 2. Interpolation.
C ---------------------------------------------------------------------
C
  200 CONTINUE
C
      NEXT = 1
C
      DO 220 NEWROW = 1, KNEWNUM
C
C       Find old latitude rows to north and south of new latitude and
C       set up the distance between new row and the old rows to N and S.
C
        DO LOOP = 1, KOLDNUM -1
          IF( (NEWLATS(NEWROW).LE.OLDLATS(LOOP)) .AND.
     X        (NEWLATS(NEWROW).GT.OLDLATS(LOOP+1)) ) THEN
            I_N = LOOP
            I_S = MIN(LOOP+1,KOLDNUM)
            GOTO 205
          ENDIF
        ENDDO
C
        IF( NEWLATS(NEWROW).EQ.OLDLATS(KOLDNUM) ) THEN
          I_N = KOLDNUM
          I_S = KOLDNUM
        ELSE
          I_N = -1
          I_S = -1
        ENDIF
C
  205   CONTINUE
        IF( I_N.NE.I_S) THEN
          DIFF   = OLDLATS(I_N) - OLDLATS(I_S)
          DIST_N = (OLDLATS(I_N) - NEWLATS(NEWROW)) / DIFF
        ELSE
          DIST_N = 1.0
        ENDIF
        DIST_S = 1.0 - DIST_N
C
C       Check if the new interpolated row lies between 2 old rows which
C       have data points
C
        LINGNS = .FALSE.
        IF( I_N.GT.0 ) THEN
          IF( (NUMOLD(I_N).GT.0).AND.(NUMOLD(I_S).GT.0) ) THEN
C
C           Yes, so set up the grid increments.
C
C           Note that the grid increments are different depending on
C           whether or not the grid is global; global grids have an
C           extra interval at the end (eg between 358.5 and 0.0), while
C           subareas have outer limits defined by 'west' and 'east' and
C           a given number of points in between.
C
            IF( LGLOBAL ) THEN
              DELONGN = 360.0/DBLE(NUMOLD(I_N))
              DELONGS = 360.0/DBLE(NUMOLD(I_S))
            ELSE
              DELONGN = DMOD((OEAST-OWEST+DBLE(360.0)), DBLE(360.0))
     X                  / DBLE(NUMOLD(I_N)-1)
              DELONGS = DMOD((OEAST-OWEST+DBLE(360.0)), DBLE(360.0))
     X                  / DBLE(NUMOLD(I_S)-1)
            ENDIF
            LINGNS  = .TRUE.
          ENDIF
C
        ENDIF
C
C       Setup longitudes for the current row
C
        INCOL = NUMNEW(NEWROW)
        IF( INCOL.NE.0 ) THEN
          RESOL = 360.0/DBLE(INCOL)
          DO LOOP = 1, INCOL
            LON(LOOP) = DBLE(LOOP-1)*RESOL
          ENDDO
C
          DO 210 NEWCOL = 1, INCOL
C
            LINGWE = (LON(NEWCOL).GT.OWEST).OR.(LON(NEWCOL).LT.OEAST)
C
            IF( LINGNS .AND. LINGWE ) THEN
C
C             If point to be interpolated is inside the input field ,
C             work out distances from new point to four neighbours
C
              ZXIN = DMOD((LON(NEWCOL)+(DBLE(360.0)-OWEST)+DBLE(720.0)),
     X                     DBLE(360.0))
              ZXIS = ZXIN
              ZXIN = ZXIN/DELONGN + 1.0
              ZXIS = ZXIS/DELONGS + 1.0
              I_NW = INT(ZXIN)
              I_NE = I_NW + 1
              I_SW = INT(ZXIS)
              I_SE = I_SW + 1
C
C             Allow wrap-around
C
              I_NW = MOD(I_NW,NUMOLD(I_N))
              I_NE = MOD(I_NE,NUMOLD(I_N))
              I_SW = MOD(I_SW,NUMOLD(I_S))
              I_SE = MOD(I_SE,NUMOLD(I_S))
              IF( I_NW.EQ.0 ) I_NW = NUMOLD(I_N)
              IF( I_SW.EQ.0 ) I_SW = NUMOLD(I_S)
              IF( I_NE.EQ.0 ) I_NE = NUMOLD(I_N)
              IF( I_SE.EQ.0 ) I_SE = NUMOLD(I_S)
C
C             Calculate distance from interpolated point to its neighbours
C
              DIST_NW = ZXIN - REAL(I_NW)
              DIST_NE = 1.0 - DIST_NW
              DIST_SW = ZXIS - REAL(I_SW)
              DIST_SE = 1.0 - DIST_SW
C
              DISTNEW(JPDISNW,NEXT) = (DIST_NW*DIST_NW + DIST_N*DIST_N)
              DISTNEW(JPDISNE,NEXT) = (DIST_NE*DIST_NE + DIST_N*DIST_N)
              DISTNEW(JPDISSW,NEXT) = (DIST_SW*DIST_SW + DIST_S*DIST_S)
              DISTNEW(JPDISSE,NEXT) = (DIST_SE*DIST_SE + DIST_S*DIST_S)
C
              DISTNEW(JPNW,NEXT) = DIST_NW
              DISTNEW(JPNE,NEXT) = DIST_NE
              DISTNEW(JPSW,NEXT) = DIST_SW
              DISTNEW(JPSE,NEXT) = DIST_SE
              DISTNEW(JPN ,NEXT) = DIST_N
              DISTNEW(JPS ,NEXT) = DIST_S
C
              NEWIDX(JPNW,NEXT) = INDEXI(I_N)+I_NW
              NEWIDX(JPNE,NEXT) = INDEXI(I_N)+I_NE
              NEWIDX(JPSW,NEXT) = INDEXI(I_S)+I_SW
              NEWIDX(JPSE,NEXT) = INDEXI(I_S)+I_SE
C
C           If the point to be interpolated is outside the input field,
C           set it to 'land'.
C
            ELSE
C
              DISTNEW(JPNW,NEXT) = 0
              DISTNEW(JPNE,NEXT) = 0
              DISTNEW(JPSW,NEXT) = 0
              DISTNEW(JPSE,NEXT) = 0
              DISTNEW(JPN ,NEXT) = 0
              DISTNEW(JPS ,NEXT) = 0
C
              DISTNEW(JPDISNW,NEXT) = 0
              DISTNEW(JPDISNE,NEXT) = 0
              DISTNEW(JPDISSW,NEXT) = 0
              DISTNEW(JPDISSE,NEXT) = 0
C
              NEWIDX(JPNW,NEXT) = 0
              NEWIDX(JPNE,NEXT) = 0
              NEWIDX(JPSW,NEXT) = 0
              NEWIDX(JPSE,NEXT) = 0
C
            ENDIF
C
            NEXT = NEXT + 1
C
  210     CONTINUE
C
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
      IF( WAVEIDX.EQ.0 ) THEN
        CALL INTLOG(JP_DEBUG, 'WAVEIDX: successful.',JPQUIET)
      ELSE
        CALL INTLOG(JP_ERROR, 'WAVEIDX: failed.',JPQUIET)
      ENDIF
C
      RETURN
      END
