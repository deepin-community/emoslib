C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION W251IDX(KOLDNUM,NUMOLD,OLDLATS,OLDWEST,OLDEAST,
     X                         KNEWNUM,NUMNEW,NEWLATS,NEWIDX)
C
C---->
C*****W251IDX*
C
C     PURPOSE
C     -------
C
C     Determines which nearest-neighbour values of an input wave
C     2D-spectra grid field to use for an output wave 2D-spectra
C     grid field.
C
C
C     INTERFACE
C     ---------
C
C     IRET = W251IDX(KOLDNUM,NUMOLD,OLDLATS,OLDWEST,OLDEAST,
C    X               KNEWNUM,NUMNEW,NEWLATS,NEWIDX)
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
C     NEWIDX - Indices of points to use
C
C     Function returns 0 if the interpolation was OK.
C
C
C     METHOD
C     ------
C
C     The index of the nearest neighbouring grid point value of
C     the input field is assigned to each point of the output
C     grid.
C     The index is zero if the output grid point is 'missing'.
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
C     J.D.Chambers      ECMWF    October  1997
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
      INTEGER JPLLMAX
      PARAMETER ( JPLLMAX = 1801 )
C                             `--> allow upto 0.1 degree resolution.
      INTEGER JPNMOUT
      PARAMETER ( JPNMOUT = 1800 )
C                             `--> allow upto 0.1 degree resolution.
C
C     Function arguments
C
      INTEGER KOLDNUM, NUMOLD, KNEWNUM, NUMNEW
      DIMENSION NUMOLD(*), NUMNEW(*)
      REAL OLDWEST, OLDEAST, OLDLATS, NEWLATS
      DIMENSION OLDLATS(*), NEWLATS(*)
      INTEGER NEWIDX
      DIMENSION NEWIDX(*)
C
C     Local arguments
C
      INTEGER NEWCOL, NEWROW, INCOL, LOOP, NEXT, NUMMAX
      INTEGER I_NW, I_SW, I_N, I_S, I_NE, I_SE
      REAL*8 DELONGN, DELONGS, DIFF, RESOL, DINC, DEPS
      REAL*8 DIST_NW, DIST_SW, DIST_NE, DIST_SE, DIST_N, DIST_S
      REAL*4 DISNW, DISNE, DISSW, DISSE
      REAL*8 ZXIN, ZXIS, LON, OWEST, OEAST
      DIMENSION LON(JPNMOUT*2)
      LOGICAL LINGNS, LINGWE
      INTEGER INDEXI
      DIMENSION INDEXI(JPLLMAX)
      LOGICAL LGLOBAL, LFULLG, LGWRAP
      DATA DEPS/0.00005/
C
C ---------------------------------------------------------------------
C*    Section 1. Initalisation.
C ---------------------------------------------------------------------
C
  100 CONTINUE
C
      DELONGN = 0.
      DELONGS = 0.
      W251IDX = 0
      CALL INTLOG(JP_DEBUG,
     X   'W251IDX: Wave interpolation requested.',JPQUIET)
C
C     Check latitude/longitude grid specification
C
      IF( KOLDNUM.GT.JPLLMAX ) THEN
        CALL INTLOG(JP_ERROR,
     X    'W251IDX: Number of latitudes in input grid = ',KOLDNUM)
        CALL INTLOG(JP_ERROR,
     X    'W251IDX: And is greater than allowed maximum = ',JPLLMAX)
        W251IDX = JPROUTINE + 1
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
     X    'W251IDX: Input wave field has no points',JPQUIET)
        W251IDX = JPROUTINE + 2
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
            IF( LGLOBAL ) THEN
             RESOL = 360.0/DBLE(INCOL)
            ELSE
             RESOL = (OEAST-OWEST)/(DBLE(INCOL)-1)
            ENDIF
             DO LOOP = 1, INCOL
               LON(LOOP) = OLDWEST + DBLE(LOOP-1)*RESOL
             ENDDO
C
          DO 210 NEWCOL = 1, INCOL
C
            LINGWE = (LON(NEWCOL).GT.OWEST).OR.(LON(NEWCOL).LT.OEAST)
C
            IF( LINGNS .AND. LINGWE ) THEN
C
C             If point to be interpolated is inside the input field ,
C             work out distances from new point to four neighbours.
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
C             Take nearest grid point value.
C
              DISNW = DIST_NW*DIST_NW + DIST_N*DIST_N
              DISNE = DIST_NE*DIST_NE + DIST_N*DIST_N
              DISSW = DIST_SW*DIST_SW + DIST_S*DIST_S
              DISSE = DIST_SE*DIST_SE + DIST_S*DIST_S
C
              IF( (DISNW.LE.DISNE).AND.(DISNW.LE.DISSW).AND.
     X            (DISNW.LE.DISSE) ) THEN
C
C               Use north-west neighbour
C
                NEWIDX(NEXT) = INDEXI(I_N)+I_NW
C
              ELSE IF( (DISNE.LE.DISSW).AND.(DISNE.LE.DISSE) ) THEN
C
C               Use north-east neighbour
C
                NEWIDX(NEXT) = INDEXI(I_N)+I_NE
C
              ELSE IF( DISSW.LE.DISSE ) THEN
C
C               Use south-west neighbour
C
                NEWIDX(NEXT) = INDEXI(I_S)+I_SW
              ELSE
C
C               Use south-east neighbour
C
                NEWIDX(NEXT) = INDEXI(I_S)+I_SE
              ENDIF
C
C           If the point to be interpolated is outside the input field,
C           set it to 'land'.
C
            ELSE
              NEWIDX(NEXT) = 0
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
      IF( W251IDX.EQ.0 ) THEN
        CALL INTLOG(JP_DEBUG, 'W251IDX: successful.',JPQUIET)
      ELSE
        CALL INTLOG(JP_ERROR, 'W251IDX: failed.',JPQUIET)
      ENDIF
C
      RETURN
      END
