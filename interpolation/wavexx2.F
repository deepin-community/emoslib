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

      INTEGER FUNCTION WAVEXX2(NPARAM, NUMLATS, NPTS,
     X                         NLATS, STEPNS, STEPWE,
     X                         OLDWAVE, NEWWAVE, NORTH, WEST, PMISS)
C
C---->
C*****WAVEXX2*
C
C     PURPOSE
C     -------
C
C     Interpolates wave fields (except 2D spectra).
C
C
C     INTERFACE
C     ---------
C
C     IRET = WAVEXX2(NPARAM, NUMLATS, NPTS,
C    X               NLATS, STEPNS, STEPWE,
C    X               OLDWAVE, NEWWAVE, NORTH, WEST, PMISS)
C
C     Input arguments
C     ---------------
C
C     NPARAM  - Parameter ID
C     NUMLATS - Input lat number north-south
C     NPTS    - Array giving number of points along each latitude
C               for input field (empty latitudes have entry 0)
C     NLATS   - Number of points N-S in new grid
C     STEPNS  - Output grid north-south resolution (degrees)
C     STEPWE  - Output grid west-east resolution (degrees)
C     OLDWAVE - Original wave field
C     NORTH   - Output grid northernmost latitude (degrees)
C     WEST    - Output grid westernmost longitude (degrees)
C     PMISS   - Missing data value
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
C     Builds the index of neighbouring points for the output grid.
C     Then works through the output grid points, checking for subarea
C     boundaries and looking up neighbouring point values and weights
C     (which may be missing data).
C
C
C     EXTERNALS
C     ---------
C
C     WAVEIDX - Determines which nearest neighbour points to use for
C               interpolating to new output grid point
C     NUMPTWE - Calculates number of grid points between west/east
C               area boundaries
C     INTLOG  - Log error message
C     JMALLOC - Dynamically allocate memory (deallocation -JFREE- not used)
C
C
C     REFERENCE
C     ---------
C
C     None.
C
C
C     Author.
C     -------
C
C     S. Curic      ECMWF    Jun 2009
C
C
C
C----<
C
      IMPLICIT NONE
C
C     Parameters
C
#include "parim.h"
C
      INTEGER JPROUTINE, JPMXLAT,
     X        JPNW, JPNE, JPSW, JPSE, JPN, JPS,
     X        JPDISNW, JPDISNE, JPDISSW, JPDISSE
      PARAMETER( JPROUTINE = 40100 )
      PARAMETER( JPMXLAT   =  1801 ) ! allow up to 0.1 degree resolution
      PARAMETER( JPNW=1, JPNE=2, JPSW=3, JPSE=4, JPN=5, JPS=6,
     X           JPDISNW=7, JPDISNE=8, JPDISSW=9, JPDISSE=10 )
C
#include "nifld.common"
#include "nofld.common"
#include "grspace.h"
C
C     Arguments
C
      INTEGER NPARAM
      INTEGER NUMLATS, NPTS(*), NLATS
      REAL STEPNS, STEPWE, OLDWAVE(*), NEWWAVE(*), NORTH, WEST, PMISS
C
C     Local variables
C
      INTEGER IEOFSET, INDEX, ISTART, IWEST, IWOFSET, KNEWNUM,
     X        KOLDNUM, LOOP, MISSLAT, NCOL, NEXT,
     X        NEXTWV, NROW, NUMNEW(JPMXLAT)
      REAL    AWEST, EAST, NEWLATS(JPMXLAT), OLDLATS(JPMXLAT),
     X        ONORTH, OSOUTH, PTLAT, PTLONG, RLATINC, ROWINC,
     X        SOUTH, OEAST, OWEST
      INTEGER INE, INW, ISE, ISW
      LOGICAL LDIREC
      REAL    C1, C2, CC, CNE_PT, CNW_PT, CSE_PT, CSW_PT, DI1N, DI1S,
     X        DI2N, DI2S, DISNE, DISNW, DISSE, DISSW, DK1, DK2, NE_PT,
     X        NW_PT, RAD, S1, S2, SE_PT, SNE_PT, SNW_PT, SS, SSE_PT,
     X        SSW_PT, SW_PT, U1, U2
C
      DATA RAD/0.01745329251/
C
C     Large, resolution-dependent arrays
C
      LOGICAL IS_ALLOCATED_LARGEARRAY
      DATA    IS_ALLOCATED_LARGEARRAY /.FALSE./
      SAVE    IS_ALLOCATED_LARGEARRAY
C
      INTEGER NEWIDX(4,JPARRAYDIM_WAVE)
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 P_NEWIDX
#endif
#endif
      POINTER(P_NEWIDX,NEWIDX)
      SAVE P_NEWIDX
C
      REAL*4 DISTNEW(10,JPARRAYDIM_WAVE)
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 P_DISTNEW
#endif
#endif
      POINTER(P_DISTNEW,DISTNEW)
      SAVE P_DISTNEW
C
C     Externals
C
      INTEGER  WAVEIDX, NUMPTWE
#ifdef POINTER_64
      INTEGER*8 JMALLOC
#else
      INTEGER JMALLOC
#endif
      LOGICAL IS_WAVE_DIRECTION
      EXTERNAL WAVEIDX, NUMPTWE, JMALLOC, IS_WAVE_DIRECTION
C
C     -----------------------------------------------------------------|
C*    Section 1. Initalisation.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      WAVEXX2 = 0
C
C     Initialise the bitmap value lookup function
C
      MISSLAT = 0

      ONORTH = FLOAT(NIAREA(1))/PPMULT
      OSOUTH = FLOAT(NIAREA(3))/PPMULT
      RLATINC = FLOAT(NIGRID(2))/PPMULT
C
C     Calculate number of latitudes if grid had been full from
C     North pole to South pole
C
      IF( NUMLATS.GT.JPMXLAT ) THEN
        CALL INTLOG(JP_ERROR,
     X    'WAVEXX2: Number of latitudes in input grid = ',NUMLATS)
        CALL INTLOG(JP_ERROR,
     X    'WAVEXX2: And is greater than allowed maximum = ',JPMXLAT)
        WAVEXX2 = JPROUTINE + 1
        GOTO 900
      ENDIF
C
C
C     Fill an array with the number of points at each latitude for the
C     input field.
C
      IF(NIREPR.EQ.JPREDLL) THEN
C
C       Input field is a reduced latitude/longitude grid
C
C       .. but it may be 'pseudo-gaussian' in layout
C       (ie global, symmetric about the equator but no latitude
C        at the equator)
C
        IF( (ONORTH.NE.90.0).AND.(MOD(NUMLATS,2).EQ.0) ) THEN
C
          ONORTH = FLOAT(NIAREA(1))
          OSOUTH = FLOAT(NIAREA(3))
          RLATINC = FLOAT(NIGRID(2))

        ENDIF
C
        DO LOOP = 1, NUMLATS
          OLDLATS(LOOP) = 90.0 - (LOOP-1)*RLATINC
        ENDDO
C
      ELSE
C
C       Input field is a regular latitude/longitude grid
C
        DO LOOP = 1, NUMLATS
          OLDLATS(LOOP) = ONORTH - (LOOP-1)*RLATINC
        ENDDO
C
        NPTS(1:NUMLATS) = NIWE
C
      ENDIF
C
C     Allocate large, resolution-dependent arrays
      IF (.NOT. IS_ALLOCATED_LARGEARRAY) THEN
        P_NEWIDX  = JMALLOC( 4*JPARRAYDIM_WAVE*JPBYTES)  !  4*len*bytes
        P_DISTNEW = JMALLOC(10*JPARRAYDIM_WAVE*4)        ! 10*len*bytes
        IF( (P_NEWIDX.EQ.0) .OR. (P_DISTNEW.EQ.0) ) THEN
          CALL INTLOG(JP_ERROR,'WAVEXX2: JMALLOC fail',JPQUIET)
          WAVEXX2 = JPROUTINE + 1
          GOTO 900
        ENDIF
        IS_ALLOCATED_LARGEARRAY = .TRUE.
      END IF
C
C     -----------------------------------------------------------------|
C*    Section 2. Setup number of points at each latitude for the
C                output latitude/longitude field.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IF( (NOREPR.EQ.JPQUASI) .OR. (NOREPR.EQ.JPGAUSSIAN) ) THEN
C
C       Reduced (quasi-regular) gaussian output
C
        KNEWNUM = NOGAUSS*2
        DO LOOP = 1, KNEWNUM
          NUMNEW(LOOP)  = NOLPTS(LOOP)
          NEWLATS(LOOP) = ROGAUSS(LOOP)
        ENDDO
C
      ELSE IF( NOREPR.EQ.JPREDLL ) THEN
C
C       Reduced (quasi-regular) lat/long output
C
        KNEWNUM = NOREDLL
        DO LOOP = 1, KNEWNUM
          NUMNEW(LOOP)  = NOLPTS(LOOP)
          NEWLATS(LOOP) = ROREDLL(LOOP)
        ENDDO
C
      ELSE
C
C       Regular output
C
        MISSLAT = NINT((90.0 - NORTH)/STEPNS)
        DO LOOP = 1, MISSLAT
          NUMNEW(LOOP)    = 0
        ENDDO
        DO LOOP = 1, NLATS
          NUMNEW(LOOP+MISSLAT) = NINT(360.0/STEPWE)
        ENDDO
C
        KNEWNUM = MISSLAT + NLATS
        DO LOOP = 1, KNEWNUM
          NEWLATS(LOOP) = 90.0 - (LOOP-1)*STEPNS
        ENDDO
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3. Get the input GRIB field characteristics.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
C     Calculate the indices of the input grid points to be used for
C     the output points
C
      OWEST = FLOAT(NIAREA(2))/PPMULT
      OEAST = FLOAT(NIAREA(4))/PPMULT

      WAVEXX2 = WAVEIDX(NUMLATS,NPTS,OLDLATS,OWEST,OEAST,
     X                  KNEWNUM, NUMNEW, NEWLATS, NEWIDX, DISTNEW)
      IF( WAVEXX2.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'WAVEXX2: Unable to calculate output grid indices',JPQUIET)
        WAVEXX2 = JPROUTINE + 3
        GOTO 900
      ENDIF
C
C     Wave direction parameters need special handling
      LDIREC = IS_WAVE_DIRECTION(NPARAM)
C
C     -----------------------------------------------------------------|
C*    Section 4. Work through the output subarea.
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
C     Fill in the wave spectra values
C
      NEXT = 0
      NEXTWV = 0
C
      SOUTH = NOAREA(3)/PPMULT
      EAST  = NOAREA(4)/PPMULT
      ISTART = 0
C
C     Work down through latitudes from north to south.
C
      DO NROW = 1, KNEWNUM
C
C       If inside north-south (subarea) boundaries ..
C
        IF( (NOREPR.EQ.JPGAUSSIAN).OR.(NOREPR.EQ.JPQUASI) ) THEN
          PTLAT = ROGAUSS(NROW)
        ELSE
          PTLAT = 90.0 - (NROW-1)*STEPNS
        ENDIF
C
        IF( (PTLAT.LE.NORTH).AND.(ABS(PTLAT-SOUTH).GT.-0.0005) ) THEN
C
C         Calculate number of points between west boundary of area and
C         Greenwich
C
          ROWINC = 360.0/NUMNEW(NROW)
C
          IWEST = INT(WEST/ROWINC)
          IF (ABS(WEST - IWEST*ROWINC).GT.0.000001) THEN
            IF (WEST.GT.0.0) IWEST = IWEST + 1
            IF (WEST.LT.0.0) IWEST = IWEST - 1
          ENDIF
          AWEST = IWEST * ROWINC
          IWOFSET = NUMPTWE(AWEST,0.0,ROWINC)
          IEOFSET = NUMPTWE(AWEST,EAST,ROWINC)
C
C         Work through subarea longitudes from west to east.
C
          DO NCOL = 1, NUMNEW(NROW)
            PTLONG = AWEST + (NCOL-1)*ROWINC
            NEXT = NUMPTWE(AWEST,PTLONG,ROWINC)
            IF( (NEXT.LE.IEOFSET).AND.(NEXT.GE.0) ) THEN
C
C             .. and inside west-east (subarea) boundaries
C
              NEXT = 1 + NEXT - IWOFSET
              IF( NEXT.LE.0) NEXT = NEXT + NUMNEW(NROW)
              NEXT = NEXT + ISTART
              NEXTWV = NEXTWV + 1
C
              INW = NEWIDX(JPNW,NEXT)
              INE = NEWIDX(JPNE,NEXT)
              ISW = NEWIDX(JPSW,NEXT)
              ISE = NEWIDX(JPSE,NEXT)
C
C             Test if any of the four neighbouring points is missing.
C
                IF( (OLDWAVE(INW).EQ.PMISS) .OR.
     X              (OLDWAVE(ISW).EQ.PMISS) .OR.
     X              (OLDWAVE(INE).EQ.PMISS) .OR.
     X              (OLDWAVE(ISE).EQ.PMISS) ) THEN
                ENDIF

              IF( (INW.EQ.0) .OR. (OLDWAVE(INW).EQ.PMISS) .OR.
     X            (ISW.EQ.0) .OR. (OLDWAVE(ISW).EQ.PMISS) .OR.
     X            (INE.EQ.0) .OR. (OLDWAVE(INE).EQ.PMISS) .OR.
     X            (ISE.EQ.0) .OR. (OLDWAVE(ISE).EQ.PMISS) ) THEN
                IF( (OLDWAVE(INW).EQ.PMISS) .AND.
     X              (OLDWAVE(ISW).EQ.PMISS) .AND.
     X              (OLDWAVE(INE).EQ.PMISS) .AND.
     X              (OLDWAVE(ISE).EQ.PMISS) ) THEN
                ENDIF
C
C               If so, take nearest grid point value.
C
                DISNW = DISTNEW(JPDISNW,NEXT)
                DISNE = DISTNEW(JPDISNE,NEXT)
                DISSW = DISTNEW(JPDISSW,NEXT)
                DISSE = DISTNEW(JPDISSE,NEXT)
C
                IF( (DISNW.LE.DISNE).AND.
     X              (DISNW.LE.DISSW).AND.
     X              (DISNW.LE.DISSE)) THEN
                  INDEX = INW
C
                ELSE IF( (DISNE.LE.DISSW).AND.
     X                   (DISNE.LE.DISSE) ) THEN
                  INDEX = INE
C
                ELSE IF( (DISSW.LE.DISSE) ) THEN
                  INDEX = ISW
C
                ELSE
                  INDEX = ISE
                ENDIF
C
                IF(INDEX.EQ.0.OR.(OLDWAVE(INDEX).EQ.PMISS)) THEN
C
C                 Nearest point is missing
C
                  NEWWAVE(NEXTWV) = PMISS
C
                ELSE
                  NEWWAVE(NEXTWV) =  OLDWAVE(INDEX)
                ENDIF
C
              ELSE
C
C               Use bi-linear interpolation from four
C               neighbouring sea points (possible conv REAL*4 to REAL*8)
C
                DI1N = DISTNEW(JPNW,NEXT)
                DI2N = DISTNEW(JPNE,NEXT)
                DI1S = DISTNEW(JPSW,NEXT)
                DI2S = DISTNEW(JPSE,NEXT)
                DK1  = DISTNEW(JPN,NEXT)
                DK2  = DISTNEW(JPS,NEXT)
C
                NW_PT = OLDWAVE(INW)
                NE_PT = OLDWAVE(INE)
                SW_PT = OLDWAVE(ISW)
                SE_PT = OLDWAVE(ISE)
                IF( .NOT. LDIREC ) THEN
                  U1 = NW_PT*DI2N + NE_PT*DI1N
                  U2 = SW_PT*DI2S + SE_PT*DI1S
                  NEWWAVE(NEXTWV) = U1*DK2 + U2*DK1
                ELSE
C
C                 Fields representing a 'direction': resolve into
C                 components and interpolate each separately.
C
#if (defined REAL_8)
#define __ATAN2 DATAN2
#define __COS   DCOS
#define __SIN   DSIN
#else
#define __ATAN2 ATAN2
#define __COS   COS
#define __SIN   SIN
#endif
                  CNW_PT = __COS(NW_PT*RAD)
                  CNE_PT = __COS(NE_PT*RAD)
                  CSW_PT = __COS(SW_PT*RAD)
                  CSE_PT = __COS(SE_PT*RAD)
                  SNW_PT = __SIN(NW_PT*RAD)
                  SNE_PT = __SIN(NE_PT*RAD)
                  SSW_PT = __SIN(SW_PT*RAD)
                  SSE_PT = __SIN(SE_PT*RAD)
                  C1 = CNW_PT*DI2N + CNE_PT*DI1N
                  C2 = CSW_PT*DI2S + CSE_PT*DI1S
                  CC = C1*DK2 + C2*DK1
                  S1 = SNW_PT*DI2N + SNE_PT*DI1N
                  S2 = SSW_PT*DI2S + SSE_PT*DI1S
                  SS = S1*DK2 + S2*DK1
                  IF( SS.LT.0.0 ) THEN
                    NEWWAVE(NEXTWV) = __ATAN2(SS,CC)/RAD + 360.0
                  ELSE
                    NEWWAVE(NEXTWV) = __ATAN2(SS,CC)/RAD
                  ENDIF
#undef __SIN
#undef __COS
#undef __ATAN2
                ENDIF
              ENDIF
            ENDIF
          ENDDO
C
        ENDIF
        ISTART = ISTART + NUMNEW(NROW)
      ENDDO
C
C     -----------------------------------------------------------------|
C*    Section 9. Closedown.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
      RETURN
      END

