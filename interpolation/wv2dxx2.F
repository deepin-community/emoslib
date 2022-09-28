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

      INTEGER FUNCTION WV2DXX2(KNSPEC, NUMLATS, NPTS,
     X                         NLATS, STEPNS, STEPWE,
     X                         OLDWAVE, NEWWAVE, NORTH, WEST, PMISS)
C
C---->
C*****WV2DXX2*
C
C     PURPOSE
C     -------
C
C     Interpolates 2D wave spectra fields.
C
C
C     INTERFACE
C     ---------
C
C     IRET = WV2DXX2(KNSPEC, NUMLATS, NPTS,
C    X               NLATS, STEPNS, STEPWE,
C    X               OLDWAVE, NEWWAVE, NORTH, WEST, PMISS)
C
C     Input arguments
C     ---------------
C
C     KNSPEC  - Number of 2D spectra values at each wave point
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
C     boundaries and looking up neighbouring point 2D spectra values
C     (which may be missing data).
C
C
C     EXTERNALS
C     ---------
C
C     WV2DIDX - builds the index of nearest neighbour points
C     NUMPTWE - Calculates number of grid points between west/east
C               area boundaries
C     NUMVALS - Counts the values in the secondary bitmap.
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
C     Handle parameter 251 as a simple form of wave 2D spectra.
C     (A field containing one direction and one frequency).
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
      INTEGER JPROUTINE, JPMXLAT, JPNSPEC
      PARAMETER( JPROUTINE = 40000 )
      PARAMETER( JPMXLAT   =  1801 ) ! allow up to 0.1 degree resolution
      PARAMETER( JPNSPEC   =   600 )
C
#include "nifld.common"
#include "nofld.common"
#include "grspace.h"
C
C     Arguments
C
      INTEGER KNSPEC
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
     X        SOUTH, OLDEAST, OLDWEST
      INTEGER IRET
C
C     Large, resolution-dependent arrays
C
      LOGICAL IS_ALLOCATED_LARGEARRAY
      DATA    IS_ALLOCATED_LARGEARRAY /.FALSE./
      SAVE    IS_ALLOCATED_LARGEARRAY
C
      INTEGER NEWIDX(JPARRAYDIM_WAVE)
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 P_NEWIDX
#endif
#endif
      POINTER(P_NEWIDX,NEWIDX)
      SAVE P_NEWIDX
C
C     Externals
C
      INTEGER  WV2DIDX, NUMVALS, NUMPTWE
#ifdef POINTER_64
      INTEGER*8 JMALLOC
#else
      INTEGER JMALLOC
#endif
      EXTERNAL WV2DIDX, NUMVALS, NUMPTWE, JMALLOC
C
C     -----------------------------------------------------------------|
C*    Section 1. Initalisation.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      WV2DXX2 = 0
C
C     Initialise the bitmap value lookup function and function
C     which counts the values in the secondary bitmap.
C
      IRET = NUMVALS(0,0,0)
C
C     Check only new-style 2D wave spectra (parameter 251)
C
      IF( KNSPEC.GT.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'WV2DXX2: Only single-value 2D spectra field allowed',JPQUIET)
        CALL INTLOG(JP_ERROR,
     X    'WV2XX2: Value given = ',KNSPEC)
        WV2DXX2 = JPROUTINE + 1
        GOTO 900
      ENDIF
C
C     Calculate number of latitudes if grid had been full from
C     North pole to South pole
C
      IF( NUMLATS.GT.JPMXLAT ) THEN
        CALL INTLOG(JP_ERROR,
     X    'WV2DXX2: Number of latitudes in input grid = ',NUMLATS)
        CALL INTLOG(JP_ERROR,
     X    'WV2DXX2: And is greater than allowed maximum = ',JPMXLAT)
        WV2DXX2 = JPROUTINE + 2
        GOTO 900
      ENDIF

      ONORTH = FLOAT(NIAREA(1))/PPMULT
      OSOUTH = FLOAT(NIAREA(3))/PPMULT

      RLATINC = FLOAT(NIGRID(2))/PPMULT

      DO LOOP = 1, NUMLATS
        OLDLATS(LOOP) = 90.0 - (LOOP-1)*RLATINC
      ENDDO
C
C     Fill an array with the number of points at each latitude for the
C     input field (this is a reduced latitude/longitude grid)
C
      MISSLAT = (90.0 - ONORTH)/RLATINC
      KOLDNUM = 1 + (90.0 - OSOUTH)/RLATINC
C
      IF( NIREPR.NE.JPREDLL ) THEN
        DO LOOP = 1, MISSLAT
          NPTS(LOOP)    = 0
        ENDDO
        DO LOOP = 1, (KOLDNUM-MISSLAT)
          NPTS(LOOP+MISSLAT) = NIWE
        ENDDO
        DO LOOP = (KOLDNUM+1), NUMLATS
          NPTS(LOOP)    = 0
        ENDDO
      ENDIF
C
C     Allocate large, resolution-dependent arrays
      IF (.NOT. IS_ALLOCATED_LARGEARRAY) THEN
        P_NEWIDX = JMALLOC(JPARRAYDIM_WAVE*JPBYTES)  ! len*bytes
        IF( P_NEWIDX.EQ.0 ) THEN
          CALL INTLOG(JP_ERROR,'WV2DXX2: JMALLOC fail',JPQUIET)
          WV2DXX2 = JPROUTINE + 1
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
      OLDWEST = FLOAT(NIAREA(2))/PPMULT
      OLDEAST = FLOAT(NIAREA(4))/PPMULT

      WV2DXX2 = WV2DIDX(NUMLATS,NPTS,OLDLATS,OLDWEST,OLDEAST,
     X                  KNEWNUM, NUMNEW, NEWLATS, NEWIDX)
      IF( WV2DXX2.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'WV2DXX2: Unable to calculate output grid indices',JPQUIET)
        WV2DXX2 = JPROUTINE + 3
        GOTO 900
      ENDIF
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
          IF( (WEST.GT.0.0).AND.(WEST.NE.(IWEST*ROWINC)) ) THEN
            IWEST = IWEST + 1
          ELSEIF( (WEST.LT.0.0).AND.(WEST.NE.(IWEST*ROWINC)) ) THEN
            IWEST = IWEST - 1
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
              INDEX = NEWIDX(NEXT)
              IF( OLDWAVE(INDEX).EQ.PMISS.OR.INDEX.EQ.0) THEN
C
C                 Nearest point is missing
C
                  NEWWAVE(NEXTWV) = PMISS
C
              ELSE
                  NEWWAVE(NEXTWV) = OLDWAVE(INDEX)
              ENDIF
C
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

