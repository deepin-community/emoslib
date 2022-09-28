C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HLL2LL(L12PNT,OLDFLD,OLDGRID,AREA,POLE,GRID,
     X                        NEWFLD,KSIZE,NLON,NLAT)
C
C---->
C**** HLL2LL
C
C     Purpose
C     -------
C
C     This routine creates a rotated regular lat/long field from a
C     global regular lat/long field using 12-point horizontal
C     interpolation.
C
C
C     Interface
C     ---------
C
C     IRET = HLL2LL(L12PNT,OLDFLD,OLDGRID,AREA,POLE,GRID,NEWFLD,KSIZE,
C    X              NLON,NLAT)
C
C
C     Input parameters
C     ----------------
C
C     L12PNT  - Chooses between 12-point and 4-point interpolation
C     OLDFLD  - The array of values from the regular lat/long field
C     OLDGRID - Grid increments (i/j) for the global lat/long field
C     AREA    - Limits of area (N/W/S/E) for the new lat/long field
C     POLE    - Pole of rotation (lat/long) for the new lat/long field
C     GRID    - Grid increments (i/j) for the new lat/long field
C     KSIZE   - The size of the array to fill with the new
C               lat/long field
C
C
C     Output parameters
C     -----------------
C
C     NEWFLD - The array of values for the regular lat/long field
C     NLON   - Number of longitudes in the regular lat/long field
C     NLAT   - Number of latitudes in the regular lat/long field
C
C     Returns 0 if function successful, non-zero otherwise.
C
C     Common block usage
C     ------------------
C
C
C
C     Method
C     ------
C
C     Numbering of the points (I is the interpolation point):
C
C                   13       5       6      14
C
C                    7       1       2       8
C                               (I)
C                    9       3       4      10
C
C                   15      11      12      16
C
C     The 12-point interpolation is not possible if either of the top
C     two rows is above the original field northern latitude. The
C     nearest neighbour is used if both rows are above, and a 4-pt
C     bilinear interpolation is used if the top row is above.
C     Similarily, if either of the bottom two rows is below the original
C     field southern latitude.
C
C
C     Externals
C     ---------
C
C     INTLOG  - Logs messages
C     JMALLOC - Dynamically allocate memory
C     JFREE   - Free dynamically allocated memory
C     HGENLL  - Calculates original lat/long (before rotation) for
C               a rotated grid
C     HNEILL  - Finds neighbours for points for interpolation
C     HWTSLL  - Calculates weightings for points for interpolation
C     FORCED_NEAREST_NEIGHBOUR - check forced interpolation method
C
C
C     Reference
C     ---------
C
C     None.
C
C
C     Comments
C     --------
C
C     None.
C
C
C     Author
C     ------
C
C     J.D.Chambers      ECMWF      November 2001
C
C
C     Modifications
C     -------------
C
C     None.
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
C
C     Parameters
C
      INTEGER JNORTH, JSOUTH, JWEST, JEAST, JW_E, JN_S, JLAT, JLON
      INTEGER JP12PT, JP4PT, JPNEARN
      PARAMETER (JP12PT  = 0)
      PARAMETER (JP4PT   = 1)
      PARAMETER (JPNEARN = 2)
      PARAMETER (JNORTH = 1 )
      PARAMETER (JWEST  = 2 )
      PARAMETER (JSOUTH = 3 )
      PARAMETER (JEAST  = 4 )
      PARAMETER (JW_E  = 1 )
      PARAMETER (JN_S  = 2 )
      PARAMETER (JLAT  = 1 )
      PARAMETER (JLON  = 2 )
C
C     Function arguments
C
      LOGICAL L12PNT
      INTEGER KSIZE, NLON, NLAT
      REAL OLDGRID(2),AREA(4),POLE(2),GRID(2),OLDFLD(*),NEWFLD(KSIZE)
C
C     Local variables
C
      INTEGER NEXT, LOOP, IRET, NLEN, NPREV, NBYTES, NUMBER
      INTEGER NOLDLAT, NOLDLON, NEAREST
C
      LOGICAL LNEW, LFIRST, LVEGGY
      INTEGER KSCHEME(1),NEIGH(12,1), KLA(1)
      REAL PWTS(12,1)
      POINTER (IPKSCHE, KSCHEME)
      POINTER (IPNEIGH, NEIGH)
      POINTER (IPKLA,   KLA)
      POINTER (IPPWTS,  PWTS)
C
      REAL PDLO0(1),PDLO1(1),PDLO2(1),PDLO3(1),PDLAT(1)
      POINTER (IPPDLO0, PDLO0)
      POINTER (IPPDLO1, PDLO1)
      POINTER (IPPDLO2, PDLO2)
      POINTER (IPPDLO3, PDLO3)
      POINTER (IPPDLAT, PDLAT)
C
      REAL PREGRID(2)
      INTEGER KPTS(1)
      REAL GLATS(1)
      INTEGER IOFFS(1)
      POINTER (IPKPTS,  KPTS)
      POINTER (IPIOFFS, IOFFS)
      POINTER (IPGLATS, GLATS)
C
      INTEGER ILL, ILLOLD
      REAL RLAT(1),RLON(1)
      POINTER (IPRLAT, RLAT)
      POINTER (IPRLON, RLON)
C
      REAL OLD(1)
      POINTER (IOLD,   OLD)
C
      DATA NPREV/-1/
      DATA LNEW/.FALSE./, LFIRST/.TRUE./
      DATA ILLOLD/-1/, IOLD/-1/
      DATA PREGRID/2*0.0/
C
      SAVE LNEW, LFIRST
      SAVE IPKSCHE, IPNEIGH, IPKLA, IPPWTS
      SAVE IPPDLO0, IPPDLO1, IPPDLO2, IPPDLO3, IPPDLAT
      SAVE NPREV, IPKPTS, IPIOFFS, IPGLATS
      SAVE ILLOLD, IPRLAT, IPRLON, IOLD
      SAVE PREGRID
C
C     Externals
C
      LOGICAL FORCED_NEAREST_NEIGHBOUR
      INTEGER HNEILL, HGENLL
#ifdef POINTER_64
      INTEGER*8 JMALLOC
#else
      INTEGER JMALLOC
#endif
C
C     -----------------------------------------------------------------|
C     Section 1.  Initialise.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      HLL2LL = 0
C
      CALL JDEBUG()

      IF( L12PNT ) THEN
        CALL INTLOG(JP_DEBUG,'HLL2LL: 12-pt interpolation',JPQUIET)
      ELSE
        CALL INTLOG(JP_DEBUG,'HLL2LL:  4-pt interpolation',JPQUIET)
      ENDIF
C
      CALL CHKPREC()
      IF( LPREC )THEN
        CALL INTLOG(JP_DEBUG,
     X   'HLL2LL: precipitation threshold applied',JPQUIET)
      ELSE
        CALL INTLOG(JP_DEBUG,
     X   'HLL2LL: precipitation threshold not applied',JPQUIET)
      ENDIF

C     Use nearest neighbour if required
      LVEGGY = FORCED_NEAREST_NEIGHBOUR(LMETHOD,NITABLE,NIPARAM)
      IF( LVEGGY ) CALL INTLOG(JP_DEBUG,
     X  'HLL2LL: nearest neighbour processing',JPQUIET)

      NOLDLAT = 1 + NINT(180.0/OLDGRID(1))
      NOLDLON = NINT(360.0/OLDGRID(2))
      NUMBER  = NOLDLAT * NOLDLON
C
      IF( (OLDGRID(1).NE.PREGRID(1)).OR.
     X    (OLDGRID(2).NE.PREGRID(2)) ) THEN
C
C       Allocate memory to hold the input field
C       (in case OLDFLD and NEWFLD are the same arrays)
C
        IF( IOLD.GT.0 ) CALL JFREE(IOLD)
C
        NBYTES = NUMBER * JPRLEN
C
        IOLD = JMALLOC(NBYTES)
#ifdef hpR64
        IOLD = IOLD/(1024*1024*1024*4)
#endif
        IF( IOLD.EQ.0 ) THEN
          CALL INTLOG(JP_ERROR,'HLL2LL: Memory allocation fail',JPQUIET)
          HLL2LL = 3
          GOTO 900
        ENDIF
C
        PREGRID(1) = OLDGRID(1)
        PREGRID(2) = OLDGRID(2)
C
      ENDIF
C
C     Preserve the input field
C
      DO LOOP = 1, NUMBER
        OLD(LOOP) = OLDFLD(LOOP)
      ENDDO
C
C     -----------------------------------------------------------------|
C     Section 2.  Generate the lat/long points for the output grid
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      NLON = 1 + NINT((AREA(JEAST) - AREA(JWEST)) / GRID(JW_E)) ! SC
      NLAT = 1 + NINT((AREA(JNORTH) - AREA(JSOUTH)) / GRID(JN_S)) ! SC
C
      NLEN = NLON * NLAT

      NOWE = NLON
      NONS = NLAT
C
C     Check that given array is big enough for the new field.
C
      IF( NLEN.GT.KSIZE ) THEN
        CALL INTLOG(JP_ERROR,'HLL2LL: Given array size = ',KSIZE)
        CALL INTLOG(JP_ERROR,'HLL2LL: Required size = ',NLEN)
        HLL2LL = 4
        GOTO 900
      ENDIF
C
C     Dynamically allocate memory for lat/long arrays.
C
      ILL = NLEN
      IF( ILL.GT.ILLOLD ) THEN
C
        LNEW = .TRUE.
C
        IF( ILLOLD.GT.0 ) CALL JFREE(IPRLON)
C
        NBYTES = 2*ILL*JPRLEN
C
        IPRLON = JMALLOC(NBYTES)
#ifdef hpR64
        IPRLON = IPRLON/(1024*1024*1024*4)
#endif
        IF( IPRLON.EQ.0 ) THEN
          CALL INTLOG(JP_ERROR,'HLL2LL: Memory allocation fail',JPQUIET)
          HLL2LL = 5
          GOTO 900
        ENDIF
C
        IPRLAT = IPRLON + (ILL*JPRLEN)
C
        ILLOLD = ILL
C
      ENDIF
C
      IRET = HGENLL(AREA,POLE,GRID,NLON,NLAT,RLAT,RLON)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'HLL2LL: HGENLL failed to get lat/lon grid data',JPQUIET)
        HLL2LL = 6
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C     Section 3.  Find neighbours for each point for interpolation.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
C     Dynamically allocate memory for interpolation arrays.
C
      IF( LNEW ) THEN
C
        IF( .NOT.LFIRST ) CALL JFREE(IPPDLO0)
C
        NBYTES = (17*JPRLEN + 14*JPBYTES) * ILL
C
        IPPDLO0 = JMALLOC(NBYTES)
#ifdef hpR64
        IPPDLO0 = IPPDLO0/(1024*1024*1024*4)
#endif
        IF( IPPDLO0.EQ.0 ) THEN
          CALL INTLOG(JP_ERROR,'HLL2LL: Memory allocation fail',JPQUIET)
          HLL2LL = 7
          GOTO 900
        ENDIF
C
        IPPDLO1 = IPPDLO0 + (ILL*JPRLEN)
        IPPDLO2 = IPPDLO1 + (ILL*JPRLEN)
        IPPDLO3 = IPPDLO2 + (ILL*JPRLEN)
        IPPDLAT = IPPDLO3 + (ILL*JPRLEN)
        IPPWTS  = IPPDLAT + (ILL*JPRLEN)
        IPKSCHE = IPPWTS  + (12*ILL*JPRLEN)
        IPKLA   = IPKSCHE + (ILL*JPBYTES)
        IPNEIGH = IPKLA   + (ILL*JPBYTES)
C
        LFIRST = .FALSE.
        LNEW   = .FALSE.
C
      ENDIF
C
C     Find neighbours.
C
      IRET = HNEILL(L12PNT,NLEN,RLAT,RLON,OLDGRID,
     X              KSCHEME,PDLAT,PDLO0,PDLO1,PDLO2,PDLO3,KLA,NEIGH)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'HLL2LL: HNEILL failed to find neighbours',JPQUIET)
        HLL2LL = 8
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C     Section 4.  Perform the 12-point horizontal interpolation.
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
C     Setup the 12-point horizontal interpolation weights
C
      CALL HWTSLL
     X  (NLEN,KSCHEME,KLA,PDLAT,oldgrid(2),pdlo0,PDLO1,PDLO2,PDLO3,
     X   NEIGH,PWTS)
C
C     Calculate the interpolated grid point values
C
      DO LOOP = 1, NLEN
        IF( LVEGGY) THEN
            NEAREST = 1
            IF( PWTS( 2,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 2
            IF( PWTS( 3,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 3
            IF( PWTS( 4,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 4
            IF( PWTS( 5,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 5
            IF( PWTS( 6,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 6
            IF( PWTS( 7,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 7
            IF( PWTS( 8,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 8
            IF( PWTS( 9,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 9
            IF( PWTS(10,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST =10
            IF( PWTS(11,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST =11
            IF( PWTS(12,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST =12
            NEWFLD(LOOP) = OLD(NEIGH( NEAREST,LOOP))
        ELSE
          IF( KSCHEME(LOOP).EQ.JP12PT ) THEN
            NEWFLD(LOOP) =
     X        OLD(NEIGH( 1,LOOP)) * PWTS( 1,LOOP) +
     X        OLD(NEIGH( 2,LOOP)) * PWTS( 2,LOOP) +
     X        OLD(NEIGH( 3,LOOP)) * PWTS( 3,LOOP) +
     X        OLD(NEIGH( 4,LOOP)) * PWTS( 4,LOOP) +
     X        OLD(NEIGH( 5,LOOP)) * PWTS( 5,LOOP) +
     X        OLD(NEIGH( 6,LOOP)) * PWTS( 6,LOOP) +
     X        OLD(NEIGH( 7,LOOP)) * PWTS( 7,LOOP) +
     X        OLD(NEIGH( 8,LOOP)) * PWTS( 8,LOOP) +
     X        OLD(NEIGH( 9,LOOP)) * PWTS( 9,LOOP) +
     X        OLD(NEIGH(10,LOOP)) * PWTS(10,LOOP) +
     X        OLD(NEIGH(11,LOOP)) * PWTS(11,LOOP) +
     X        OLD(NEIGH(12,LOOP)) * PWTS(12,LOOP)
C
          ELSE IF( KSCHEME(LOOP).EQ.JP4PT ) THEN
            NEWFLD(LOOP) =
     X        OLD(NEIGH( 1,LOOP)) * PWTS( 1,LOOP) +
     X        OLD(NEIGH( 2,LOOP)) * PWTS( 2,LOOP) +
     X        OLD(NEIGH( 3,LOOP)) * PWTS( 3,LOOP) +
     X        OLD(NEIGH( 4,LOOP)) * PWTS( 4,LOOP)
C
          ELSE
            DO NEXT = 1, 4
              IF( NEIGH(NEXT,LOOP).NE.0 )
     X          NEWFLD(LOOP) = OLD(NEIGH(NEXT,LOOP))
            ENDDO
C
          ENDIF
        ENDIF
      ENDDO
C
C     -----------------------------------------------------------------|
C     Section 9.  Return.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END

