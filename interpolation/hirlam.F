C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HIRLAM(L12PNT,OLDFLD,KOUNT,KGAUSS,
     X  HTYPE,AREA,POLE,GRID,NEWFLD,KSIZE,NLON,NLAT)
C
C---->
C**** HIRLAM
C
C     Purpose
C     -------
C
C     This routine creates a rotated regular lat/long field from a
C     gaussian field using 12-point horizontal interpolation.
C
C
C     Interface
C     ---------
C
C     IRET = HIRLAM(L12PNT,OLDFLD,KOUNT,KGAUSS,HTYPE,AREA,POLE,GRID,
C    X              NEWFLD,KSIZE,NLON,NLAT)
C
C
C     Input parameters
C     ----------------
C
C     L12PNT - Chooses between 12-point and 4-point interpolation
C              = .TRUE. for 12-point horizontal
C              = .FALSE. for 4-point
C     OLDFLD - Array of values from the (old) gaussian field
C     KOUNT  - Number of values in OLDFLD
C     KGAUSS - Gaussian grid number
C     HTYPE  - Gaussian grid type
C              = 'R' for reduced ("quasi-regular"),
C              = 'O' for reduced/octahedral,
C              = 'F' for full,
C              = 'U' for a user-defined gaussian grid
C     AREA   - Limits of output area (N/W/S/E)
C     POLE   - Pole of rotation (lat/long)
C     GRID   - Output lat/long grid increments (we/ns)
C     KSIZE  - The size of the output array to fill with the regular
C              lat/long field
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
C     nifld.common
C     nofld.common
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
C     JGETGG  - Reads the definition of a gaussian grid
C     HGENGRD - Calculates original lat/long (before rotation) for
C               a rotated grid
C     HNEI12  - Finds neighbours for points for interpolation
C     HWTS12  - Calculates weightings for points for interpolation
C     CHKPREC - Check if precipitation threshold has been redefined
C     FORCED_NEAREST_NEIGHBOUR - check forced interpolation method
C     HIRLAM_USERSPACE_1_GET   - HIRLAM userspace memory #1
C     HIRLAM_USERSPACE_2_GET   - HIRLAM userspace memory #2
C     HIRLAM_USERSPACE_3_GET   - HIRLAM userspace memory #3
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
C     J.D.Chambers      ECMWF      January 2001
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
#include "grfixed.h"
C
C     Parameters
C
      INTEGER JNORTH, JSOUTH, JWEST, JEAST, JW_E, JN_S
      INTEGER JP12PT, JP4PT
      PARAMETER (JP12PT = 0)
      PARAMETER (JP4PT  = 1)
      PARAMETER (JNORTH = 1)
      PARAMETER (JWEST  = 2)
      PARAMETER (JSOUTH = 3)
      PARAMETER (JEAST  = 4)
      PARAMETER (JW_E   = 1)
      PARAMETER (JN_S   = 2)
C
C     Function arguments
C
      LOGICAL L12PNT
      INTEGER KOUNT, KGAUSS, KSIZE, NLON, NLAT
      REAL AREA(4), POLE(2), GRID(2)
      REAL OLDFLD(*), NEWFLD(KSIZE)
      CHARACTER*1 HTYPE
C
C     Local variables
C
      INTEGER NEXT, LOOP, IRET, NLEN, NBYTES, NUMBER
      INTEGER NEAREST, NEND, COUNT
      INTEGER NJPR,  NJPB
      INTEGER NGSPEC_TMP
      INTEGER IOGAUSS,IOREPR
      CHARACTER*1 HTYPE_TMP
C
      INTEGER KSCHEME(1),NEIGH(12,1), KLA(1)
      REAL PWTS(12,1)
      POINTER (IPKSCHE, KSCHEME)
      POINTER (IPNEIGH, NEIGH)
      POINTER (IPKLA,   KLA)
      POINTER (IPPWTS,  PWTS)
      LOGICAL LVEGGY
C
      REAL PDLO0(1),PDLO1(1),PDLO2(1),PDLO3(1),PDLAT(1)
      POINTER (IPPDLO0, PDLO0)
      POINTER (IPPDLO1, PDLO1)
      POINTER (IPPDLO2, PDLO2)
      POINTER (IPPDLO3, PDLO3)
      POINTER (IPPDLAT, PDLAT)
C
      INTEGER IGG
      INTEGER KPTS(1)
      REAL GLATS(1)
      INTEGER IOFFS(1)
      POINTER (IPKPTS,  KPTS)
      POINTER (IPIOFFS, IOFFS)
      POINTER (IPGLATS, GLATS)
C
      INTEGER ILL
      REAL RLAT(1),RLON(1)
      POINTER (IPRLAT, RLAT)
      POINTER (IPRLON, RLON)
C
      REAL OLD(KOUNT)
C
      CHARACTER*7 FNAME
      PARAMETER  (FNAME = 'HIRLAM ')
C
      DATA NUMBER/-1/,IOGAUSS/-1/,IOREPR/-1/
C
      SAVE IPKSCHE, IPNEIGH, IPKLA, IPPWTS
      SAVE IPPDLO0, IPPDLO1, IPPDLO2, IPPDLO3, IPPDLAT
      SAVE IPKPTS, IPIOFFS, IPGLATS
      SAVE IPRLAT, IPRLON
      SAVE NUMBER, IOGAUSS, IOREPR
C
C     Externals
C
      LOGICAL FORCED_NEAREST_NEIGHBOUR
      LOGICAL ISOCTAHEDRAL
      INTEGER HNEI12
      INTEGER HGENGRD
#ifdef POINTER_64
      INTEGER*8
#else
      INTEGER
#endif
     X  HIRLAM_USERSPACE_1_GET,
     X  HIRLAM_USERSPACE_2_GET,
     X  HIRLAM_USERSPACE_3_GET
C
C     Statement functions
C
      REAL A, B
      LOGICAL NOTEQ
C
C     Tests that A is not (almost) equal to B
C
      NOTEQ(A,B) = (ABS((A)-(B)).GT.(ABS(A)*1E-3))
C
C     -----------------------------------------------------------------|
C     Section 1.  Initialise.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      HIRLAM = 0
C
      CALL JDEBUG()
C
      IF( L12PNT ) THEN
        CALL INTLOG(JP_DEBUG,FNAME//': 12-pt interpolation',JPQUIET)
      ELSE
        CALL INTLOG(JP_DEBUG,FNAME//': 4-pt interpolation',JPQUIET)
      ENDIF
C
      CALL CHKPREC()
      IF( LPREC )THEN
        CALL INTLOG(JP_DEBUG,
     X    FNAME//': precipitation threshold applied',JPQUIET)
      ELSE
        CALL INTLOG(JP_DEBUG,
     X    FNAME//': precipitation threshold not applied',JPQUIET)
      ENDIF

      IF( HTYPE.NE.'O' .AND. HTYPE.NE.'R' .AND. HTYPE.NE.'U'
     X .AND. HTYPE.NE.'F' ) THEN
        CALL INTLOG(JP_ERROR,
     X    FNAME//': Gaussian type unsupported ('//HTYPE//')',JPQUIET)
        HIRLAM = 1
        GOTO 900
      ENDIF

C     Set interpolation method (use nearest neighbour if required)
      LVEGGY = FORCED_NEAREST_NEIGHBOUR(LMETHOD,NITABLE,NIPARAM)
      IF( LVEGGY ) CALL INTLOG(JP_DEBUG,
     X  FNAME//': nearest neighbour processing',JPQUIET)


C
C     Dynamically allocate memory for gaussian grid information.
C
      IGG = KGAUSS*2
      NBYTES = (IGG*JPRLEN) + (2*IGG+1)*JPBYTES
C
      IPKPTS = HIRLAM_USERSPACE_1_GET(NBYTES)
#ifdef hpR64
      IPKPTS = IPKPTS/(1024*1024*1024*4)
#endif
      IF( IPKPTS.EQ.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    FNAME//': Memory allocate fail',JPQUIET)
        HIRLAM = 1
        GOTO 900
      ENDIF
C
      IPGLATS = IPKPTS  + (IGG*JPBYTES)
      IPIOFFS = IPGLATS + (IGG*JPRLEN)
C
      IOGAUSS = -1
      IOREPR  = -1
C
C     Build up offsets to start of each latitude in original field(s)
C
      IF( (KGAUSS.NE.IOGAUSS).OR.(IOREPR.NE.NIREPR) ) THEN
        CALL INTLOG(JP_DEBUG,FNAME//': trying Gaussian '//HTYPE,KGAUSS)
        CALL JGETGG(KGAUSS,HTYPE,GLATS,KPTS,IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      FNAME//': JGETGG failed to get gaussian data',JPQUIET)
          HIRLAM = 2
          GOTO 900
        ENDIF
C
        IOFFS(1) = 1
        DO LOOP = 2, (KGAUSS*2+1)
          IOFFS(LOOP) = IOFFS(LOOP-1) + KPTS(LOOP-1)
        ENDDO
C
C
C       Check whether number of gaussian points agrees with grid specification.
C       If not and grid is N80/N160 non-octahedral, try the 12-pt specification.
C
        NUMBER = (IOFFS(KGAUSS*2+1) - 1)
        IF( NUMBER.NE.KOUNT ) THEN
          IF( (KGAUSS.EQ.80).OR.(KGAUSS.EQ.160) .AND.
     X        (NIREPR.EQ.JPQUASI) .AND.
     X        (.NOT. ISOCTAHEDRAL(NIGAUSS,MILLEN)) ) THEN
            NGSPEC_TMP = NGSPEC
            NGSPEC = 12
            HTYPE_TMP = 'F'
            IF( NIREPR.EQ.JPQUASI ) HTYPE_TMP = 'R'
            CALL INTLOG(JP_DEBUG,
     X        FNAME//': trying 12-pt Gaussian '//HTYPE_TMP,KGAUSS)
            CALL JGETGG(KGAUSS,HTYPE_TMP,GLATS,KPTS,IRET)
            NGSPEC = NGSPEC_TMP
C
            IF( IRET.NE.0 ) THEN
              CALL INTLOG(JP_ERROR,
     X          FNAME//': JGETGG failed to get gaussian data',JPQUIET)
              HIRLAM = 2
              GOTO 900
            ENDIF
C
            IOFFS(1) = 1
            DO LOOP = 2, (KGAUSS*2+1)
              IOFFS(LOOP) = IOFFS(LOOP-1) + KPTS(LOOP-1)
            ENDDO
C
            NUMBER = (IOFFS(KGAUSS*2+1) - 1)
            IF( NUMBER.NE.KOUNT ) THEN
           CALL INTLOG(JP_ERROR,FNAME//': Given no. of points =',KOUNT)
           CALL INTLOG(JP_ERROR,FNAME//': Expected no. of pts =',NUMBER)
              HIRLAM = 4
              GOTO 900
            ENDIF
C
          ELSE
C
           CALL INTLOG(JP_ERROR,FNAME//': Given no. of points =',KOUNT)
           CALL INTLOG(JP_ERROR,FNAME//': Expected no. of pts =',NUMBER)
            HIRLAM = 4
            GOTO 900
          ENDIF
        ENDIF
C
        IOGAUSS = KGAUSS
        IOREPR  = NIREPR
      ENDIF
C
C     Preserve the input field
C     (in case OLDFLD and NEWFLD are the same arrays)
C
      NUMBER = (IOFFS(KGAUSS*2+1) - 1)
      OLD(1:NUMBER) = OLDFLD(1:NUMBER)
C
C     -----------------------------------------------------------------|
C     Section 2.  Generate the lat/long points for the output grid
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C (DJ Mar-05)
      NLON = 1 + NINT((AREA(JEAST) - AREA(JWEST)) / GRID(JW_E))
      NLAT = 1 + NINT((AREA(JNORTH) - AREA(JSOUTH)) / GRID(JN_S))
C
      NLEN = NLON * NLAT

      NOWE = NLON
      NONS = NLAT
C
C     Check that given array is big enough for the new field.
C
      IF( NLEN.GT.KSIZE ) THEN
        CALL INTLOG(JP_ERROR,FNAME//': Given array size = ',KSIZE)
        CALL INTLOG(JP_ERROR,FNAME//': Required size = = ',NLEN)
        HIRLAM = 5
        GOTO 900
      ENDIF
C
C     Dynamically allocate memory for lat/long arrays.
C
      ILL = NLEN
      NBYTES = 2*ILL*JPRLEN
C
      IPRLON = HIRLAM_USERSPACE_2_GET(NBYTES)
#ifdef hpR64
      IPRLON = IPRLON/(1024*1024*1024*4)
#endif
      IF( IPRLON.EQ.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    FNAME//': Memory allocate fail',JPQUIET)
        HIRLAM = 6
        GOTO 900
      ENDIF
C
      IPRLAT  = IPRLON + (ILL*JPRLEN)
C
      IRET = HGENGRD(AREA,POLE,GRID,NLON,NLAT,RLAT,RLON)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    FNAME//': HGENGRD failed to get lat/lon grid data',JPQUIET)
        HIRLAM = 7
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
      NJPR=17
      NJPB=14
      NBYTES = (NJPR*JPRLEN + NJPB*JPBYTES) * ILL
C
      IPPDLO0 = HIRLAM_USERSPACE_3_GET(NBYTES)
#ifdef hpR64
      IPPDLO0 = IPPDLO0/(1024*1024*1024*4)
#endif
      IF( IPPDLO0.EQ.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    FNAME//': Memory allocate fail',JPQUIET)
        HIRLAM = 8
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
C     Find neighbours.
C
      IRET = HNEI12(L12PNT,NLEN,RLAT,RLON,KGAUSS,KPTS,GLATS,
     X              KSCHEME,PDLAT,PDLO0,PDLO1,PDLO2,PDLO3,KLA,NEIGH)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    FNAME//': HNEI12 failed to find neighbours',JPQUIET)
        HIRLAM = 9
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
      CALL HWTS12
     X  (NLEN,KSCHEME,KLA,PDLAT,GLATS,PDLO0,PDLO1,PDLO2,PDLO3,NEIGH,
     X   PWTS)
C
C     Calculate the interpolated grid point values
C
      DO LOOP = 1, NLEN
        IF( KSCHEME(LOOP).EQ.JP12PT ) THEN
C
C     See if any of the neighbours are missing
C
          COUNT = 0
          IF( NOTEQ(OLD(NEIGH( 1,LOOP)),RMISSGV) ) COUNT = COUNT + 1
          IF( NOTEQ(OLD(NEIGH( 2,LOOP)),RMISSGV) ) COUNT = COUNT + 1
          IF( NOTEQ(OLD(NEIGH( 3,LOOP)),RMISSGV) ) COUNT = COUNT + 1
          IF( NOTEQ(OLD(NEIGH( 4,LOOP)),RMISSGV) ) COUNT = COUNT + 1
          IF( NOTEQ(OLD(NEIGH( 5,LOOP)),RMISSGV) ) COUNT = COUNT + 1
          IF( NOTEQ(OLD(NEIGH( 6,LOOP)),RMISSGV) ) COUNT = COUNT + 1
          IF( NOTEQ(OLD(NEIGH( 7,LOOP)),RMISSGV) ) COUNT = COUNT + 1
          IF( NOTEQ(OLD(NEIGH( 8,LOOP)),RMISSGV) ) COUNT = COUNT + 1
          IF( NOTEQ(OLD(NEIGH( 9,LOOP)),RMISSGV) ) COUNT = COUNT + 1
          IF( NOTEQ(OLD(NEIGH(10,LOOP)),RMISSGV) ) COUNT = COUNT + 1
          IF( NOTEQ(OLD(NEIGH(11,LOOP)),RMISSGV) ) COUNT = COUNT + 1
          IF( NOTEQ(OLD(NEIGH(12,LOOP)),RMISSGV) ) COUNT = COUNT + 1
C
C         Interpolate using twelve neighbours if none are missing
C
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
          IF( COUNT.EQ.12) THEN
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
C         Set missing if all neighbours are missing
C
          ELSE IF( COUNT.EQ.0 ) THEN
            NEWFLD(LOOP) = RMISSGV
C
C         Otherwise, use the nearest neighbour
C
          ELSE
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
          ENDIF
        ENDIF
C
        ELSE IF( KSCHEME(LOOP).EQ.JP4PT ) THEN
C
C     See if any of the neighbours are missing
C
          COUNT = 0
          IF( NOTEQ(OLD(NEIGH( 1,LOOP)),RMISSGV) ) COUNT = COUNT + 1
          IF( NOTEQ(OLD(NEIGH( 2,LOOP)),RMISSGV) ) COUNT = COUNT + 1
          IF( NOTEQ(OLD(NEIGH( 3,LOOP)),RMISSGV) ) COUNT = COUNT + 1
          IF( NOTEQ(OLD(NEIGH( 4,LOOP)),RMISSGV) ) COUNT = COUNT + 1
C
C         Interpolate using four neighbours if none are missing
C
c          IF( COUNT.EQ.4.AND.LVEGGY.EQV.(.FALSE.) ) THEN
        IF( LVEGGY) THEN

            NEAREST = 1
            IF( PWTS( 2,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 2
            IF( PWTS( 3,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 3
            IF( PWTS( 4,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 4
            NEWFLD(LOOP) = OLD(NEIGH( NEAREST,LOOP))

        ELSE
          IF( COUNT.EQ.4) THEN
            NEWFLD(LOOP) =
     X        OLD(NEIGH( 1,LOOP)) * PWTS( 1,LOOP) +
     X        OLD(NEIGH( 2,LOOP)) * PWTS( 2,LOOP) +
     X        OLD(NEIGH( 3,LOOP)) * PWTS( 3,LOOP) +
     X        OLD(NEIGH( 4,LOOP)) * PWTS( 4,LOOP)
C
C         Set missing if all neighbours are missing
C
          ELSE IF( COUNT.EQ.0 ) THEN
            NEWFLD(LOOP) = RMISSGV
C
C         Otherwise, use the nearest neighbour
C
          ELSE
            NEAREST = 1
            IF( PWTS( 2,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 2
            IF( PWTS( 3,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 3
            IF( PWTS( 4,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 4
            NEWFLD(LOOP) = OLD(NEIGH( NEAREST,LOOP))
          ENDIF
       ENDIF
C
        ELSE
          DO NEXT = 1, 4
            IF( NEIGH(NEXT,LOOP).NE.0 )
     X        NEWFLD(LOOP) = OLD(NEIGH(NEXT,LOOP))
          ENDDO
C
        ENDIF
C
C       Remove precipitation if less than a 'trace' or if nearest
C       neighbour is less than a trace
C
        IF( LPREC ) THEN
          IF( NEWFLD(LOOP).LT.ZPRECIP ) THEN
            NEWFLD(LOOP) = 0.0
          ELSE
            NEAREST = 1
            NEND = 12
            IF( KSCHEME(LOOP).NE.JP12PT ) NEND = 4
            DO NEXT = 2, NEND
              IF( PWTS( NEXT,LOOP).GT.PWTS( NEAREST,LOOP) )
     X          NEAREST = NEXT
            ENDDO
            IF( OLD(NEIGH(NEAREST,LOOP)).LT.ZPRECIP ) NEWFLD(LOOP) = 0.0
          ENDIF
        ENDIF
C
      ENDDO
C
C     -----------------------------------------------------------------|
C     Section 9.  Return.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
      RETURN
      END

