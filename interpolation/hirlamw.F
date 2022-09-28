C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HIRLAMW(L12PNT,OLDFLDU,OLDFLDV,KOUNT,KGAUSS,
     X  HTYPE,AREA,POLE,GRID,NEWFLDU,NEWFLDV,KSIZE,NLON,NLAT)
C
C---->
C**** HIRLAMW
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
C     IRET = HIRLAMW(L12PNT,OLDFLDU,OLDFLDV,KOUNT,KGAUSS,HTYPE,AREA,
C    X               POLE,GRID,NEWFLDU,NEWFLDV,KSIZE,NLON,NLAT)
C
C
C     Input parameters
C     ----------------
C
C     L12PNT  - Chooses between 12-point and 4-point interpolation
C               = .TRUE. for 12-point horizontal
C               = .FALSE. for 4-point
C     OLDFLDU - The array of values from the gaussian U field
C     OLDFLDV - The array of values from the gaussian V field
C     KOUNT   - Number of values in OLDFLDU/V
C     KGAUSS  - Gaussian number for the gaussian field
C     HTYPE   - Gaussian grid type
C               = 'R' for reduced ("quasi-regular"),
C               = 'O' for reduced/octahedral,
C               = 'F' for full,
C               = 'U' for a user-defined gaussian grid
C     AREA    - Limits of output area (N/W/S/E)
C     POLE    - Pole of rotation (lat/long)
C     GRID    - Output lat/long grid increments (we/ns)
C     KSIZE   - The size of the output array to fill with the regular
C               lat/long field
C
C
C     Output parameters
C     -----------------
C
C     NEWFLDU - The array of values for the regular lat/long U field
C     NEWFLDV - The array of values for the regular lat/long V field
C     NLON    - Number of longitudes in the regular lat/long field
C     NLAT    - Number of latitudes in the regular lat/long field
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
C     JGETGG  - Reads the definition of a gaussian grid
C     HGENGRW - Calculates original lat/long (before rotation) for
C               a rotated grid for wind fields.
C     HNEI12  - Finds neighbours for points for interpolation
C     HWTS12  - Calculates weightings for points for interpolation
C     HIRLAMW_USERSPACE_1_GET - HIRLAMW userspace memory #1
C     HIRLAMW_USERSPACE_2_GET - HIRLAMW userspace memory #2
C     HIRLAMW_USERSPACE_3_GET - HIRLAMW userspace memory #3
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
      REAL OLDFLDU(*), NEWFLDU(KSIZE)
      REAL OLDFLDV(*), NEWFLDV(KSIZE)
      CHARACTER*1 HTYPE
C
C     Local variables
C
      INTEGER NEXT, LOOP, IRET, NLEN, NBYTES, NUMBER
      INTEGER NPREV, COUNTU, COUNTV, NEAREST
      CHARACTER*1 HPREV
C
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
      INTEGER IGG, IGGOLD
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
      REAL RCLAT(1),RCLON(1),RDIR(1),C(1),S(1),U(1),V(1)
      POINTER (IPRCLAT, RCLAT)
      POINTER (IPRCLON, RCLON)
      POINTER (IPRDIR, RDIR)
      POINTER (IPC, C)
      POINTER (IPS, S)
      POINTER (IPU, U)
      POINTER (IPV, V)
C
      REAL OLDU(KOUNT), OLDV(KOUNT)
C
      CHARACTER*7 FNAME
      PARAMETER  (FNAME = 'HIRLAMW')
C
      DATA IGGOLD/-1/
      DATA NUMBER/-1/
      DATA NPREV/-1/
      DATA HPREV/' '/
C
      SAVE IPKSCHE, IPNEIGH, IPKLA, IPPWTS
      SAVE IPPDLO0, IPPDLO1, IPPDLO2, IPPDLO3, IPPDLAT
      SAVE IGGOLD, IPKPTS, IPIOFFS, IPGLATS
      SAVE IPRLAT, IPRLON
      SAVE NUMBER
      SAVE NPREV
      SAVE HPREV
      SAVE IPRCLAT, IPRCLON, IPRDIR
      SAVE IPC, IPS, IPU, IPV
C
C     Externals
C
      INTEGER HNEI12
      INTEGER HGENGRW
#ifdef POINTER_64
      INTEGER*8
#else
      INTEGER
#endif
     X  HIRLAMW_USERSPACE_1_GET,
     X  HIRLAMW_USERSPACE_2_GET,
     X  HIRLAMW_USERSPACE_3_GET
C
C     Statement functions
C
      REAL A, B
      LOGICAL NOTEQ
C
C     Convert degrees to radians
C
      REAL X, RADIAN
      RADIAN(X) = (X) * 0.0174532925
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
      HIRLAMW = 0
C
      CALL JDEBUG()
C
      IF( L12PNT ) THEN
        CALL INTLOG(JP_DEBUG,FNAME//': 12-pt interpolation',JPQUIET)
      ELSE
        CALL INTLOG(JP_DEBUG,FNAME//': 4-pt interpolation',JPQUIET)
      ENDIF

      IF( HTYPE.NE.'O' .AND. HTYPE.NE.'R' .AND. HTYPE.NE.'U'
     X .AND. HTYPE.NE.'F' ) THEN
        CALL INTLOG(JP_ERROR,
     X    FNAME//': Gaussian type unsupported ('//HTYPE//')',JPQUIET)
        HIRLAMW = 1
        GOTO 900
      ENDIF

C     Dynamically allocate memory for gaussian grid information.
C
      IGG = KGAUSS*2
      IF( IGG.GT.IGGOLD ) THEN
C
        NBYTES = (IGG*JPRLEN) + (2*IGG+1)*JPBYTES
C
        IPKPTS = HIRLAMW_USERSPACE_1_GET(NBYTES)
#ifdef hpR64
        IPKPTS = IPKPTS/(1024*1024*1024*4)
#endif
        IF( IPKPTS.EQ.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      FNAME//': Memory allocate fail',JPQUIET)
          HIRLAMW = 1
          GOTO 900
        ENDIF
C
        IPGLATS = IPKPTS  + (IGG*JPBYTES)
        IPIOFFS = IPGLATS + (IGG*JPRLEN)
C
        IGGOLD = IGG
        NPREV = -1
        HPREV = ' '
C
      ENDIF
C
C     Build up offsets to start of each latitude in original field(s)
C
      IF( KGAUSS.NE.NPREV .AND. HPREV.NE.HTYPE) THEN
        CALL INTLOG(JP_DEBUG,FNAME//': trying Gaussian '//HTYPE,KGAUSS)
        CALL JGETGG(KGAUSS,HTYPE,GLATS,KPTS,IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      FNAME//': JGETGG failed to get gaussian data',JPQUIET)
          HIRLAMW = 2
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
          HIRLAMW = 3
          GOTO 900
        ENDIF
C
        NPREV = KGAUSS
        HPREV = HTYPE
      ENDIF
C
C     Preserve the input fields
C     (in case OLDFLDx and NEWFLDx are the same arrays)
C
      OLDU(1:NUMBER) = OLDFLDU(1:NUMBER)
      OLDV(1:NUMBER) = OLDFLDV(1:NUMBER)
C
C     -----------------------------------------------------------------|
C     Section 2.  Generate the lat/long points for the output grid
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
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
        HIRLAMW = 5
        GOTO 900
      ENDIF
C
C     Dynamically allocate memory for lat/long arrays.
C
      ILL = NLEN
      NBYTES = 9*ILL*JPRLEN
C
      IPRLON = HIRLAMW_USERSPACE_2_GET(NBYTES)
#ifdef hpR64
      IPRLON = IPRLON/(1024*1024*1024*4)
#endif
      IF( IPRLON.EQ.0 ) THEN
        CALL INTLOG(JP_ERROR,FNAME//': Memory allocate fail',JPQUIET)
        HIRLAMW = 6
        GOTO 900
      ENDIF
C
      IPRLAT  = IPRLON  + (ILL*JPRLEN)
      IPRCLON = IPRLAT  + (ILL*JPRLEN)
      IPRCLAT = IPRCLON + (ILL*JPRLEN)
      IPRDIR  = IPRCLAT + (ILL*JPRLEN)
      IPC     = IPRDIR  + (ILL*JPRLEN)
      IPS     = IPC     + (ILL*JPRLEN)
      IPU     = IPS     + (ILL*JPRLEN)
      IPV     = IPU     + (ILL*JPRLEN)
C
      IRET = HGENGRW(AREA,POLE,GRID,NLON,NLAT,RCLAT,RCLON,RLAT,RLON)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X      FNAME//': HGENGRW failed to get lat/lon grid',JPQUIET)
        HIRLAMW = 7
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
      NBYTES = (17*JPRLEN + 14*JPBYTES) * ILL
C
      IPPDLO0 = HIRLAMW_USERSPACE_3_GET(NBYTES)
#ifdef hpR64
      IPPDLO0 = IPPDLO0/(1024*1024*1024*4)
#endif
      IF( IPPDLO0.EQ.0 ) THEN
        CALL INTLOG(JP_ERROR,FNAME//': Memory allocate fail',JPQUIET)
        HIRLAMW = 8
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
     X      FNAME//': HNEI12 failed to find neighbours',JPQUIET)
        HIRLAMW = 9
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
          COUNTU = 0
          IF( NOTEQ(OLDU(NEIGH( 1,LOOP)),RMISSGV) ) COUNTU = COUNTU + 1
          IF( NOTEQ(OLDU(NEIGH( 2,LOOP)),RMISSGV) ) COUNTU = COUNTU + 1
          IF( NOTEQ(OLDU(NEIGH( 3,LOOP)),RMISSGV) ) COUNTU = COUNTU + 1
          IF( NOTEQ(OLDU(NEIGH( 4,LOOP)),RMISSGV) ) COUNTU = COUNTU + 1
          IF( NOTEQ(OLDU(NEIGH( 5,LOOP)),RMISSGV) ) COUNTU = COUNTU + 1
          IF( NOTEQ(OLDU(NEIGH( 6,LOOP)),RMISSGV) ) COUNTU = COUNTU + 1
          IF( NOTEQ(OLDU(NEIGH( 7,LOOP)),RMISSGV) ) COUNTU = COUNTU + 1
          IF( NOTEQ(OLDU(NEIGH( 8,LOOP)),RMISSGV) ) COUNTU = COUNTU + 1
          IF( NOTEQ(OLDU(NEIGH( 9,LOOP)),RMISSGV) ) COUNTU = COUNTU + 1
          IF( NOTEQ(OLDU(NEIGH(10,LOOP)),RMISSGV) ) COUNTU = COUNTU + 1
          IF( NOTEQ(OLDU(NEIGH(11,LOOP)),RMISSGV) ) COUNTU = COUNTU + 1
          IF( NOTEQ(OLDU(NEIGH(12,LOOP)),RMISSGV) ) COUNTU = COUNTU + 1
          COUNTV = 0
          IF( NOTEQ(OLDV(NEIGH( 1,LOOP)),RMISSGV) ) COUNTV = COUNTV + 1
          IF( NOTEQ(OLDV(NEIGH( 2,LOOP)),RMISSGV) ) COUNTV = COUNTV + 1
          IF( NOTEQ(OLDV(NEIGH( 3,LOOP)),RMISSGV) ) COUNTV = COUNTV + 1
          IF( NOTEQ(OLDV(NEIGH( 4,LOOP)),RMISSGV) ) COUNTV = COUNTV + 1
          IF( NOTEQ(OLDV(NEIGH( 5,LOOP)),RMISSGV) ) COUNTV = COUNTV + 1
          IF( NOTEQ(OLDV(NEIGH( 6,LOOP)),RMISSGV) ) COUNTV = COUNTV + 1
          IF( NOTEQ(OLDV(NEIGH( 7,LOOP)),RMISSGV) ) COUNTV = COUNTV + 1
          IF( NOTEQ(OLDV(NEIGH( 8,LOOP)),RMISSGV) ) COUNTV = COUNTV + 1
          IF( NOTEQ(OLDV(NEIGH( 9,LOOP)),RMISSGV) ) COUNTV = COUNTV + 1
          IF( NOTEQ(OLDV(NEIGH(10,LOOP)),RMISSGV) ) COUNTV = COUNTV + 1
          IF( NOTEQ(OLDV(NEIGH(11,LOOP)),RMISSGV) ) COUNTV = COUNTV + 1
          IF( NOTEQ(OLDV(NEIGH(12,LOOP)),RMISSGV) ) COUNTV = COUNTV + 1
C
C         Interpolate using twelve neighbours if none are missing
C
          IF( (COUNTU.EQ.12).AND.(COUNTV.EQ.12) ) THEN
C
            U(LOOP) =
     X        OLDU(NEIGH( 1,LOOP)) * PWTS( 1,LOOP) +
     X        OLDU(NEIGH( 2,LOOP)) * PWTS( 2,LOOP) +
     X        OLDU(NEIGH( 3,LOOP)) * PWTS( 3,LOOP) +
     X        OLDU(NEIGH( 4,LOOP)) * PWTS( 4,LOOP) +
     X        OLDU(NEIGH( 5,LOOP)) * PWTS( 5,LOOP) +
     X        OLDU(NEIGH( 6,LOOP)) * PWTS( 6,LOOP) +
     X        OLDU(NEIGH( 7,LOOP)) * PWTS( 7,LOOP) +
     X        OLDU(NEIGH( 8,LOOP)) * PWTS( 8,LOOP) +
     X        OLDU(NEIGH( 9,LOOP)) * PWTS( 9,LOOP) +
     X        OLDU(NEIGH(10,LOOP)) * PWTS(10,LOOP) +
     X        OLDU(NEIGH(11,LOOP)) * PWTS(11,LOOP) +
     X        OLDU(NEIGH(12,LOOP)) * PWTS(12,LOOP)
            V(LOOP) =
     X        OLDV(NEIGH( 1,LOOP)) * PWTS( 1,LOOP) +
     X        OLDV(NEIGH( 2,LOOP)) * PWTS( 2,LOOP) +
     X        OLDV(NEIGH( 3,LOOP)) * PWTS( 3,LOOP) +
     X        OLDV(NEIGH( 4,LOOP)) * PWTS( 4,LOOP) +
     X        OLDV(NEIGH( 5,LOOP)) * PWTS( 5,LOOP) +
     X        OLDV(NEIGH( 6,LOOP)) * PWTS( 6,LOOP) +
     X        OLDV(NEIGH( 7,LOOP)) * PWTS( 7,LOOP) +
     X        OLDV(NEIGH( 8,LOOP)) * PWTS( 8,LOOP) +
     X        OLDV(NEIGH( 9,LOOP)) * PWTS( 9,LOOP) +
     X        OLDV(NEIGH(10,LOOP)) * PWTS(10,LOOP) +
     X        OLDV(NEIGH(11,LOOP)) * PWTS(11,LOOP) +
     X        OLDV(NEIGH(12,LOOP)) * PWTS(12,LOOP)
C
C         Set missing if all neighbours are missing
C
          ELSE IF( (COUNTU.EQ.0).OR.(COUNTV.EQ.0) ) THEN
            U(LOOP) = RMISSGV
            V(LOOP) = RMISSGV
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
            U(LOOP) = OLDU(NEIGH( NEAREST,LOOP))
            V(LOOP) = OLDV(NEIGH( NEAREST,LOOP))
          ENDIF
C
        ELSE IF( KSCHEME(LOOP).EQ.JP4PT ) THEN
C
C     See if any of the neighbours are missing
C
          COUNTU = 0
          IF( NOTEQ(OLDU(NEIGH( 1,LOOP)),RMISSGV) ) COUNTU = COUNTU + 1
          IF( NOTEQ(OLDU(NEIGH( 2,LOOP)),RMISSGV) ) COUNTU = COUNTU + 1
          IF( NOTEQ(OLDU(NEIGH( 3,LOOP)),RMISSGV) ) COUNTU = COUNTU + 1
          IF( NOTEQ(OLDU(NEIGH( 4,LOOP)),RMISSGV) ) COUNTU = COUNTU + 1
          COUNTV = 0
          IF( NOTEQ(OLDV(NEIGH( 1,LOOP)),RMISSGV) ) COUNTV = COUNTV + 1
          IF( NOTEQ(OLDV(NEIGH( 2,LOOP)),RMISSGV) ) COUNTV = COUNTV + 1
          IF( NOTEQ(OLDV(NEIGH( 3,LOOP)),RMISSGV) ) COUNTV = COUNTV + 1
          IF( NOTEQ(OLDV(NEIGH( 4,LOOP)),RMISSGV) ) COUNTV = COUNTV + 1
C
C         Interpolate using four neighbours if none are missing
C
          IF( (COUNTU.EQ.4).AND.(COUNTV.EQ.4) ) THEN
C
            U(LOOP) =
     X        OLDU(NEIGH( 1,LOOP)) * PWTS( 1,LOOP) +
     X        OLDU(NEIGH( 2,LOOP)) * PWTS( 2,LOOP) +
     X        OLDU(NEIGH( 3,LOOP)) * PWTS( 3,LOOP) +
     X        OLDU(NEIGH( 4,LOOP)) * PWTS( 4,LOOP)
            V(LOOP) =
     X        OLDV(NEIGH( 1,LOOP)) * PWTS( 1,LOOP) +
     X        OLDV(NEIGH( 2,LOOP)) * PWTS( 2,LOOP) +
     X        OLDV(NEIGH( 3,LOOP)) * PWTS( 3,LOOP) +
     X        OLDV(NEIGH( 4,LOOP)) * PWTS( 4,LOOP)
C
C         Set missing if all neighbours are missing
C
          ELSE IF( (COUNTU.EQ.0).OR.(COUNTV.EQ.0) ) THEN
            U(LOOP) = RMISSGV
            V(LOOP) = RMISSGV
C
C         Otherwise, use the nearest neighbour
C
          ELSE
            NEAREST = 1
            IF( PWTS( 2,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 2
            IF( PWTS( 3,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 3
            IF( PWTS( 4,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 4
            U(LOOP) = OLDU(NEIGH( NEAREST,LOOP))
            V(LOOP) = OLDV(NEIGH( NEAREST,LOOP))
          ENDIF
C
        ELSE
          DO NEXT = 1, 4
            IF( NEIGH(NEXT,LOOP).NE.0 ) THEN
              U(LOOP) = OLDU(NEIGH(NEXT,LOOP))
              V(LOOP) = OLDV(NEIGH(NEXT,LOOP))
            ENDIF
          ENDDO
C
        ENDIF
      ENDDO
C
C     -----------------------------------------------------------------|
C     Section 5.  Adjust the U and V components to new (rotated) axes
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
      DO LOOP = 1, NLEN
        RDIR(LOOP) = 0.0
      ENDDO
C
      CALL HPSHGPW(RLON,RLAT,RCLON,RCLAT,RDIR,NLEN,POLE)
C
      DO LOOP = 1, NLEN
        RDIR(LOOP) = -RADIAN(RDIR(LOOP))
      ENDDO
C
      DO LOOP = 1, NLEN
        C(LOOP) = COS(RDIR(LOOP))
        S(LOOP) = SIN(RDIR(LOOP))
      ENDDO
C
      DO LOOP = 1, NLEN
        IF( NOTEQ(U(LOOP),RMISSGV).AND.NOTEQ(V(LOOP),RMISSGV) ) THEN
          NEWFLDU(LOOP) = U(LOOP)*C(LOOP) - V(LOOP)*S(LOOP)
          NEWFLDV(LOOP) = U(LOOP)*S(LOOP) + V(LOOP)*C(LOOP)
        ELSE
          NEWFLDU(LOOP) = RMISSGV
          NEWFLDV(LOOP) = RMISSGV
        ENDIF
      ENDDO
C
C     -----------------------------------------------------------------|
C     Section 9.  Return.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
      RETURN
      END
