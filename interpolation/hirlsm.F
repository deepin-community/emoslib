C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HIRLSM(L12PNT,OLDFLD,KOUNT,KGAUSS,
     X  HTYPE,AREA,POLE,GRID,NEWFLD,KSIZE,NLON,NLAT)
C
C---->
C**** HIRLSM
C
C     Purpose
C     -------
C
C     This routine creates a rotated regular lat/long field from a
C     reduced gaussian field using 12-point horizontal interpolation
C     and land-sea masks.
C
C
C     Interface
C     ---------
C
C     IRET = HIRLSM(L12PNT,OLDFLD,KOUNT,KGAUSS,HTYPE,AREA,POLE,GRID,
C    X              NEWFLD,KSIZE,NLON,NLAT)
C
C
C     Input parameters
C     ----------------
C
C     L12PNT - Chooses between 12-point and 4-point interpolation
C              = .TRUE. for 12-point horizontal
C              = .FALSE. for 4-point
C     OLDFLD - Array of values from the (old) reduced gaussian field
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
C     HWTSLSM - Calculates LSM weightings for points for interpolation
C     HGETLSM - Reads values for gaussian land-sea mask
C     FORCED_NEAREST_NEIGHBOUR - check forced interpolation method
C     HIRLSM_USERSPACE_1_GET   - HIRLAM userspace memory #1
C     HIRLSM_USERSPACE_2_GET   - HIRLAM userspace memory #2
C     HIRLSM_USERSPACE_3_GET   - HIRLAM userspace memory #3
C     HIRLSM_USERSPACE_4_GET   - HIRLAM userspace memory #4
C     HIRLSM_USERSPACE_5_GET   - HIRLAM userspace memory #5
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
      INTEGER NPREV
      INTEGER INUM
      INTEGER IOSIZE, INSIZE
      INTEGER NEWSIZE
      REAL OLDAREA(4), OLDPOLE(2), OLDGRID(2)
      REAL OLDLSM(1)
      POINTER (IPOLDLS, OLDLSM )
C
      INTEGER KSCHEME(1),NEIGH(12,1), KLA(1)
      REAL PWTS(12,1)
      POINTER (IPKSCHE, KSCHEME)
      POINTER (IPNEIGH, NEIGH)
      POINTER (IPKLA,   KLA)
      POINTER (IPPWTS,  PWTS)
      LOGICAL LVEGGY
      CHARACTER*12 Y1FLAG, Y2FLAG
      LOGICAL LNEWSPC, LOLDNEW
      LOGICAL LSOIL  ! Special processing for soil parameters
      LOGICAL LITAL
      LOGICAL LVEGGY2
      INTEGER KSCHEMEN(1)
      REAL PWTSN(12,1)
      POINTER (IPKSCHEN, KSCHEMEN)
      POINTER (IPPWTSN,  PWTSN)
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
C
      REAL OLD(KOUNT)
C
      CHARACTER*7 FNAME
      PARAMETER  (FNAME = 'HIRLSM ')
C
      DATA IGGOLD/-1/, NUMBER/-1/, NPREV/-1/
C
      REAL NEWLSM(1)
      POINTER (IPNEWLS, NEWLSM)
      DATA OLDAREA/4*-1.0/, OLDPOLE/2*-1.0/, OLDGRID/2*-1.0/
      DATA IOSIZE/-1/, INSIZE/-1/
      DATA IPNEWLS/0/, IPOLDLS/0/
C
      SAVE IGGOLD, NUMBER, NPREV
      SAVE IPKPTS, IPIOFFS, IPGLATS, IPOLDLS
      SAVE IOSIZE, INSIZE
      SAVE OLDAREA, OLDPOLE, OLDGRID
C
C     Externals
C
      LOGICAL FORCED_NEAREST_NEIGHBOUR
      INTEGER HNEI12
      INTEGER HGENGRD
      INTEGER HGETLSM
#ifdef POINTER_64
      INTEGER*8
#else
      INTEGER
#endif
     X  HIRLSM_USERSPACE_1_GET,
     X  HIRLSM_USERSPACE_2_GET,
     X  HIRLSM_USERSPACE_3_GET,
     X  HIRLSM_USERSPACE_4_GET,
     X  HIRLSM_USERSPACE_5_GET
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
      IPKSCHEN = 0
      IPPWTSN  = 0
      HIRLSM = 0
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

      IF( HTYPE.NE.'O' .AND. HTYPE.NE.'R' .AND. HTYPE.NE.'U' ) THEN
        CALL INTLOG(JP_ERROR,
     X    FNAME//': Gaussian type unsupported ('//HTYPE//')',JPQUIET)
        HIRLSM = 1
        GOTO 900
      ENDIF


C     Set method based on environment variables
      CALL GETENV('SOIL_PARAM',            Y1FLAG)
      CALL GETENV('NEAREST_NEIGHBOUR_NEW', Y2FLAG)


C     Use nearest neighbour if required
      LVEGGY = FORCED_NEAREST_NEIGHBOUR(LMETHOD,NITABLE,NIPARAM)


C
C     Fields with bitmap (as SST) and VEGGY Parameters are treated
C     with a corrected nearest neighbour method matching LSM
C     Force nearest neighbour for same type lsm processing
C      with INTOUT parameter
      LVEGGY2 = .FALSE.
      LSOIL   = .FALSE.  ! Special processing for soil parameters
      LITAL   = .FALSE.
      IF( Y2FLAG(1:1).EQ.'1'.OR.LNNLSM ) THEN
C       envvar NEAREST_NEIGHBOUR_NEW=1, or
C       intout interpolation="nearest lsm"
        IF(LVEGGY) LVEGGY2 = .TRUE.
        LVEGGY = .FALSE.
        LITAL  = .TRUE.
      ELSEIF( Y1FLAG(1:1).EQ.'1' .AND. LVEGGY ) THEN
C       envvar SOIL_PARAM=1, and
C       ( vegetation parameter or
C         envvar NEAREST_NEIGHBOUR=1 or
C         intout interpolation="nearest neighbour" )
        LSOIL  = .TRUE.
        LVEGGY = .FALSE.
      ENDIF

      IF( LVEGGY ) CALL INTLOG(JP_DEBUG,
     X  FNAME//': nearest neighbour processing: vegetation',JPQUIET)
      IF( LSOIL  ) CALL INTLOG(JP_DEBUG,
     X  FNAME//': nearest neighbour processing: soil',      JPQUIET)
      IF( LITAL  ) CALL INTLOG(JP_DEBUG,
     X  FNAME//': nearest neighbour processing: new',       JPQUIET)


C
C     Dynamically allocate memory for gaussian grid information.
C
      IGG = KGAUSS*2
      IF( IGG.GT.IGGOLD ) THEN
C
        NBYTES = (IGG*JPRLEN) + (2*IGG+1)*JPBYTES
C
        IPKPTS = HIRLSM_USERSPACE_1_GET(NBYTES)
#ifdef hpR64
        IPKPTS = IPKPTS/(1024*1024*1024*4)
#endif
        IF( IPKPTS.EQ.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      FNAME//': Memory allocate fail',JPQUIET)
          HIRLSM = 1
          GOTO 900
        ENDIF
C
        IPGLATS = IPKPTS  + (IGG*JPBYTES)
        IPIOFFS = IPGLATS + (IGG*JPRLEN)
C
        IGGOLD = IGG
        NPREV = -1
C
      ENDIF
C
C     Has gaussian grid changed from last time through?
C
CC    LOLDNEW = (KGAUSS.NE.NPREV)  this forces reloading the LSM always!
      LOLDNEW = .TRUE.
C
C     Build up offsets to start of each latitude in original field(s)
      IF( LOLDNEW ) THEN
        CALL INTLOG(JP_DEBUG,FNAME//': trying Gaussian '//HTYPE,KGAUSS)
        CALL JGETGG(KGAUSS,HTYPE,GLATS,KPTS,IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      FNAME//': JGETGG failed to get gaussian data',JPQUIET)
          HIRLSM = 2
          GOTO 900
        ENDIF
C
        IOFFS(1) = 1
        DO LOOP = 2, (KGAUSS*2+1)
          IOFFS(LOOP) = IOFFS(LOOP-1) + KPTS(LOOP-1)
        ENDDO
C
        NPREV = KGAUSS
      ENDIF
C
      NUMBER = (IOFFS(KGAUSS*2+1) - 1)
      IF( NUMBER.NE.KOUNT ) THEN
        CALL INTLOG(JP_ERROR,FNAME//': Given no. of points =',KOUNT)
        CALL INTLOG(JP_ERROR,FNAME//': Expected no. of pts =',NUMBER)
        HIRLSM = 4
        GOTO 900
      ENDIF
C
C     Preserve the input field
C     (in case OLDFLD and NEWFLD are the same arrays)
C
      OLD(1:NUMBER) = OLDFLD(1:NUMBER)
C
C     If old land-sea mask has changed, pick up different land-sea mask
C
      IF( LOLDNEW ) THEN
C
C       Allocate memory for old land-sea mask
C
        IOSIZE = NUMBER*JPRLEN
        IPOLDLS = HIRLSM_USERSPACE_2_GET(IOSIZE)
        IF( IPOLDLS.EQ.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      FNAME//': Memory allocate fail',JPQUIET)
          HIRLSM = 4
          GOTO 900
        ENDIF
C
C       Read values for different land-sea mask in memory
C
        NEWSIZE = NUMBER*JPRLEN
        IRET = HGETLSM(KGAUSS,HTYPE,OLDLSM,NEWSIZE)
Cs        IRET = HGETLSM(KGAUSS,OLDLSM,IOSIZE)

        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      FNAME//': HGETLSM failed to get LSM data',JPQUIET)
          HIRLSM = 4
          GOTO 900
        ENDIF
      ENDIF
C
C     -----------------------------------------------------------------|
C     Section 2.  Generate the lat/long points for the output grid
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C Sinisa put nint instead int (SC Aug-2005)
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
        HIRLSM = 5
        GOTO 900
      ENDIF
C
C     Dynamically allocate memory for lat/long arrays.
C
      ILL = NLEN
      NBYTES = 2*ILL*JPRLEN
C
      IPRLON = HIRLSM_USERSPACE_3_GET(NBYTES)
#ifdef hpR64
      IPRLON = IPRLON/(1024*1024*1024*4)
#endif
      IF( IPRLON.EQ.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    FNAME//': Memory allocate fail',JPQUIET)
        HIRLSM = 6
        GOTO 900
      ENDIF
C
      IPRLAT  = IPRLON + (ILL*JPRLEN)
C
      IRET = HGENGRD(AREA,POLE,GRID,NLON,NLAT,RLAT,RLON)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    FNAME//': HGENGRD failed to get lat/lon grid data',JPQUIET)
        HIRLSM = 7
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
      IF (LITAL.AND.L12PNT) THEN
        NJPR=29
        NJPB=15
      ENDIF
      NBYTES = (NJPR*JPRLEN + NJPB*JPBYTES) * ILL
C
      IPPDLO0 = HIRLSM_USERSPACE_4_GET(NBYTES)
#ifdef hpR64
      IPPDLO0 = IPPDLO0/(1024*1024*1024*4)
#endif
      IF( IPPDLO0.EQ.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    FNAME//': Memory allocate fail',JPQUIET)
        HIRLSM = 8
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
      IF (LITAL.AND.L12PNT) THEN
        IPPWTSN  = IPNEIGH + (12*ILL*JPBYTES)
        IPKSCHEN = IPPWTSN  + (12*ILL*JPRLEN)
      ENDIF
C
C
C     -----------------------------------------------------------------|
C     Section 4.  Create the new land-sea mask
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
C     Create new land-sea mask if specification has changed
C
      LNEWSPC = .FALSE.
      IF( AREA(1).NE.OLDAREA(1) ) LNEWSPC = .TRUE.
      IF( AREA(2).NE.OLDAREA(2) ) LNEWSPC = .TRUE.
      IF( AREA(3).NE.OLDAREA(3) ) LNEWSPC = .TRUE.
      IF( AREA(4).NE.OLDAREA(4) ) LNEWSPC = .TRUE.
      IF( GRID(1).NE.OLDGRID(1) ) LNEWSPC = .TRUE.
      IF( GRID(2).NE.OLDGRID(2) ) LNEWSPC = .TRUE.
      IF( POLE(1).NE.OLDPOLE(1) ) LNEWSPC = .TRUE.
      IF( POLE(2).NE.OLDPOLE(2) ) LNEWSPC = .TRUE.
C
      IF( .NOT.LNEWSPC ) GOTO 500
C
      OLDAREA(1) = AREA(1)
      OLDAREA(2) = AREA(2)
      OLDAREA(3) = AREA(3)
      OLDAREA(4) = AREA(4)
      OLDGRID(1) = GRID(1)
      OLDGRID(2) = GRID(2)
      OLDPOLE(1) = POLE(1)
      OLDPOLE(2) = POLE(2)
C
C     Allocate memory for new land-sea mask
C
      INSIZE = NLEN*JPRLEN
      IPNEWLS = HIRLSM_USERSPACE_5_GET(INSIZE)
      IF( IPNEWLS.EQ.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    FNAME//': Memory allocation fail',JPQUIET)
        HIRLSM = 4
        GOTO 900
      ENDIF
C
C     Find neighbours.
C     (Note: 4-point scheme is used for land-sea mask interpolation).
C
      IRET = HNEI12(.FALSE.,NLEN,RLAT,RLON,KGAUSS,KPTS,GLATS,
     X              KSCHEME,PDLAT,PDLO0,PDLO1,PDLO2,PDLO3,KLA,NEIGH)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    FNAME//': HNEI12 failed to find neighbours',JPQUIET)
        HIRLSM = 9
        GOTO 900
      ENDIF
C
C     Perform the interpolation for the new land-sea mask.
C
      CALL HWTS12
     X  (NLEN,KSCHEME,KLA,PDLAT,GLATS,PDLO0,PDLO1,PDLO2,PDLO3,NEIGH,
     X   PWTS)
C
C     Calculate the interpolated grid point values
C
      DO LOOP = 1, NLEN
        IF( KSCHEME(LOOP).EQ.JP12PT ) THEN
          NEWLSM(LOOP) =
     X      OLDLSM(NEIGH( 1,LOOP)) * PWTS( 1,LOOP) +
     X      OLDLSM(NEIGH( 2,LOOP)) * PWTS( 2,LOOP) +
     X      OLDLSM(NEIGH( 3,LOOP)) * PWTS( 3,LOOP) +
     X      OLDLSM(NEIGH( 4,LOOP)) * PWTS( 4,LOOP) +
     X      OLDLSM(NEIGH( 5,LOOP)) * PWTS( 5,LOOP) +
     X      OLDLSM(NEIGH( 6,LOOP)) * PWTS( 6,LOOP) +
     X      OLDLSM(NEIGH( 7,LOOP)) * PWTS( 7,LOOP) +
     X      OLDLSM(NEIGH( 8,LOOP)) * PWTS( 8,LOOP) +
     X      OLDLSM(NEIGH( 9,LOOP)) * PWTS( 9,LOOP) +
     X      OLDLSM(NEIGH(10,LOOP)) * PWTS(10,LOOP) +
     X      OLDLSM(NEIGH(11,LOOP)) * PWTS(11,LOOP) +
     X      OLDLSM(NEIGH(12,LOOP)) * PWTS(12,LOOP)
C
        ELSE IF( KSCHEME(LOOP).EQ.JP4PT ) THEN
C
          NEWLSM(LOOP) =
     X      OLDLSM(NEIGH( 1,LOOP)) * PWTS( 1,LOOP) +
     X      OLDLSM(NEIGH( 2,LOOP)) * PWTS( 2,LOOP) +
     X      OLDLSM(NEIGH( 3,LOOP)) * PWTS( 3,LOOP) +
     X      OLDLSM(NEIGH( 4,LOOP)) * PWTS( 4,LOOP)
C
C
        ELSE
          DO NEXT = 1, 4
            IF( NEIGH(NEXT,LOOP).NE.0 )
     X        NEWLSM(LOOP) = OLDLSM(NEIGH(NEXT,LOOP))
          ENDDO
C
        ENDIF
C
      ENDDO

C
C     -----------------------------------------------------------------|
C     Section 5.  Perform the 12-point horizontal interpolation.
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
C     Fin neighbours.
C
      IRET = HNEI12(L12PNT,NLEN,RLAT,RLON,KGAUSS,KPTS,GLATS,
     X              KSCHEME,PDLAT,PDLO0,PDLO1,PDLO2,PDLO3,KLA,NEIGH)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    FNAME//': HNEI12 failed to find neighbours',JPQUIET)
        HIRLSM = 5
        GOTO 900
      ENDIF
C
C     Setup the 12-point horizontal interpolation weights
C
      CALL HWTSLSM
     X  (NLEN,KSCHEME,KLA,PDLAT,GLATS,PDLO0,PDLO1,PDLO2,PDLO3,NEIGH,
     X   OLDLSM,NEWLSM,PWTS)
C
C     Calculate the interpolated grid point values
C
      IF (LITAL.AND.L12PNT) THEN
        DO LOOP = 1, NLEN
          IF (KSCHEME(LOOP).EQ.JP12PT) THEN
            KSCHEMEN(LOOP)=JP4PT
          ELSE
            KSCHEMEN(LOOP)=KSCHEME(LOOP)
          ENDIF
        ENDDO
        CALL HWTSLSM
     X  (NLEN,KSCHEMEN,KLA,PDLAT,GLATS,PDLO0,PDLO1,PDLO2,PDLO3,NEIGH,
     X   OLDLSM,NEWLSM,PWTSN)
      ENDIF

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
        ELSE IF( LSOIL ) THEN
          IF(NEWLSM(LOOP).GE.0.5)THEN
            NEAREST = 1
            IF( PWTS( 2,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 2,LOOP)).GE.0.5)) NEAREST =  2
            IF( PWTS( 3,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 3,LOOP)).GE.0.5)) NEAREST =  3
            IF( PWTS( 4,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 4,LOOP)).GE.0.5)) NEAREST =  4
            IF( PWTS( 5,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 5,LOOP)).GE.0.5)) NEAREST =  5
            IF( PWTS( 6,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 6,LOOP)).GE.0.5)) NEAREST =  6
            IF( PWTS( 7,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 7,LOOP)).GE.0.5)) NEAREST =  7
            IF( PWTS( 8,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 8,LOOP)).GE.0.5)) NEAREST =  8
            IF( PWTS( 9,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 9,LOOP)).GE.0.5)) NEAREST =  9
            IF( PWTS(10,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH(10,LOOP)).GE.0.5)) NEAREST = 10
            IF( PWTS(11,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH(11,LOOP)).GE.0.5)) NEAREST = 11
            IF( PWTS(12,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH(12,LOOP)).GE.0.5)) NEAREST = 12
          ELSE
            NEAREST = 1
            IF( PWTS( 2,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 2,LOOP)).LT.0.5)) NEAREST =  2
            IF( PWTS( 3,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 3,LOOP)).LT.0.5)) NEAREST =  3
            IF( PWTS( 4,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 4,LOOP)).LT.0.5)) NEAREST =  4
            IF( PWTS( 5,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 5,LOOP)).LT.0.5)) NEAREST =  5
            IF( PWTS( 6,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 6,LOOP)).LT.0.5)) NEAREST =  6
            IF( PWTS( 7,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 7,LOOP)).LT.0.5)) NEAREST =  7
            IF( PWTS( 8,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 8,LOOP)).LT.0.5)) NEAREST =  8
            IF( PWTS( 9,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 9,LOOP)).LT.0.5)) NEAREST =  9
            IF( PWTS(10,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH(10,LOOP)).LT.0.5)) NEAREST = 10
            IF( PWTS(11,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH(11,LOOP)).LT.0.5)) NEAREST = 11
            IF( PWTS(12,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH(12,LOOP)).LT.0.5)) NEAREST = 12
          ENDIF
            NEWFLD(LOOP) = OLD(NEIGH( NEAREST,LOOP))
        ELSE
          IF( COUNT.EQ.12 .AND..NOT.LVEGGY2) THEN
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
          ELSE IF( COUNT.EQ.0 .AND..NOT.LVEGGY2) THEN
            NEWFLD(LOOP) = RMISSGV
C
C         Otherwise, use the nearest neighbour
C
          ELSE


      IF(LITAL)THEN
           NEAREST = 1
           IF(NEWLSM(LOOP).GE.0.5)THEN     !land point
            INUM=0
            DO NEXT=1,4
              IF(OLDLSM(NEIGH( NEXT,LOOP)).GE.0.5)THEN
                INUM=INUM+1
                IF( INUM.GE.2)THEN
                  IF(PWTSN( NEXT,LOOP).GT.PWTSN(NEAREST,LOOP)) THEN
                    NEAREST = NEXT
                  ENDIF
                ELSEIF( INUM.EQ.1) THEN
                  NEAREST = NEXT
                ENDIF
              ENDIF
            ENDDO

            ELSE  !sea point

            INUM=0
            DO NEXT=1,4
              IF(OLDLSM(NEIGH( NEXT,LOOP)).LT.0.5)THEN
                INUM=INUM+1
                IF( INUM.GE.2)THEN
                  IF(PWTSN( NEXT,LOOP).GT.PWTSN(NEAREST,LOOP)) THEN
                    NEAREST = NEXT
                  ENDIF
                ELSEIF( INUM.EQ.1) THEN
                  NEAREST = NEXT
                ENDIF
              ENDIF
            ENDDO

            ENDIF  !end if sea/land
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
       ENDIF

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
        IF( LVEGGY) THEN

            NEAREST = 1
            IF( PWTS( 2,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 2
            IF( PWTS( 3,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 3
            IF( PWTS( 4,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 4
            NEWFLD(LOOP) = OLD(NEIGH( NEAREST,LOOP))

        ELSE IF( LSOIL ) THEN

          IF(NEWLSM(LOOP).GE.0.5)THEN
C           land point
            NEAREST = 1
            IF( PWTS( 2,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 2,LOOP)).GE.0.5)) NEAREST = 2
            IF( PWTS( 3,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 3,LOOP)).GE.0.5)) NEAREST = 3
            IF( PWTS( 4,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 4,LOOP)).GE.0.5)) NEAREST = 4
          ELSE
C           sea point
            NEAREST = 1
            IF( PWTS( 2,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 2,LOOP)).LT.0.5)) NEAREST = 2
            IF( PWTS( 3,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 3,LOOP)).LT.0.5)) NEAREST = 3
            IF( PWTS( 4,LOOP).GT.PWTS(NEAREST,LOOP) .AND.
     X        (OLDLSM(NEIGH( 4,LOOP)).LT.0.5)) NEAREST = 4
          ENDIF
          NEWFLD(LOOP) = OLD(NEIGH( NEAREST,LOOP))

        ELSE

          IF( COUNT.EQ.4 .AND. .NOT.LVEGGY2) THEN
            NEWFLD(LOOP) =
     X        OLD(NEIGH( 1,LOOP)) * PWTS( 1,LOOP) +
     X        OLD(NEIGH( 2,LOOP)) * PWTS( 2,LOOP) +
     X        OLD(NEIGH( 3,LOOP)) * PWTS( 3,LOOP) +
     X        OLD(NEIGH( 4,LOOP)) * PWTS( 4,LOOP)
C
C         Set missing if all neighbours are missing
C
          ELSE IF( COUNT.EQ.0 .AND. .NOT.LVEGGY2) THEN
            NEWFLD(LOOP) = RMISSGV
C
C         Otherwise, use the nearest neighbour
C
          ELSE

            IF(LITAL)THEN

              IF(NEWLSM(LOOP).GE.0.5)THEN
C               land point
                NEAREST = 1
                INUM=0
                DO NEXT=1,4
                  IF(OLDLSM(NEIGH( NEXT,LOOP)).GE.0.5)THEN
                    INUM=INUM+1
                    IF( INUM.GE.2)THEN
                      IF(PWTS( NEXT,LOOP).GT.PWTS(NEAREST,LOOP)) THEN
                        NEAREST = NEXT
                      ENDIF
                    ELSEIF( INUM.EQ.1) THEN
                      NEAREST = NEXT
                    ENDIF
                  ENDIF
                ENDDO
              ELSE
C               sea point
                NEAREST = 1
                INUM=0
                DO NEXT=1,4
                  IF(OLDLSM(NEIGH( NEXT,LOOP)).LT.0.5)THEN
                    INUM=INUM+1
                    IF( INUM.GE.2)THEN
                      IF(PWTS( NEXT,LOOP).GT.PWTS(NEAREST,LOOP)) THEN
                        NEAREST = NEXT
                      ENDIF
                    ELSEIF( INUM.EQ.1) THEN
                      NEAREST = NEXT
                    ENDIF
                  ENDIF
                ENDDO
              ENDIF

            ELSE

c             (.NOT. LITAL)
              NEAREST = 1
              IF( PWTS( 2,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 2
              IF( PWTS( 3,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 3
              IF( PWTS( 4,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 4

            ENDIF

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
C       For sea-ice cover, ensure no values are outside the range (0,1)
C
        IF( (NIPARAM.EQ.31).AND.(NITABLE.EQ.128) ) THEN
          IF( NOTEQ( NEWFLD(LOOP),RMISSGV) ) THEN
            IF( NEWFLD(LOOP).GT.1.0 ) NEWFLD(LOOP) = 1.0
            IF( NEWFLD(LOOP).LT.0.0 ) NEWFLD(LOOP) = 0.0
          ENDIF
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

