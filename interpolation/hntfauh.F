C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HNTFAUH(FLDIN,INLEN)
C
C---->
C**** HNTFAUH
C
C     Purpose
C     -------
C
C     Prepare to interpolate to grid point field.
C
C
C     Interface
C     ---------
C
C     IRET = HNTFAUH( FLDIN,INLEN)
C
C     Input
C     -----
C
C     FLDIN  - Input field (unpacked).
C     INLEN  - Input field length (words).
C
C
C     Output
C     ------
C
C     Field unpacked values are in ZNFELDI, rotated if necessary.
C
C     Returns: 0, if OK. Otherwise, an error occured in interpolation.
C
C
C     Method
C     ------
C
C     If the input is a spectral field and the output is a rotated
C     grid-point field, create a global reduced gaussian field and
C     then create the rotated grid-point field from it.
C
C
C     Externals
C     ---------
C
C     INTLOG  - Log error message.
C     INTLOGR - Log error message.
C     JDEBUG  - Checks environment to switch on/off debug
C     FIXAREA - Fixup input/output field area definitions.
C     HSH2GG  - Find suitable gaussian grid/spectral truncation
C     HIRLAM  - Creates rotated lat/long field from reduced gaussian
C     HIRLSM  - Creates rotated lat/long field from reduced gaussian
C               using land-sea mask
C     HRG2GG  - Creates rotated gaussian field from reduced gaussian
C     LSMFLD  - Determines whether a field is to be interpolated using
C               a land-sea mask
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     February 2001
C
C----<
C
C     -----------------------------------------------------------------|
C*    Section 0.   Variables
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
C     Function arguments
C
      INTEGER INLEN
      REAL FLDIN(*)
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "intf.h"
#include "current.h"
#include "grfixed.h"
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 40150)
C
C     Local variables
C
      CHARACTER*1 HOLDTYP
      CHARACTER*1 HTYPE
      INTEGER     IERR
      INTEGER     IRET
      INTEGER     ISHIZE
      INTEGER     ISIZE
      INTEGER     ITEMP,K,J
      INTEGER     KPTS(JPGTRUNC*2)
      INTEGER     LOOP
      INTEGER     NCOUNT
      INTEGER     NGAUSS
      INTEGER     NLAT
      INTEGER     NLON
      INTEGER     NTRUNC
      INTEGER     NUMPTS
      INTEGER     NUVFLAG
      LOGICAL     LLATOUT
      LOGICAL     LSP2RGG
      LOGICAL     LUSELSM
      REAL        AREA(4)
      REAL        EAST
      REAL        EW
      REAL        GRID(2)
      REAL        NS
      REAL        PLATS(JPGTRUNC*2)
      REAL        POLE(2)
      REAL        RGGRID(1)
      REAL        SWORK(1)
      REAL        ROW(5120) !2560 is enough for 2017-07-18 UKMO grids
      REAL        WEST
      REAL        ZNFLDO(1)
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IRGGRID
      INTEGER*8 ISWORK
      INTEGER*8 IZNFLDO
#endif
#endif
      POINTER (IRGGRID, RGGRID)
      POINTER (ISWORK,  SWORK)
      POINTER (IZNFLDO, ZNFLDO)
C
      SAVE IRGGRID
      SAVE ISWORK
      SAVE IZNFLDO
C
      DATA ISIZE/0/
C
C     Externals
C
      CHARACTER*1 GGHTYPE
      INTEGER HSH2GG, HIRLAM, HIRLSM, HRG2GG, FIXAREA, PDDEFS
      LOGICAL LSMFLD
      EXTERNAL GGHTYPE
      EXTERNAL HSH2GG, HIRLAM, HIRLSM, HRG2GG, FIXAREA, PDDEFS
      EXTERNAL LSMFLD

C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      HNTFAUH = 0
      IRET    = 0
C
      NCOUNT = INLEN
      IF( LIMISSA ) THEN
        CALL INTLOG(JP_DEBUG,'HNTFAUH: Use missing value',JPQUIET)
        LIMISSV = .TRUE.
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2.   Store input data.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Allocate work array ZNFELDI if not already done.
C
      IF( IZNJDCI.NE.1952999238 ) THEN
        CALL JMEMHAN( 19, IZNFLDI, JPEXPAND, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_WARN,'HNTFAUH: ZNFELDI allocate fail',JPQUIET)
          HNTFAUH = IRET
          GOTO 900
        ENDIF
        IZNJDCI = 1952999238
      ENDIF

C     Move unpacked values in from user array
      ZNFELDI(1:INLEN) = FLDIN(1:INLEN)
      IF (NISCNM.EQ.64) THEN
        CALL INTLOG(JP_DEBUG,
     X    'HNTFAUH: Scanning flag west-east/south-north',NISCNM)
        ITEMP = NIAREA(1)
        NIAREA(1) = NIAREA(3)
        NIAREA(3) = ITEMP

        K=0
        DO J=1,NINS/2
          ROW(1:NIWE) = ZNFELDI(K+1:K+NIWE)
          ZNFELDI(K+1:K+NIWE) = ZNFELDI(INLEN-NIWE-K+1:INLEN-K)
          ZNFELDI(INLEN-NIWE-K+1:INLEN-K) = ROW(1:NIWE)
          K = K+NIWE
        END DO
      ENDIF
C
      LLATOUT = (NOREPR.EQ.JPREGROT).OR.(NOREPR.EQ.JPREGULAR)

      IF( .NOT.LNOROTA ) GOTO 900
C
C     -----------------------------------------------------------------|
C*    Section 3.   Handle rotation, if necessary.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      CALL INTLOG(JP_DEBUG,'HNTFAUH: Rotate field.',JPQUIET)
      CALL INTLOG(JP_DEBUG,'HNTFAUH: South pole lat  ',NOROTA(1))
      CALL INTLOG(JP_DEBUG,'HNTFAUH: South pole long ',NOROTA(2))
C
C     Fill area limits (handles case when default 0/0/0/0 given)
C
      IRET = FIXAREA()
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'HNTFAUH: area fixup failed',JPQUIET)
        HNTFAUH = JPROUTINE + 3
        GOTO 900
      ENDIF
C
      AREA(1) = REAL(NOAREA(1))/PPMULT
      AREA(2) = REAL(NOAREA(2))/PPMULT
      AREA(3) = REAL(NOAREA(3))/PPMULT
      AREA(4) = REAL(NOAREA(4))/PPMULT
C
      GRID(1) = REAL(NOGRID(1))/PPMULT
      GRID(2) = REAL(NOGRID(2))/PPMULT
C
      POLE(1) = REAL(NOROTA(1))/PPMULT
      POLE(2) = REAL(NOROTA(2))/PPMULT
C
C     -----------------------------------------------------------------|
C*    Section 4.   Spectral to rotated grid-point
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
      LSP2RGG = .FALSE.
      IF( (NIREPR.EQ.JPSPHERE).OR.(NIREPR.EQ.JPSPHROT) ) THEN
C
C       Convert spectral to suitable global reduced gaussian grid
C
        CALL INTLOG(JP_DEBUG,
     X    'HNTFAUH: Spectral to suitable reduced gaussian',JPQUIET)
C
        NTRUNC = NIRESO

        IF( LNORESO ) THEN
          NTRUNC = NORESO
          NGAUSS = 0
          HTYPE  = ''
          NS = 0.
          EW = 0.
          IRET = HSH2GG(NS,EW,NTRUNC,NGAUSS,HTYPE,KPTS,PLATS,ISIZE)
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'HNTFAUH: problem getting data for reduced grid',NTRUNC)
            HNTFAUH = JPROUTINE + 4
            GOTO 900
          ENDIF
          GOTO 401
        ENDIF

        IF( LARESOL ) THEN
          NS = 0.
          EW = 0.
          IF( LLATOUT ) THEN
            NS = GRID(1)
            EW = GRID(2)
            NTRUNC = 0
            NGAUSS = 0
            HTYPE = ''
          ELSE
            HTYPE = 'R'
          ENDIF
          IRET = HSH2GG(NS,EW,NTRUNC,NGAUSS,HTYPE,KPTS,PLATS,ISIZE)
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'HNTFAUH: problem getting data for reduced grid',NTRUNC)
            HNTFAUH = JPROUTINE + 4
            GOTO 900
          ENDIF
        ENDIF

C       Truncate if a smaller resolution has been requested
C
  401 CONTINUE
      IF( NTRUNC.LT.NIRESO ) THEN
       CALL INTLOG(JP_DEBUG,'HNTFAUH: Truncation changed from:',NIRESO)
       CALL INTLOG(JP_DEBUG,'HNTFAUH: to: ',NTRUNC)
       CALL INTLOG(JP_DEBUG,'HNTFAUH: Gaussian number is:',NGAUSS)
C
        ISHIZE =  (NTRUNC+1)*(NTRUNC+4)
        CALL JMEMHAN( 3, IZNFLDO, ISHIZE, 1, IERR)
        IF( IERR.NE.0 ) THEN
          CALL INTLOG(JP_FATAL,
     X      'HNTFAUH: Get scratch space failed',JPQUIET)
          HNTFAUH = JPROUTINE + 4
          GOTO 900
        ENDIF

C       Generate spherical harmonics with output truncation
        CALL SH2SH( ZNFELDI, NIRESO, ZNFLDO, NTRUNC )

C       Move new spherical harmonics to 'input' array
        ZNFELDI(1:ISHIZE) = ZNFLDO(1:ISHIZE)

      ELSE
          CALL INTLOG(JP_DEBUG,
     X      'HNTFAUH: Spectral to suitable reduced gaussian',JPQUIET)
C
          NTRUNC = NIRESO
          NGAUSS = 0
          HTYPE  = ''
          NS = 0.
          EW = 0.
          IRET = HSH2GG(NS,EW,NTRUNC,NGAUSS,HTYPE,KPTS,PLATS,ISIZE)
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'HNTFAUH: problem getting data for reduced grid',NTRUNC)
            HNTFAUH = JPROUTINE + 4
            GOTO 900
          ENDIF

        ENDIF

C
C       Dynamically allocate memory for global reduced gaussian grid
C
        CALL JMEMHAN( 18, IRGGRID, ISIZE, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'HNTFAUH: memory alloc for reduced grid fail',JPQUIET)
          HNTFAUH = JPROUTINE + 4
          GOTO 900
        ENDIF

C       Set flag to show field is not a wind component
        NUVFLAG = 0

C       Create the reduced gaussian grid
        HOLDTYP = HOGAUST
        WEST = 0.0
        EAST = 360.0 - (360.0/(NGAUSS*4))
c       EMOS-199: adjusted for reduced_gg/octahedral
        IF (HTYPE.EQ.'O')  EAST = 360.0 - (360.0/FLOAT(KPTS(NGAUSS)))
        IF (HTYPE.NE.'R' .AND. HTYPE.NE.'O' .AND.
     X      HTYPE.NE.'F' .AND. HTYPE.NE.'U') THEN
          HTYPE = 'R'
        ENDIF
        CALL JAGGGP(ZNFELDI,NTRUNC,PLATS(1),PLATS(NGAUSS*2),WEST,
     X              EAST,NGAUSS,HTYPE,KPTS,RGGRID,NUVFLAG,IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'HNTFAUH: spectral to reduced gaussian failed',JPQUIET)
          HNTFAUH = JPROUTINE + 4
          GOTO 900
        ENDIF
        HOGAUST = HOLDTYP

        NCOUNT = 0
        DO LOOP = 1, (NGAUSS*2)
          NCOUNT= NCOUNT + KPTS(LOOP)
        ENDDO

        LSP2RGG = .TRUE.

        IF( NOREPR.EQ.JPFGGROT ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'HNTFAUH: Convert gaussian to rotated gaussian',JPQUIET)
          GOTO 600
        ENDIF

      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 5.   Gaussian to rotated lat/long
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
      IF( (NOREPR.EQ.JPREGROT) .AND. (
     X        (NIREPR.EQ.JPQUASI) .OR.
     X        (NIREPR.EQ.JPGAUSSIAN) .OR.
     X        (LSP2RGG) )) THEN
C
        CALL INTLOG(JP_DEBUG,'HNTFAUH: Gauss to lat/lon',JPQUIET)
C
C       Dynamically allocate memory for rotated lat/long grid
C
        NLON = 1 + NINT(FLOAT((NOAREA(JPEAST)  - NOAREA(JPWEST))) /
     X         NOGRID(JPWESTEP))
        NLAT = 1 + NINT(FLOAT((NOAREA(JPNORTH) - NOAREA(JPSOUTH))) /
     X         NOGRID(JPNSSTEP))
C
        NUMPTS = NLON * NLAT
        ISIZE  = NUMPTS
        CALL JMEMHAN( 11, ISWORK, ISIZE, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'HNTFAUH: memory alloc for lat/long grid fail',JPQUIET)
          HNTFAUH = JPROUTINE + 5
          GOTO 900
        ENDIF
C
        LUSELSM = LSMFLD()
C
C       If original field was spectral, ...
C
        IF( LSP2RGG ) THEN
          IF( LUSELSM ) THEN
            IRET = HIRLSM(LO12PT,RGGRID,NCOUNT,NGAUSS,HTYPE,AREA,POLE,
     X                    GRID,SWORK,ISIZE,NLON,NLAT)
          ELSE
            IRET = HIRLAM(LO12PT,RGGRID,NCOUNT,NGAUSS,HTYPE,AREA,POLE,
     X                    GRID,SWORK,ISIZE,NLON,NLAT)
          ENDIF
C
        ELSE
C
C       If original field was gaussian, ...
C
          IRET = PDDEFS()
          NGAUSS = NIGAUSS
          HTYPE  = GGHTYPE(NIREPR,NIGAUSS,MILLEN)
cs        IF( LUSELSM ) THEN
          IF( LSM ) THEN
            IRET = HIRLSM(LO12PT,ZNFELDI,NCOUNT,NGAUSS,HTYPE,AREA,POLE,
     X                    GRID,SWORK,ISIZE,NLON,NLAT)
          ELSE
            IRET = HIRLAM(LO12PT,ZNFELDI,NCOUNT,NGAUSS,HTYPE,AREA,POLE,
     X                    GRID,SWORK,ISIZE,NLON,NLAT)
          ENDIF
C
        ENDIF
C
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'HNTFAUH: HIRLAM rotation failed',JPQUIET)
          HNTFAUH = JPROUTINE + 5
          GOTO 900
        ENDIF
C
cs      setting size of output field
        OUTLROT = NLON * NLAT
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 6.   Gaussian to rotated gaussian
C     -----------------------------------------------------------------|
C
  600 CONTINUE
C
      IF( (LSP2RGG.AND.(NOREPR.EQ.JPFGGROT)) .OR.
     X      (((NIREPR.EQ.JPQUASI).OR.(NIREPR.EQ.JPGAUSSIAN)) .AND.
     X     (NOREPR.EQ.JPFGGROT)) ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'HNTFAUH: Gaussian to reduced gaussian',JPQUIET)
C
C       Dynamically allocate memory for rotated lat/long grid
C
C       ISIZE = NOGAUSS * NOGAUSS * 8
        ISIZE = (2*NOGAUSS) * (4*NOGAUSS + 20)  ! account for RGG/octahedral
        CALL JMEMHAN( 11, ISWORK, ISIZE, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'HNTFAUH: memory alloc for gaussian grid fail',JPQUIET)
          HNTFAUH = JPROUTINE + 6
          GOTO 900
        ENDIF
C
C       If original field was spectral, ...
C
        IF( LSP2RGG ) THEN
          IRET = HRG2GG(LO12PT,RGGRID,NGAUSS,AREA,POLE,
     X                  NOGAUSS,HOGAUST,SWORK,ISIZE,NUMPTS)
C
        ELSE
C
C       If original field was gaussian, ...
C
          NGAUSS = NIGAUSS
          IRET = HRG2GG(LO12PT,ZNFELDI,NGAUSS,AREA,POLE,
     X                  NOGAUSS,HOGAUST,SWORK,ISIZE,NUMPTS)
        ENDIF
cs      setting size of output field
        OUTLROT = NUMPTS
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'HNTFAUH: HRG2GG rotation failed',JPQUIET)
          HNTFAUH = JPROUTINE + 6
          GOTO 900
        ENDIF
C
      ENDIF

C     move rotated field back into field original array
      ZNFELDI(1:NUMPTS) = SWORK(1:NUMPTS)

C     -----------------------------------------------------------------|
C*    Section 9.   Closedown.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
