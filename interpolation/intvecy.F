C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INTVECY(PUFIELD,PVFIELD,KASIZE,
     X                         PUOUT,PVOUT,OUTLEN)
C
C---->
C**** INTVECY
C
C     Purpose
C     -------
C     Interpolate U or V component fields to grid point.
C
C
C     Interface
C     ---------
C     IRET = INTVECY(PUFIELD,PVFIELD,KASIZE,
C    X               KUGRIB,KVGRIB,OUTLEN)
C
C     Input
C     -----
C     PUFIELD - U values.
C     PVFIELD - V values.
C     KASIZE  - Size of input arrays
C
C
C     Output
C     ------
C     PUOUT - Output wind U component field
C     PVOUT - Output wind V component field
C     OUTLEN - Output U field length (words).
C
C
C     Method
C     ------
C     None.
C
C
C     Externals
C     ---------
C     RESET_C - Reset interpolation handling options using GRIB product.
C     IBASINI - Ensure basic interpolation setup is done.
C     INTFAU  - Prepare to interpolate unpacked input field.
C     INTFBU  - Interpolate unpacked input field.
C     INTLOG  - Log error message.
C     MKFRAME - Create a 'frame' from a rectangular field.
C     INTUVGH - Interpolate GRIB U and V reduced gaussian to rotated GRIB
C               U and V.
C
C
C     Author
C     ------
C     S.Curic     ECMWF     December 2006
C
C
C----<
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
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
      PARAMETER (JPROUTINE = 40170)
C
C     Function arguments
C
      INTEGER KASIZE, OUTLEN
      REAL PUFIELD(KASIZE), PVFIELD(KASIZE), PUOUT(*), PVOUT(*)
C
C     Local variables
C
      CHARACTER*1 HOLDTYP
      CHARACTER*1 HTYPE
      CHARACTER*3 EXTRA
      INTEGER IRET
      INTEGER ISAME
      INTEGER ISIZE
      INTEGER KPTS(JPGTRUNC*2)
      INTEGER LOOP
      INTEGER NCOUNT
      INTEGER NGAUSS
      INTEGER NLAT
      INTEGER NLON
      INTEGER NSIZE
      INTEGER NTRUNC
      INTEGER NUMPTS
      INTEGER NUVFLAG
      LOGICAL LFIRST
      LOGICAL LFRAME
      LOGICAL LNEWUV
      LOGICAL LOLDWIND
      LOGICAL LOMISSV
      LOGICAL LSPECUV
      REAL AREA(4)
      REAL EAST
      REAL EW
      REAL GLATS(JPGTRUNC*2)
      REAL GRID(2)
      REAL NS
      REAL OLDGRID(2)
      REAL POLE(2)
      REAL RGGRID
      REAL SWORK
      REAL TWORK
      REAL WEST
cc    INTEGER IN_U
cc    INTEGER IN_V
      POINTER (IRGGRID,RGGRID(1))
      POINTER (ISWORK,SWORK(1))
      POINTER (ITWORK,TWORK(1))
C
C     Externals
C
      CHARACTER*1 GGHTYPE
      INTEGER FIXAREA, INTFAU, INTFBU, HSH2GG, IBASINI
      INTEGER HIRLAMW, INSANE !, RESET_C
      INTEGER HRG2GGW, HLL2LLW
      EXTERNAL GGHTYPE
      EXTERNAL FIXAREA, INTFAU, INTFBU, HSH2GG, IBASINI
      EXTERNAL HIRLAMW, INSANE !, RESET_C
      EXTERNAL HRG2GGW, HLL2LLW
C
      DATA IRGGRID/0/, ISWORK/0/, ITWORK/0/
      SAVE IRGGRID, ISWORK, ITWORK
      DATA LFIRST/.TRUE./, LNEWUV/.TRUE./, EXTRA/'NO '/
      SAVE LFIRST, LNEWUV
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      INTVECY = 0
      IF( LFIRST ) THEN
        CALL GETENV('IGNORE_UV_EXTRA_MODE', EXTRA)
        IF((EXTRA(1:1).EQ.'Y').OR.(EXTRA(1:1).EQ.'y')) LNEWUV = .FALSE.
        IF( LNEWUV ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'INTVECY: IGNORE_UV_EXTRA_MODE not turned on',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,
     X      'INTVECY: IGNORE_UV_EXTRA_MODE turned on',JPQUIET)
        ENDIF
        LFIRST = .FALSE.
      ENDIF
C
      LOLDWIND = LWINDSET
C
C     Ensure that basic initialisation has been done
C
      IRET = IBASINI(0)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTVECY: basic initialise failed',JPQUIET)
        INTVECY = IRET
        GOTO 900
      ENDIF
C
C
cs      NSIZE = ISEC4(1)
      NSIZE = KASIZE
cs      NIREPR = ISEC2(1)
      LSPECUV = (NIREPR.EQ.JPSPHERE).OR.(NIREPR.EQ.JPSPHROT)
cs      IF( LSPECUV ) NIRESO = ISEC2(2)
C
C     Setup interpolation options from input GRIB characteristics.
C
cs      IRET = RESET_C(ISEC1, ISEC2, ZSEC2, ISEC4)
cs      IF( IRET.NE.0 ) THEN
cs        CALL INTLOG(JP_ERROR,
cs     X    'INTVECY: Setup interp. options from GRIB failed.',JPQUIET)
cs        INTVECY = IRET
cs        GOTO 900
cs      ENDIF
C
C       Only allowed rotations are:
C        - spectral to regular lat/long
C        - reduced gaussian to regular lat/long
C
      IF( LNOROTA ) THEN
        IF( ((NOREPR.NE.JPREGROT).AND.(NOREPR.NE.JPREGULAR)).OR.
     X      ((NIREPR.NE.JPSPHERE).AND.(NIREPR.NE.JPQUASI)) ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTVECY: For vector fields, only allowed rotations are:',
     X      JPQUIET)
          CALL INTLOG(JP_ERROR,
     X      'INTVECY: spectral to regular lat/long, or',JPQUIET)
          CALL INTLOG(JP_ERROR,
     X      'INTVECY: reduced gaussian to regular lat/long',JPQUIET)
          INTVECY = JPROUTINE + 1
          GOTO 900
        ENDIF
      ENDIF
C
C     Check that no outrageous values given for interpolation
C
      ISAME = INSANE()
      IF( (ISAME.GT.0).AND.(ISAME.NE.27261) ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTVECY: Interpolation cannot use given values.',JPQUIET)
        INTVECY = ISAME
        GOTO 900
      ENDIF
C
      LFRAME = LNOFRAME.AND.
     X         ((NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPGAUSSIAN).OR.
     X          (NOREPR.EQ.JPREGROT ).OR.(NOREPR.EQ.JPFGGROT  ) )
C
C     Fill area limits (handles case when default 0/0/0/0 given)
C
      IRET = FIXAREA()
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTVECY: area fixup failed',JPQUIET)
        INTVECY = JPROUTINE + 3
        GOTO 900
      ENDIF
C
      IF( .NOT. LNOROTA ) THEN
        NOROTA(1) = -9000000
        NOROTA(2) = 0
      ENDIF
C
      CALL INTLOG(JP_DEBUG,'INTVECY: Rotate the U & V fields',JPQUIET)
      CALL INTLOG(JP_DEBUG,'INTVECY: South pole lat  ',NOROTA(1))
      CALL INTLOG(JP_DEBUG,'INTVECY: South pole long ',NOROTA(2))
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
      IF( .NOT. LSPECUV ) GOTO 500
C
C     Get some scratch memory for the U and V fields
C
C     Unpack U field
C
      NIFORM = 0
cs      NIPARAM = IN_U
      LWIND = .TRUE.
      LOLDWIND = LWINDSET
      LWINDSET = .TRUE.
C
cs      IN_U = ISEC1(6)
C
C     Unpack V field
C
cs      NIPARAM = IN_V
cs      IN_V = ISEC1(6)
C
      IF( LNOROTA ) GOTO 300
C
C     -----------------------------------------------------------------|
C*    Section 2.   Spectral to grid-point with no rotation
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      CALL INTLOG(JP_DEBUG,
     X  'INTVECY: Interpolate U & V fields with no rotation',JPQUIET)
C
C
C     Interpolate U field
C
      IRET = INTFAU(PUFIELD, KASIZE)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTVECY: Prepare to interpolate failed.',JPQUIET)
        INTVECY = JPROUTINE + 2
        GOTO 900
      ENDIF
C
      IRET = INTFBU(PUFIELD, KASIZE, PUOUT, OUTLEN)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTVECY: Interpolation failed.',JPQUIET)
        INTVECY = JPROUTINE + 2
        GOTO 900
      ENDIF
C
C     Interpolate V field
C
      IRET = INTFAU(PVFIELD, KASIZE)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTVECY: Prepare to interpolate failed.',JPQUIET)
        INTVECY = JPROUTINE + 2
        GOTO 900
      ENDIF
C
      IRET = INTFBU(PVFIELD, KASIZE, PVOUT, OUTLEN)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTVECY: Interpolation failed.',JPQUIET)
        INTVECY = JPROUTINE + 2
        GOTO 900
      ENDIF
C
      GOTO 700
C
C     -----------------------------------------------------------------|
C*    Section 3.   Spectral to grid-point with rotation
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      IF( .NOT.LUSEHIR ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTVECY: Unable to rotate spectral U or V:',JPQUIET)
        INTVECY  = JPROUTINE + 3
        GOTO 900
      ENDIF
C
C     Convert spectral to suitable global reduced gaussian
C
      NTRUNC = NIRESO
      NGAUSS = 0
      HTYPE  = ''
      NS = 0.
      EW = 0.
      IRET = HSH2GG(NS,EW,NTRUNC,NGAUSS,HTYPE,KPTS,GLATS,ISIZE)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTVECY: problem getting data for reduced grid',NTRUNC)
        INTVECY = JPROUTINE + 4
        GOTO 900
      ENDIF
C
      NCOUNT = ISIZE
C
C     Dynamically allocate memory for global reduced gaussian grid
C
      CALL JMEMHAN( 18, IRGGRID, (NCOUNT*2), 1, IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTVECY: memory alloc for reduced grid fail',JPQUIET)
        INTVECY = JPROUTINE + 4
        GOTO 900
      ENDIF
C
C     Set flag to show field is a wind component
C
      NUVFLAG = 1
C
C     Create the reduced gaussian grid
C
      HOLDTYP = HOGAUST
      WEST = 0.0
      EAST = 360.0 - (360.0/FLOAT(KPTS(NGAUSS)))
C
C     U component spectral -> reduced gaussian
C
      CALL JAGGGP(PUFIELD,NTRUNC,GLATS(1),GLATS(NGAUSS*2),WEST,
     X            EAST,NGAUSS,HTYPE,KPTS,RGGRID,NUVFLAG,IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTVECY: spectral to reduced gaussian failed',JPQUIET)
        INTVECY = JPROUTINE + 4
        GOTO 900
      ENDIF
C
      HOGAUST = HOLDTYP
C
C     V component spectral -> reduced gaussian
C
      CALL JAGGGP(PVFIELD,NTRUNC,GLATS(1),GLATS(NGAUSS*2),WEST,
     X            EAST,NGAUSS,HTYPE,KPTS,RGGRID(1+NCOUNT),NUVFLAG,IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTVECY: spectral to reduced gaussian failed',JPQUIET)
        INTVECY = JPROUTINE + 4
        GOTO 900
      ENDIF
C
      HOGAUST = HOLDTYP
C
C     Rotate using 12-point horizontal interpolation
C
C     Dynamically allocate memory for rotated lat/long grid
C
      NLON = 1 + NINT((AREA(JPEAST)  - AREA(JPWEST)) /
     X       GRID(JPWESTEP))                  ! SC
      NLAT = 1 + NINT((AREA(JPNORTH) - AREA(JPSOUTH)) /
     X       GRID(JPNSSTEP))                  ! SC
C
      OUTLEN = NLON * NLAT
C
      IRET = HIRLAMW(LO12PT,RGGRID,RGGRID(1+NCOUNT),NCOUNT,NGAUSS,HTYPE,
     X  AREA,POLE,GRID,PUOUT,PVOUT,OUTLEN,NLON,NLAT)
C
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTVECY: HIRLAMW rotation failed',JPQUIET)
        INTVECY = JPROUTINE + 6
        GOTO 900
      ENDIF
C
C     Set the components flag for rotated U and V coefficients
C
cs      ISEC2(19) = 8
      NOWE = NLON
      NONS = NLAT
      OUTLEN = NLAT*NLON

C
      GOTO 700
C
C     -----------------------------------------------------------------|
C*    Section 5.   Grid to grid  point
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C*    5a.   Generate interpolated lat/long U and V fields.
C     -----------------------------------------------------------------|
C
C
      IF( (NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPREGROT) ) THEN
C
C       Dynamically allocate scrath space for rotated lat/long grid
C
        NLON = 1 + NINT(FLOAT(NOAREA(JPEAST)  - NOAREA(JPWEST)) /
     X         NOGRID(JPWESTEP))
        NLAT = 1 + NINT(FLOAT(NOAREA(JPNORTH) - NOAREA(JPSOUTH)) /
     X         NOGRID(JPNSSTEP))
C
        NOWE = NLON
        NONS = NLAT
        OUTLEN = NLON * NLAT
C
C       Rotate reduced gaussian to lat/long
C
        IF( (NIREPR.EQ.JPGAUSSIAN).OR.(NIREPR.EQ.JPQUASI) ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'INTVECY: Rotate reduced gaussian to lat/long',JPQUIET)
          HTYPE = GGHTYPE(NIREPR,NIGAUSS,MILLEN)
          IRET = HIRLAMW(LO12PT,
     X                   PUFIELD,PVFIELD,KASIZE,
     X                   NIGAUSS,HTYPE,AREA,POLE,GRID,
     X                   PUOUT,PVOUT,OUTLEN,NLON,NLAT)
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTVECY: HIRLAMW rotation failed',JPQUIET)
            INTVECY = JPROUTINE + 3
            GOTO 900
          ENDIF
C
C       Rotate lat/long to lat/long
C
        ELSE
        CALL INTLOG(JP_DEBUG,
     X    'INTVECY: Rotate lat/long to lat/long',JPQUIET)
          OLDGRID(1) = REAL(NIGRID(1)) / PPMULT
          OLDGRID(2) = REAL(NIGRID(2)) / PPMULT
          IRET = HLL2LLW(LO12PT,PUFIELD,PVFIELD,
     X                   OLDGRID,AREA,POLE,GRID,
     X                   PUOUT,PVOUT,OUTLEN,NLON,NLAT)
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTVECY: HLL2LLW rotation failed',JPQUIET)
            INTVECY = JPROUTINE + 3
            GOTO 900
          ENDIF
        ENDIF
C
      ELSE
C
C*    Section 5b.   Generate interpolated gaussian U and V fields.
C     -----------------------------------------------------------------|
C
  400   CONTINUE
C
        CALL INTLOG(JP_DEBUG,
     X    'INTVECY: Rotate gaussian to gaussian',JPQUIET)
C
C       Dynamically allocate memory for rotated gaussian grids
C
        NUMPTS = NOGAUSS * NOGAUSS
        OUTLEN = 2 * NUMPTS * 8
C
cs        NGAUSS = ISEC2(10)
        IRET = HRG2GGW(LO12PT,
     X                 PUFIELD,PVFIELD,KASIZE,
     X                 NIGAUSS,AREA,POLE,NOGAUSS,HOGAUST,
     X                 PUOUT,PVOUT,OUTLEN,NUMPTS)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X        'INTVECY: HRG2GGW rotation failed',JPQUIET)
            INTVECY = JPROUTINE + 4
            GOTO 900
          ENDIF
C
      ENDIF
C
C
  700 CONTINUE
C
C     Reset the input format flag
C
cs      NIFORM = 1
C
C     If a 'frame' has been specified, build the frame
C
      IF( LFRAME ) THEN
        LOMISSV = LIMISSV
        LIMISSV = .TRUE.
        ISEC1(5) = 192
        ISEC3(2) = NINT(RMISSGV)
        ZSEC3(2) = RMISSGV
        CALL MKFRAME(NLON,NLAT,PUOUT,RMISSGV,NOFRAME)
        CALL MKFRAME(NLON,NLAT,PVOUT,RMISSGV,NOFRAME)
        LIMISSV = LOMISSV
      ENDIF

      DO 910 LOOP = 1, 4
        NOAAPI(LOOP) = NOAREA(LOOP)
  910 CONTINUE
C
C     -----------------------------------------------------------------|
C*    Section 9.   Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
