C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INTUVXH(PUVSH,KNVALS,ZNFLDO,KUGRIB,KVGRIB,
     X                         OUTLENU,OUTLENV)
C
C---->
C**** INTUVXH
C
C     Purpose
C     -------
C
C     Interpolate U or V component spectral fields to grid point.
C
C
C     Interface
C     ---------
C
C     IRET = INTUVXH(PUVSH,KNVALS,ZNFLDO,KUGRIB,KVGRIB,OUTLENU,OUTLENV)
C
C     Input
C     -----
C
C     PUVSH  - Spectral U/V values. (U first)
C     KNVALS - Number of values in each wind component field.
C     ZNFLDO - Work array.
C
C
C     Output
C     ------
C
C     KUGRIB  - Output wind U component field (GRIB format).
C     KVGRIB  - Output wind V component field (GRIB format).
C     OUTLENU - Output U field length (words).
C     OUTLENV - Output V field length (words).
C
C
C     Method
C     ------
C
C     None.
C
C
C     Externals
C     ---------
C
C     INTUVDH - Encode/decode data into/from GRIB code.
C     INTFAU  - Prepare to interpolate unpacked input field.
C     INTFBU  - Interpolate unpacked input field.
C     INTLOG  - Log error message.
C     MKFRAME - Create a 'frame' from a rectangular field.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     February 2001
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
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 40170 )
C
C     Function arguments
C
      INTEGER KNVALS, KUGRIB(*), KVGRIB(*), OUTLENU, OUTLENV
      REAL PUVSH(KNVALS*2), ZNFLDO(*)
C
C     Local variables
C
      CHARACTER*1 HOLDTYP
      CHARACTER*1 HTYPE
      INTEGER ILENF
      INTEGER IRET
      INTEGER ISIZE
      INTEGER NCOUNT
      INTEGER NGAUSS
      INTEGER NLAT
      INTEGER NLON
      INTEGER NTRUNC
      INTEGER NUMPTS
      INTEGER NUVFLAG
      LOGICAL LFRAME
      LOGICAL LOLDWIND
      REAL AREA(4)
      REAL EAST
      REAL EW
      REAL GRID(2)
      REAL NS
      REAL POLE(2)
      REAL WEST
C
      REAL RGGRID, SWORK
      POINTER (IRGGRID, RGGRID(1) )
      POINTER (ISWORK, SWORK(1) )
C
      INTEGER KPTS(JPGTRUNC*2)
      REAL GLATS(JPGTRUNC*2)
C
      DATA IRGGRID/0/, ISWORK/0/
      SAVE IRGGRID, ISWORK
C
C     Externals
C
      INTEGER INTFAU, INTFBU, INTUVDH, HSH2GG !, FIXAREA
      INTEGER HIRLAMW
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      INTUVXH = 0
C
      LFRAME = LNOFRAME.AND.
     X         ((NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPGAUSSIAN).OR.
     X          (NOREPR.EQ.JPREGROT ).OR.(NOREPR.EQ.JPFGGROT  ) )
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
     X  'INTUVXH: Interoplate U & V fields with no rotation',JPQUIET)
C
C     Unpack and interpolate U field
C
      NIFORM = 0
      NIPARAM = JP_U
      LWIND = .TRUE.
      LOLDWIND = LWINDSET
      LWINDSET = .TRUE.
C
      IRET = INTFAU( PUVSH, KNVALS)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVXH: Prepare to interpolate failed.',JPQUIET)
        INTUVXH = JPROUTINE + 2
        GOTO 900
      ENDIF
C
      IRET = INTFBU( PUVSH, KNVALS, ZNFLDO, ILENF)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTUVXH: Interpolation failed.',JPQUIET)
        INTUVXH = JPROUTINE + 2
        GOTO 900
      ENDIF
C
C     Unpack and interpolate V field
C
      NIPARAM = JP_V
C
      IRET = INTFAU( PUVSH(1+KNVALS), KNVALS)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVXH: Prepare to interpolate failed.',JPQUIET)
        INTUVXH = JPROUTINE + 2
        GOTO 900
      ENDIF
C
      IRET = INTFBU( PUVSH(1+KNVALS), KNVALS, ZNFLDO(1+ILENF), ILENF)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTUVXH: Interpolation failed.',JPQUIET)
        INTUVXH = JPROUTINE + 2
        GOTO 900
      ENDIF
C
C     Reset the input format flag
C
      NIFORM = 1
C
C     Get some scratch memory for the U fields before interpolation
C     (to prevent overwriting V in ZNFLDO during interpolation)
C
cs      ISIZE  = ILENF
cs      CALL JMEMHAN( 11, ISWORK, ISIZE, 1, IRET)
cs      IF( IRET.NE.0 ) THEN
cs        CALL INTLOG(JP_ERROR,
cs     X    'INTUVXH: memory alloc for scratch memory failed',JPQUIET)
cs        INTUVXH = JPROUTINE + 2
cs        GOTO 900
cs      ENDIF
C
      LWINDSET = LOLDWIND
C
C
C     If a 'frame' has been specified, build the frame
C
      IF( LFRAME ) THEN
        LIMISSV = .TRUE.
        NLON = 1 + NINT(FLOAT(NOAREA(JPEAST)  - NOAREA(JPWEST)) /
     X       NOGRID(JPWESTEP))
        NLAT = 1 + NINT(FLOAT(NOAREA(JPNORTH) - NOAREA(JPSOUTH)) /
     X       NOGRID(JPNSSTEP))
        CALL MKFRAME(NLON,NLAT,ZNFLDO,RMISSGV,NOFRAME)
        CALL MKFRAME(NLON,NLAT,ZNFLDO(1+ILENF),RMISSGV,NOFRAME)
      ENDIF

C     Code data into GRIB
cs      DO LOOP = 1, ILENF
cs        SWORK(LOOP) = ZNFLDO(LOOP)
cs      ENDDO
C
cs      IRET = INTUVDH(SWORK,ILENF,KUGRIB,OUTLENU,'C',JP_U)
      IRET = INTUVDH(ZNFLDO,ILENF,KUGRIB,OUTLENU,'C',JP_U)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVXH: Wind component into GRIB encoding fail',IRET)
        INTUVXH = JPROUTINE + 2
        GOTO 900
      ENDIF
C
      IRET = INTUVDH(ZNFLDO(1+ILENF),ILENF,KVGRIB,OUTLENV,'C',JP_V)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVXH: Wind component into GRIB encoding fail',IRET)
        INTUVXH = JPROUTINE + 2
        GOTO 900
      ENDIF
C
      GOTO 900
C
C     -----------------------------------------------------------------|
C*    Section 3.   Initialise spectral to grid-point with rotation
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      IF( .NOT.LUSEHIR ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVXH : Unable to rotate spectral U or V:',JPQUIET)
        INTUVXH  = JPROUTINE + 3
        GOTO 900
      ENDIF
C
      IF( (NOREPR.NE.JPREGROT).AND.(NOREPR.NE.JPREGULAR) ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVXH : For U/V, only regular lat/long',JPQUIET)
        CALL INTLOG(JP_ERROR,
     X    'INTUVXH : output rotated grids allowed',JPQUIET)
        INTUVXH  = JPROUTINE + 3
        GOTO 900
      ENDIF
C
      CALL INTLOG(JP_DEBUG,'INTUVXH: Rotate the U & V fields',JPQUIET)
      CALL INTLOG(JP_DEBUG,'INTUVXH: South pole lat  ',NOROTA(1))
      CALL INTLOG(JP_DEBUG,'INTUVXH: South pole long ',NOROTA(2))
C
C     Fill area limits (handles case when default 0/0/0/0 given)
C
cs      IRET = FIXAREA()
cs      IF( IRET.NE.0 ) THEN
cs        CALL INTLOG(JP_ERROR,'INTUVXH: area fixup failed',JPQUIET)
cs        INTUVXH = JPROUTINE + 3
cs        GOTO 900
cs      ENDIF
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
C*    Section 4.   Convert spectral to suitable global reduced gaussian
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
      NTRUNC = NIRESO
      NGAUSS = 0
      HTYPE  = ''
      NS = 0.
      EW = 0.
      IRET = HSH2GG(NS,EW,NTRUNC,NGAUSS,HTYPE,KPTS,GLATS,ISIZE)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVXH: problem getting data for reduced grid',NTRUNC)
        INTUVXH = JPROUTINE + 4
        GOTO 900
      ENDIF
      NCOUNT = ISIZE
C
C     Dynamically allocate memory for global reduced gaussian grid
C
      CALL JMEMHAN( 18, IRGGRID, (NCOUNT*2), 1, IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVXH: memory alloc for reduced grid fail',JPQUIET)
        INTUVXH = JPROUTINE + 4
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
C     U component
C
      CALL JAGGGP(PUVSH,NTRUNC,GLATS(1),GLATS(NGAUSS*2),WEST,
     X            EAST,NGAUSS,HTYPE,KPTS,RGGRID,NUVFLAG,IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVXH: spectral to reduced gaussian failed',JPQUIET)
        INTUVXH = JPROUTINE + 4
        GOTO 900
      ENDIF
C
      HOGAUST = HOLDTYP
C
C     V component
C
      CALL JAGGGP(PUVSH(1+KNVALS),NTRUNC,GLATS(1),GLATS(NGAUSS*2),WEST,
     X            EAST,NGAUSS,HTYPE,KPTS,RGGRID(1+NCOUNT),NUVFLAG,IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVXH: spectral to reduced gaussian failed',JPQUIET)
        INTUVXH = JPROUTINE + 4
        GOTO 900
      ENDIF
C
      HOGAUST = HOLDTYP
C
C     -----------------------------------------------------------------|
C*    Section 5.   Rotate using 12-point horizontal interpolation
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
C     Dynamically allocate memory for rotated lat/long grid
C
      NLON = 1 + NINT(FLOAT(NOAREA(JPEAST)  - NOAREA(JPWEST)) /
     X       NOGRID(JPWESTEP))
      NLAT = 1 + NINT(FLOAT(NOAREA(JPNORTH) - NOAREA(JPSOUTH)) /
     X       NOGRID(JPNSSTEP))
C
      NUMPTS = NLON * NLAT
      ISIZE  = NUMPTS * 2
      CALL JMEMHAN( 11, ISWORK, ISIZE, 1, IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVXH: memory alloc for lat/long grid fail',JPQUIET)
        INTUVXH = JPROUTINE + 5
        GOTO 900
      ENDIF
C
      IRET = HIRLAMW(LO12PT,RGGRID,RGGRID(1+NCOUNT),NCOUNT,NGAUSS,HTYPE,
     X               AREA,POLE,GRID,SWORK,SWORK(1+NUMPTS),NUMPTS,NLON,
     X               NLAT)
C
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVXH: HIRLAMW rotation failed',JPQUIET)
        INTUVXH = JPROUTINE + 5
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 6.   Pack the fields into GRIB format
C     -----------------------------------------------------------------|
C
  600 CONTINUE
C
C     Reset the input format flag
C
      NIFORM = 1
C
C     Set the components flag for rotated U and V coefficients
C
      ISEC2(19) = 8
C
C     If a 'frame' has been specified, build the frame
C
      IF( LFRAME ) THEN
        LIMISSV = .TRUE.
        CALL MKFRAME(NLON,NLAT,SWORK,RMISSGV,NOFRAME)
        CALL MKFRAME(NLON,NLAT,SWORK(1+NUMPTS),RMISSGV,NOFRAME)
      ENDIF
C
      IRET = INTUVDH(SWORK,NUMPTS,KUGRIB,OUTLENU,'C',JP_U)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVXH: Wind component into GRIB encoding fail',IRET)
        INTUVXH = JPROUTINE + 6
        GOTO 900
      ENDIF
C
      IRET = INTUVDH(SWORK(1+NUMPTS),NUMPTS,KVGRIB,OUTLENV,'C',JP_V)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVXH: Wind component into GRIB encoding fail',IRET)
        INTUVXH = JPROUTINE + 6
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9.   Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
