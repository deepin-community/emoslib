C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INTUVGH(KUGRIBI, KVGRIBI, INLEN,
     X                         KUGRIBO, KVGRIBO,
     X                         OUTLENU, OUTLENV)
C
C---->
C**** INTUVGH
C
C     Purpose
C     -------
C
C     Interpolate GRIB format input U and V component reduced gaussian
C     fields to rotated GRIB format U and V fields.
C
C
C     Interface
C     ---------
C
C     IRET = INTUVGH(KUGRIBI,KVGRIBI,INLEN,KUGRIBO,KVGRIBO,OUTLENU,OUTLENV)
C
C     Input
C     -----
C
C     KUGRIBI - Input U component field (reduced gaussian, GRIB format).
C     KVGRIBI - Input V component field (reduced gaussian, GRIB format).
C     INLEN   - Input field length (words).
C
C
C     Output
C     ------
C
C     KUGRIBO - Output U field (GRIB format).
C     KVGRIBO - Output V field (GRIB format).
C     OUTLENU - Output U field length (words).
C     OUTLENV - Output V field length (words).
C
C
C     Method
C     ------
C
C     Use HIRLAM rotation scheme.
C
C     Note that a common block is used in intf.h to hold the U/V
C     fields before interpolation.
C
C     Externals
C     ---------
C
C     IBASINI - Ensure basic interpolation setup is done.
C     INTUVDH - Encodes/decodes data into/from GRIB code.
C     JMEMHAN - Allocate scratch memory.
C     FIXAREA - Fixup area definition to correspond to grid definitions
C     INTLOG  - Log error message.
C     MKFRAME - Create a 'frame' from a rectangular field.
C     RESET_C - Reset interpolation handling options using GRIB product.
C     INSANE  - Ensure no outrageous values given for interpolation.
C     HRG2GGW - Creates rotated gaussian U and V from reduced gaussian
C     HLL2LLW - Creates rotated lat/long U and V from lat/long
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     October 2001
C
C
C----<
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
C     Function arguments
C
      INTEGER KUGRIBI(*), KVGRIBI(*), INLEN
      INTEGER KUGRIBO(*), KVGRIBO(*), OUTLENU, OUTLENV
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "intf.h"
#include "intlog.h"
#include "current.h"
C
C     Parameters
C
      INTEGER JPROUTINE, JPALLOC, JPSCR4
      PARAMETER (JPROUTINE = 40170 )
      PARAMETER (JPALLOC = 1)
      PARAMETER (JPSCR4 = 4)
C
C     Local variables
C
      REAL OLDGRID(2), AREA(4), GRID(2), POLE(2)
      LOGICAL LOLDWIND, LFRAME
      INTEGER IRET, KPR, ISZOUV, ISIZE, ISAME, IPARAM
      INTEGER LOOP, NGAUSS
      INTEGER IPINU, IPINV, NUMPTS, NLON, NLAT, IN_U, IN_V
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IOUV, ISWORK
#endif
#endif
      REAL OLDUV, SWORK
      POINTER ( IOUV, OLDUV(1) )
      POINTER (ISWORK, SWORK(1) )
C
C     Externals
C
      INTEGER RESET_C, FIXAREA, IBASINI, INSANE, INTUVDH, HIRLAMW
      INTEGER HRG2GGW, HLL2LLW
      EXTERNAL RESET_C, FIXAREA, IBASINI, INSANE, INTUVDH, HIRLAMW
      EXTERNAL HRG2GGW, HLL2LLW
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
      IRET     = 0
      KPR      = 0
      LOLDWIND = .FALSE.
      INTUVGH  = 0
C
      LFRAME = LNOFRAME.AND.
     X         ((NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPREGROT))
C
C     Ensure that basic initialisation has been done
C
      IRET = IBASINI(0)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTUVGH: basic initialise failed',JPQUIET)
        INTUVGH = IRET
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2.   Unpack the U/V fields.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Need to establish input characteristics, so unpack GRIB sections 1
C     and 2.
C
      IPARAM = 0
      IPINU  = 1
      IRET = INTUVDH(OLDUV(IPINU),ISZOUV,KUGRIBI,INLEN,'I',IPARAM)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTUVGH: GRIB header decode failed',IRET)
        INTUVGH = IRET
        GOTO 900
      ENDIF
C
C     Check whether the input field is an ECMWF reduced gaussian or
C     lat/long U field
C
      IF( (ISEC1(1).NE.128).OR.
     X    ((ISEC1(6).NE.JP_U).AND.(ISEC1(6).NE.JP_10U)) ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVGH: Input is not ECMWF U field.',JPQUIET)
        INTUVGH = JPROUTINE + 2
        GOTO 900
      ENDIF
      IF( (ISEC2(1).NE.4).AND.
     X    ((ISEC2(17).NE.0) .OR.(ISEC2(1).NE.0)) ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVGH: Input not gaussian or lat/long.',JPQUIET)
        INTUVGH = JPROUTINE + 2
        GOTO 900
      ENDIF
C
C     Get scratch memory for U/V unpacked fields.
C     U/V memory areas are adjacent.
C
      IF( ISEC2(17).EQ.1 ) THEN
        ISZOUV  = 0
        DO LOOP = 1, ISEC2(3)
          ISZOUV = ISZOUV + ISEC2(22+LOOP)
        ENDDO
      ELSE
        ISZOUV = ISEC2(2) * ISEC2(3)
      ENDIF
C
      IPINU = 1
      IPINV = 1 + ISZOUV
      CALL JMEMHAN( JPSCR4, IOUV, ISZOUV*2, JPALLOC, IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVGH: Scratch memory type 4 allocation failed.',JPQUIET)
        INTUVGH = IRET
        GOTO 900
      ENDIF
C
C     Decode input U and V
C
      IRET = INTUVDH(OLDUV(IPINU),ISZOUV,KUGRIBI,INLEN,'D',IPARAM)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTUVGH: Vorticity decoding failed',IRET)
        INTUVGH = IRET
        GOTO 900
      ENDIF
      IN_U = ISEC1(6)
C
      IRET = INTUVDH(OLDUV(IPINV),ISZOUV,KVGRIBI,INLEN,'D',IPARAM)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTUVGH: Divergence decoding failed',IRET)
        INTUVGH = IRET
        GOTO 900
      ENDIF
      IN_V = ISEC1(6)
C
C     Setup interpolation options from input GRIB characteristics.
C
      IRET = RESET_C( ISEC1, ISEC2, ZSEC2, ISEC4)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVGH: Setup interp. options from GRIB failed.',JPQUIET)
        INTUVGH = IRET
        GOTO 900
      ENDIF
C
C     Check that no outrageous values given for interpolation
C
      ISAME = INSANE()
      IF( ISAME.GT.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVGH: Interpolation cannot use given values.',JPQUIET)
        INTUVGH = ISAME
        GOTO 900
      ENDIF
C
      IRET = FIXAREA()
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTUVGH: Fixarea failed.',JPQUIET)
        INTUVGH = IRET
        GOTO 900
      ENDIF
C
      LWIND = .TRUE.
      LOLDWIND = LWINDSET
      LWINDSET = .TRUE.
C
      DO LOOP = 1, 4
        AREA(LOOP) = REAL(NOAREA(LOOP))/PPMULT
      ENDDO
      DO LOOP = 1, 2
        GRID(LOOP) = REAL(NOGRID(LOOP))/PPMULT
        POLE(LOOP) = REAL(NOROTA(LOOP))/PPMULT
      ENDDO
C
C     -----------------------------------------------------------------|
C*    Section 3.   Generate interpolated lat/long U and V fields.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
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
        NUMPTS = NLON * NLAT
        ISIZE  = NUMPTS * 2
        CALL JMEMHAN( 11, ISWORK, ISIZE, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTUVGH: memory alloc for lat/long grid fail',JPQUIET)
          INTUVGH = JPROUTINE + 3
          GOTO 900
        ENDIF
C
C       Rotate reduced gaussian to lat/long
C
        IF( (NIREPR.EQ.JPGAUSSIAN).OR.(NIREPR.EQ.JPQUASI) ) THEN
          IRET = HIRLAMW(LO12PT,
     X                   OLDUV(IPINU),OLDUV(IPINV),ISZOUV,
     X                   NOGAUSS,AREA,POLE,GRID,
     X                   SWORK,SWORK(1+NUMPTS),NUMPTS,NLON,NLAT)
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTUVGH: HIRLAMW rotation failed',JPQUIET)
            INTUVGH = JPROUTINE + 3
            GOTO 900
          ENDIF
C
C       Rotate lat/long to lat/long
C
        ELSE
          OLDGRID(1) = REAL(NIGRID(1)) / PPMULT
          OLDGRID(2) = REAL(NIGRID(2)) / PPMULT
          IRET = HLL2LLW(LO12PT,OLDUV(IPINU),OLDUV(IPINV),
     X                   OLDGRID,AREA,POLE,GRID,
     X                   SWORK,SWORK(1+NUMPTS),NUMPTS,NLON,NLAT)
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTUVGH: HLL2LLW rotation failed',JPQUIET)
            INTUVGH = JPROUTINE + 3
            GOTO 900
          ENDIF
        ENDIF
C
C       If a 'frame' has been specified, build the frame
C
        IF( LFRAME ) THEN
          LIMISSV = .TRUE.
          CALL MKFRAME(NLON,NLAT,SWORK,RMISSGV,NOFRAME)
          CALL MKFRAME(NLON,NLAT,SWORK(1+NUMPTS),RMISSGV,NOFRAME)
        ENDIF
C
      ELSE
C
C     -----------------------------------------------------------------|
C*    Section 4.   Generate interpolated gaussian U and V fields.
C     -----------------------------------------------------------------|
C
  400   CONTINUE
C
        IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X    'INTUVGH: Gaussian to gaussian',JPQUIET)
C
C       Dynamically allocate memory for rotated gaussian grids
C
        NUMPTS = 0
        DO LOOP = 1, NOGAUSS*2
          NUMPTS = NUMPTS + NOLPTS(LOOP)
        ENDDO
        ISIZE = 2 * NUMPTS
        CALL JMEMHAN( 11, ISWORK, ISIZE, 1, IRET)
        IF( IRET.NE.0 ) THEN
          IF( LDEBUG ) CALL INTLOG(JP_ERROR,
     X      'INTUVGH: memory alloc for gaussian grid fail',JPQUIET)
          INTUVGH = JPROUTINE + 4
          GOTO 900
        ENDIF
C
        NGAUSS = ISEC2(10)
        IRET = HRG2GGW(LO12PT,
     X                 OLDUV(IPINU),OLDUV(IPINV),ISZOUV,
     X                 NGAUSS,AREA,POLE,NOGAUSS,HOGAUST,
     X                 SWORK,SWORK(1+ISIZE/2),ISIZE,NUMPTS)
        IF( IRET.NE.0 ) THEN
          IF( LDEBUG ) CALL INTLOG(JP_ERROR,
     X        'INTUVGH: HRG2GGW rotation failed',JPQUIET)
            INTUVGH = JPROUTINE + 4
            GOTO 900
          ENDIF
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 5.   Pack the fields into GRIB format
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
      IF( ((NOREPR.EQ.JPQUASI).OR.
     X     (NOREPR.EQ.JPQGGROT).OR.
     X     (NOREPR.EQ.JPFGGROT).OR.
     X     (NOREPR.EQ.JPGAUSSIAN)).AND.
     X    ( (NOROTA(1).NE.9000000).AND.(NOROTA(2).NE.0) ) ) THEN
        ISEC2(1) = JPFGGROT
      ELSE
        ISEC2(1) = NOREPR
      ENDIF
C
      ISEC4(1) = NUMPTS
C
C     Reset the input format flag
C
      NIFORM = 1
C
C     Set the components flag for rotated U and V coefficients
C
      ISEC2(19) = 8
C
      IRET = INTUVDH(SWORK,NUMPTS,KUGRIBO,OUTLENU,'C',IN_U)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVGH: Wind component into GRIB encoding fail',IRET)
        INTUVGH = JPROUTINE + 4
        GOTO 900
      ENDIF
C
      IRET = INTUVDH(SWORK(1+(ISIZE/2)),NUMPTS,KVGRIBO,OUTLENV,'C',IN_V)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVGH: Wind component into GRIB encoding fail',IRET)
        INTUVGH = JPROUTINE + 4
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9.   Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
C     Clear change flags for next product processing and reset wind flag
C
      LCHANGE = .FALSE.
      LSMCHNG = .FALSE.
      LWINDSET  = LOLDWIND
C
      RETURN
      END
