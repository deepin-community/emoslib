C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INTUVP( KVGRIB, KDGRIB, INLEN,
     X                         KUGRIBO, KVGRIBO, OUTLEN)
C
C---->
C**** INTUVP
C
C     Purpose
C     -------
C
C     Interpolate GRIB format input vorticity and divergence field to
C     GRIB format U and V fields.
C
C
C     Interface
C     ---------
C
C     IRET = INTUVP(KVGRIB,KDGRIB,INLEN,KUGRIBO,KVGRIBO,OUTLEN)
C
C     Input
C     -----
C
C     KVGRIB - Input vorticity field  (spectral, GRIB format).
C     KDGRIB - Input divergence field (spectral, GRIB format).
C     INLEN  - Input field length (words).
C
C
C     Output
C     ------
C
C     KUGRIBO - Output U field (GRIB format).
C     KVGRIBO - Output V field (GRIB format).
C     OUTLEN  - Length of each output U and V field (words).
C
C
C     Method
C     ------
C
C     Convert spectral vorticity/divergence to spectral U/V and then
C     interpolate U and V to output fields.
C
C     Note that a common block is used in intf.h to hold the U/V
C     fields before interpolation.
C
C     Externals
C     ---------
C
C     IBASINI - Ensure basic interpolation setup is done.
C     INTUVDH - Encodes/decodes data into/from GRIB code.
C     INTUVXH - Interpolate U and V spectral components to grid point.
C     JVOD2UV - Converts spectral vorticity/divergence to spectral U/V.
C     JMEMHAN - Allocate scratch memory.
C     ISCRSZ  - Calculate number of values in generated field.
C     FIXAREA - Fixup area definition to correspond to grid definitions
C     INTLOG  - Log error message.
C     RESET_C - Reset interpolation handling options using GRIB product.
C     INSANE  - Ensure no outrageous values given for interpolation.
C     GRSMKP  - P factor calculation switch for routine GRIBEX.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     February 2001
C     Rewritten to use more functions for processing.
C
C
C----<
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
C     Function arguments
C
      INTEGER KVGRIB(*), KDGRIB(*), INLEN
      INTEGER KUGRIBO(*), KVGRIBO(*), OUTLEN
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "intf.h"
C
C     Parameters
C
      INTEGER JPROUTINE, JPALLOC, JPSCR3, JPSCR4, JPSCR5
      INTEGER JPVORT, JPDIVE
      PARAMETER (JPROUTINE = 26900 )
      PARAMETER (JPALLOC = 1)
      PARAMETER (JPSCR3 = 3)
      PARAMETER (JPSCR4 = 4)
      PARAMETER (JPSCR5 = 5)
      PARAMETER (JPVORT = 138)
      PARAMETER (JPDIVE = 155)
C
C     Local variables
C
      REAL EW, NS, DUMMY
      LOGICAL LOLDWIND, LSPECUV
      INTEGER IERR, KPR, ISZVD, ISZUV, ISIZE, ISAME, IDIVOFF
      INTEGER LOOP, MTRUNC, NTRUNC, NTROLD, NTROLD2, NPARAM
      INTEGER NOLD, NLEN, NLENU, NLENV, HNORESO
      INTEGER INRESO, IHOLD
      LOGICAL LSTYLE
      REAL NORTH, SOUTH, WEST, EAST
C
      LOGICAL LFIRST, LNEWUV
      CHARACTER*3 EXTRA
      DATA LFIRST/.TRUE./, LNEWUV/.TRUE./, EXTRA/'NO '/
      SAVE LFIRST, LNEWUV
C
      DATA NTROLD/-1/, NTROLD2/-1/
      SAVE NTROLD, NTROLD2
      INTEGER IPVORT, IPDIV, IP_U, IP_V
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IZNFLDO
#endif
#endif
      REAL ZNFLDO
      POINTER ( IZNFLDO, ZNFLDO )
      DIMENSION ZNFLDO( 1 )
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IUV, IVD
#endif
#endif
      REAL UV, VD
      POINTER ( IUV, UV )
      POINTER ( IVD, VD )
      DIMENSION UV( 1 ), VD( 1 )
C
C     Externals
C
      INTEGER RESET_C, ISCRSZ, FIXAREA, AURESOL, DSSAREA
      INTEGER IBASINI, INSANE, INTUVDH, INTUVXH
      INTEGER INTUVGH !, INTUVPH
      EXTERNAL RESET_C, ISCRSZ, FIXAREA, AURESOL, DSSAREA
      EXTERNAL IBASINI, INSANE, INTUVDH, INTUVXH
      EXTERNAL INTUVGH !, INTUVPH
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
      INTUVP = 0
      IERR   = 0
      KPR    = 0
C
      IF( LFIRST ) THEN
        CALL GETENV('IGNORE_UV_EXTRA_MODE', EXTRA)
        IF((EXTRA(1:1).EQ.'Y').OR.(EXTRA(1:1).EQ.'y')) LNEWUV = .FALSE.
        IF( LNEWUV ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'INTUVP: IGNORE_UV_EXTRA_MODE not turned on',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,
     X      'INTUVP: IGNORE_UV_EXTRA_MODE turned on',JPQUIET)
        ENDIF
        LFIRST = .FALSE.
      ENDIF
C
      LOLDWIND = LWINDSET
cs      LSPECUV = (NOREPR.EQ.JPSPHERE).OR.(NOREPR.EQ.JPSPHROT)
C
C     Ensure that basic initialisation has been done
C
      IERR = IBASINI(0)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTUVP: basic initialise failed',JPQUIET)
        INTUVP = IERR
        GOTO 990
      ENDIF
C
C     Establish truncation of the input vorticity and divergence:
C     unpack GRIB sections 1 and 2.
C
      NPARAM = JPVORT
      ISZVD = 1
      NLEN = INLEN
      IERR = INTUVDH(DUMMY,ISZVD,KVGRIB,NLEN,'I',NPARAM)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTUVP: GRIB header decode failed',IERR)
        INTUVP = IERR
        GOTO 990
      ENDIF
C
C     If a rotation is required, check the input fields and call the
C     appropriate interpolation
C
      IF( LUSEHIR.AND.LNOROTA ) THEN
cs        LWIND = .TRUE.
cs        LWINDSET = .TRUE.
cs        IF( (ISEC1(6).EQ.JPVORT).AND.(ISEC2(1).EQ.JPSPHERE) ) THEN
csC
csC         Spectral vorticity/divergence to rotated U and V
csC
cs          NLEN  = INLEN
cs          NLENU = OUTLEN
cs          NLENV = OUTLEN
cs          IERR = INTUVPH(KVGRIB,KDGRIB,NLEN,KUGRIBO,KVGRIBO,
cs     X                   NLENU,NLENV)
C
cs        ELSE IF( ((ISEC1(6).EQ.JP_U).OR.(ISEC1(6).EQ.JP_10U)).AND.
cs        IF( ((ISEC1(6).EQ.JP_U).OR.(ISEC1(6).EQ.JP_10U)).AND.
        IF( (ISEC1(6).EQ.JP_10U).AND.
     X           (ISEC2(1).EQ.JPGAUSSIAN) ) THEN
C
C         Gaussian U and V to rotated U and V
C
          NLEN  = INLEN
          NLENU = OUTLEN
          NLENV = OUTLEN
          IERR = INTUVGH(KVGRIB,KDGRIB,NLEN,KUGRIBO,KVGRIBO,
     X                   NLENU,NLENV)
         IF( IERR.NE.0 ) THEN
           CALL INTLOG(JP_ERROR,'INTUVP: Rotation failed',IERR)
           GOTO 990
        ENDIF
        INTUVP = IERR
           GOTO 900
C
cs        ELSE
cs          CALL INTLOG(JP_ERROR,'INTUVP: Invalid rotation',IERR)
cs         IERR = JPROUTINE + 1
        ENDIF
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2.   Unpack the vorticity/divergence fields.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Get scratch memory for vorticity/divergence unpacked fields.
C     Vorticity/divergence memory areas are adjacent.
C
      INRESO = ISEC2(2)
      ISZVD  = (INRESO+1)*(INRESO+4)
      IPVORT = 1
      IPDIV  = IPVORT + ISZVD
      CALL JMEMHAN( JPSCR4, IVD, ISZVD*2, JPALLOC, IERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVP: Scratch memory type 4 allocation failed.',JPQUIET)
        INTUVP = IERR
        GOTO 990
      ENDIF
C
C     Decode vorticity and divergence.
C
      NPARAM = JPVORT
      NLEN = INLEN
      IERR = INTUVDH(VD(IPVORT),ISZVD,KVGRIB,NLEN,'D',NPARAM)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTUVP: Vorticity decoding failed',IERR)
        INTUVP = IERR
        GOTO 990
      ENDIF
C
      NPARAM = JPDIVE
      NLEN = INLEN
      IERR = INTUVDH(VD(IPDIV),ISZVD,KDGRIB,NLEN,'D',NPARAM)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTUVP: Divergence decoding failed',IERR)
        INTUVP = IERR
        GOTO 990
      ENDIF
C
C     Setup interpolation options from input GRIB characteristics.
C
      IHOLD = NIRESO
      IERR = RESET_C( ISEC1, ISEC2, ZSEC2, ISEC4)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVP: Setup interp. options from GRIB failed.',JPQUIET)
        INTUVP = IERR
        GOTO 990
      ENDIF
      IF( IHOLD.NE.0 ) NIRESO = IHOLD
C
C     Check that no outrageous values given for interpolation
C
      ISAME = INSANE()
      IF( ISAME.GT.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVP: Interpolation cannot use given values.',JPQUIET)
        INTUVP = ISAME
        GOTO 990
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3. Convert spectral vorticity/divergence to spectral U/V
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      LSPECUV = (NOREPR.EQ.JPSPHERE).OR.(NOREPR.EQ.JPSPHROT)
      IF( LSPECUV ) THEN
C
C       Spectral U and V for Tn are to be generated from vorticity
C       and divergence spectral T(n-1)
C
        IF( LARESOL.AND.LNEWUV ) THEN
          IF( (NOGRID(1).NE.0).AND.(NOGRID(2).NE.0) ) THEN
            EW = FLOAT(NOGRID(1))/PPMULT
            NS = FLOAT(NOGRID(2))/PPMULT
            NTRUNC = AURESOL(NS,EW) - 1
          ELSE IF( NOGAUSS.NE.0 ) THEN
            EW = 90.0/FLOAT(NOGAUSS)
            NS = EW
            NTRUNC = AURESOL(NS,EW) - 1
          ELSE IF( LNORESO ) THEN
            NTRUNC = NORESO - 1
          ELSE
            NTRUNC = INRESO - 1
          ENDIF
          IF( NTRUNC.GT.(INRESO-1) ) NTRUNC = INRESO - 1
C
        ELSE IF( LNORESO ) THEN
          NTRUNC = NORESO - 1
        ELSE
          NTRUNC = INRESO - 1
        ENDIF
C
        IF( LNEWUV ) THEN
          MTRUNC = NTRUNC + 1
        ELSE
          NTRUNC = NTRUNC + 1
          MTRUNC = NTRUNC
        ENDIF
C
C     -----------------------------------------------------------------|
C       Use old-style processing if IGNORE_UV_EXTRA_MODE = Y
C     -----------------------------------------------------------------|
C
        IF( .NOT.LNEWUV ) THEN
C
          NIRESO = ISEC2(2)
C
          CALL INTLOG(JP_DEBUG,'INTUVP: vo/div truncation = ', NIRESO)
C
C         Get scratch memory for U and V spectral fields.
C         U and V memory areas are adjacent.
C
          ISZUV = MAX((NIRESO+1)*(NIRESO+4),(NORESO+1)*(NORESO+4))
          IP_U  = 1
          IP_V  = IP_U + ISZUV
C
          ISIZE = ISZUV*2
          CALL JMEMHAN( JPSCR3, IUV, ISIZE, JPALLOC, IERR)
          IF( IERR.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTUVP: Scratch memory type 3 allocation failed',JPQUIET)
            INTUVP = IERR
            GOTO 900
          ENDIF
C
C         Generate U and V with same truncation as input fields.
C
          CALL INTLOG(JP_DEBUG,
     X      'INTUVP: Make intermediate U/V with truncation = ', NIRESO)
          CALL JVOD2UV(VD(IPVORT),VD(IPDIV),NIRESO,
     X                 UV(IP_U),UV(IP_V),NIRESO)
C
C         Do spectral truncation for spectral output
C
          IF( LNORESO ) THEN
C
            CALL INTLOG(JP_DEBUG,
     X        'INTUVP: Make U/V spectral output with truncation',NORESO)
C
            ISIZE =  (NORESO+1)*(NORESO+4)
            CALL JMEMHAN( JPSCR5, IZNFLDO, ISIZE, JPALLOC, IERR)
            IF( IERR.NE.0 ) THEN
              CALL INTLOG(JP_ERROR,
     X          'INTUVP: Scratch memory type 5 allocn failed',JPQUIET)
              INTUVP = JPROUTINE + 4
              GOTO 900
            ENDIF
C
            CALL SH2SH( UV(IP_U), NIRESO, ZNFLDO, NORESO )
            DO LOOP = 1, (NORESO+1)*(NORESO+2)
              UV(IP_U+LOOP-1) = ZNFLDO(LOOP)
            ENDDO
C
            CALL SH2SH( UV(IP_V), NIRESO, ZNFLDO, NORESO )
            IP_V = 1 + (NORESO+1)*(NORESO+4)
            DO LOOP = 1, (NORESO+1)*(NORESO+2)
              UV(IP_V+LOOP-1) = ZNFLDO(LOOP)
            ENDDO
C
            NIRESO = NORESO
C
          ENDIF
C
C         Set GRIBEX flag to force recalculation of complex packing
C         factor.
C
          CALL GRSMKP(1)
C
C         Code U into GRIB
C
          NPARAM = JP_U
          NLENU = OUTLEN
          IERR = INTUVDH(UV(IP_U),ISZUV,KUGRIBO,NLENU,'C',NPARAM)
          IF( IERR.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTUVP: U encoding into GRIB failed.',IERR)
            INTUVP = JPROUTINE + 4
            GOTO 380
          ENDIF
C
C         Code V into GRIB
C
          NPARAM = JP_V
          NLENV = OUTLEN
          IERR = INTUVDH(UV(IP_V),ISZUV,KVGRIBO,NLENV,'C',NPARAM)
          IF( IERR.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTUVP: V encoding into GRIB failed.',IERR)
            INTUVP = JPROUTINE + 4
            GOTO 380
          ENDIF
C
  380     CONTINUE
C
C         Turn off GRIBEX flag which forces recalculation of complex
C         packing factor
C
          CALL GRSMKP(0)
C
          GOTO 900
C
C     -----------------------------------------------------------------|
C       Use new-style processing if IGNORE_UV_EXTRA_MODE not set
C     -----------------------------------------------------------------|
C
        ELSE
C
          CALL INTLOG(JP_DEBUG,'INTUVP: vo/div truncation = ', NTRUNC)
          CALL INTLOG(JP_DEBUG,'INTUVP: U/V truncation    = ', MTRUNC)
C
C         Truncate vorticity and divergence to correspond to U/V
C
          ISIZE =  (MTRUNC+1)*(MTRUNC+4)
          CALL JMEMHAN( JPSCR5, IZNFLDO, ISIZE*2, JPALLOC, IERR)
          IF( IERR.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTUVP: Scratch memory type 5 allocation failed',JPQUIET)
            INTUVP = JPROUTINE + 4
            GOTO 990
          ENDIF
C
          IPVORT = 1
          IPDIV  = IPVORT + (INRESO+1)*(INRESO+4)
          IDIVOFF = 1 + (NTRUNC+1)*(NTRUNC+4)
C
          CALL SH2SH( VD(IPVORT), INRESO, ZNFLDO, NTRUNC )
C
          CALL SH2SH( VD(IPDIV), INRESO, ZNFLDO(IDIVOFF), NTRUNC )
C
C         Get scratch memory for U and V spectral fields.
C         U and V memory areas are adjacent.
C
          ISZUV = (MTRUNC+1)*(MTRUNC+4)
          IP_U  = 1
          IP_V  = IP_U + ISZUV
C
          ISIZE = ISZUV*2
          CALL JMEMHAN( JPSCR3, IUV, ISIZE, JPALLOC, IERR)
          IF( IERR.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTUVP: Scratch memory type 3 allocation failed',JPQUIET)
            INTUVP = IERR
            GOTO 990
          ENDIF
C
C         Generate U and V spectral fields
C
          CALL JVOD2UV(ZNFLDO(IPVORT),ZNFLDO(IDIVOFF),NTRUNC,
     X                 UV(IP_U),UV(IP_V),MTRUNC)
C
C         Set GRIBEX flag to force recalculation of complex packing
C         factor.
C         Preserve the original output truncation
C
          CALL GRSMKP(1)
          HNORESO = NORESO
C
C         Code U into GRIB using the new truncation
C
          NORESO = MTRUNC
          NPARAM = JP_U
          NLENU = OUTLEN
          IERR = INTUVDH(UV(IP_U),ISZUV,KUGRIBO,NLENU,'C',NPARAM)
          IF( IERR.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTUVP: U encoding into GRIB failed.',IERR)
            INTUVP = JPROUTINE + 4
            GOTO 390
          ENDIF
C
C         Code V into GRIB using the new truncation
C
          NPARAM = JP_V
          NLENV = OUTLEN
          IERR = INTUVDH(UV(IP_V),ISZUV,KVGRIBO,NLENV,'C',NPARAM)
          IF( IERR.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTUVP: V encoding into GRIB failed.',IERR)
            INTUVP = JPROUTINE + 4
            GOTO 390
          ENDIF
C
  390     CONTINUE
C
C         Turn off GRIBEX flag which forces recalculation of complex
C         packing factor
C         Restore the original output truncation
C
          CALL GRSMKP(0)
          NORESO = HNORESO
C
          GOTO 900
        ENDIF
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 4.   Generate grid point GRIB format U and V fields.
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
C     Spectral U and V for Tn are to be generated from vorticity
C     and divergence spectral T(n-1)
C
C     See whether or not the 'autoresol' flag is set.
C     If not, use the input truncation.
C
      IF( LARESOL ) THEN
        IF( (NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPREGROT) ) THEN
          EW = FLOAT(NOGRID(1))/PPMULT
          NS = FLOAT(NOGRID(2))/PPMULT
        ELSE
          EW = 90.0/FLOAT(NOGAUSS)
          NS = EW
        ENDIF
        NTRUNC = AURESOL(EW,NS)
        IF( NTRUNC.NE.NTROLD ) THEN
          NTROLD = NTRUNC
          CALL INTLOG(JP_WARN,
     X      'INTUVP: Resolution automatically set to ', NTRUNC)
        ENDIF
      ELSE IF( LNORESO ) THEN
        NTRUNC = NORESO
      ELSE
        NTRUNC = INRESO
      ENDIF
C
C     Check whether the output resolution is greater than the input
C
      IF( NTRUNC.GT.INRESO ) THEN
C
C       Issue warning if the output resolution was user-supplied
C
        IF( .NOT.LARESOL ) THEN
C
C         Revert to the input truncation
C
          IF( INRESO.NE.NTROLD2 ) THEN
            CALL INTLOG(JP_WARN,
     X        'INTUVP: spectral -> grid point interpolation',JPQUIET)
            CALL INTLOG(JP_WARN,
     X        'INTUVP: User supplied resolution = ',NTRUNC)
            CALL INTLOG(JP_WARN,
     X        'INTUVP: Input field resolution   = ',INRESO)
            CALL INTLOG(JP_WARN,
     X        'INTUVP: User supplied resolution ignored',JPQUIET)
            CALL INTLOG(JP_WARN,
     X        'INTUVP: Input field resolution has been used',JPQUIET)
            NTROLD2 = INRESO
          ENDIF
          NTRUNC = INRESO
C
        ELSE
C
C         Revert to the input truncation
C
          NTRUNC = INRESO
          IF( NTRUNC.NE.NTROLD2 ) THEN
            NTROLD2 = NTRUNC
            CALL INTLOG(JP_WARN,
     X        'INTUVP: Auto-resolution selection too high',JPQUIET)
            CALL INTLOG(JP_WARN,
     X        'INTUVP: Resolution set to input resolution: ',NTRUNC)
          ENDIF
        ENDIF
      ENDIF
C
C     IF extra mode is in use, adjust the calculated truncation.
C
      MTRUNC = NTRUNC
      IF( LNEWUV ) NTRUNC = MTRUNC - 1
C
      CALL INTLOG(JP_DEBUG,'INTUVP: vo/div truncation = ', NTRUNC)
      CALL INTLOG(JP_DEBUG,'INTUVP: U/V truncation    = ', MTRUNC)
C
      ISIZE =  (MTRUNC+1)*(MTRUNC+4)
      CALL JMEMHAN( JPSCR5, IZNFLDO, ISIZE*2, JPALLOC, IERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVP: Scratch memory type 5 allocation failed.',JPQUIET)
        INTUVP = JPROUTINE + 4
        GOTO 990
      ENDIF
C
      IPVORT = 1
      IPDIV = 1 + (INRESO+1)*(INRESO+4)
      IDIVOFF = IPVORT + (NTRUNC+1)*(NTRUNC+4)
C
      CALL SH2SH( VD(IPDIV), INRESO, ZNFLDO(IDIVOFF), NTRUNC )
C
      CALL SH2SH( VD(IPVORT), INRESO, ZNFLDO, NTRUNC )
C
C     Get scratch memory for U and V spectral fields.
C     U and V memory areas are adjacent.
C
      ISZUV = (MTRUNC+1)*(MTRUNC+4)
      IP_U  = 1
      IP_V  = IP_U + ISZUV
C
      ISIZE = ISZUV*2
      CALL JMEMHAN( JPSCR3, IUV, ISIZE, JPALLOC, IERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVP: Scratch memory type 3 allocation failed.',JPQUIET)
        INTUVP = IERR
        GOTO 990
      ENDIF
C
C     Generate U and V spectral fields
C
      CALL JVOD2UV(ZNFLDO(IPVORT),ZNFLDO(IDIVOFF),NTRUNC,
     X             UV(IP_U),UV(IP_V),MTRUNC)
C
       LSTYLE = LNOSTYLE.AND.
     X         (NOSTYLE.EQ.JPSDISM).AND.
     X         (NOREPR.EQ.JPREGULAR)
C
        IF( LSTYLE ) THEN
            EW    = NOGRID(1) / PPMULT
            NS    = NOGRID(2) / PPMULT
            NORTH = REAL(NOAREA(1)) / PPMULT
            WEST  = REAL(NOAREA(2)) / PPMULT
            SOUTH = REAL(NOAREA(3)) / PPMULT
            EAST  = REAL(NOAREA(4)) / PPMULT
C
            IERR = DSSAREA( EW, NS, NORTH, WEST, SOUTH, EAST)
            IF( IERR.NE.0 ) THEN
              CALL INTLOG(JP_ERROR,'INTUVP: DSSAREA failed:',IERR)
              INTUVP = JPROUTINE + 9
              GOTO 900
            ENDIF
C
            NOAREA(1) = NINT(NORTH * PPMULT)
            NOAREA(2) = NINT(WEST  * PPMULT)
            NOAREA(3) = NINT(SOUTH * PPMULT)
            NOAREA(4) = NINT(EAST  * PPMULT)
         ELSE
C           Fixup area definition to correspond to grid definitions
             IERR = FIXAREA()
             IF ( IERR .NE. 0 ) THEN
               CALL INTLOG(JP_ERROR,
     X           'INTUVP: Fixup area definition failed.',JPQUIET)
               INTUVP = IERR
               GOTO 900
             ENDIF
         ENDIF

cs      IERR = FIXAREA()
cs      IF( IERR.NE.0 ) THEN
cs        CALL INTLOG(JP_ERROR,'INTUVP: Fixarea failed.',JPQUIET)
cs        INTUVP = IERR
cs        GOTO 990
cs      ENDIF
C
C     Get scratch space for interpolation
      ISIZE = ISCRSZ()*2
      IF( ISIZE.LE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTUVP: Get scratch space failed.',ISIZE)
        INTUVP = JPROUTINE + 5
        GOTO 990
      ENDIF
      CALL JMEMHAN( JPSCR5, IZNFLDO, ISIZE, JPALLOC, IERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVP: Scratch memory(5) allocation failed.',JPQUIET)
        INTUVP = JPROUTINE + 5
        GOTO 990
      ENDIF
C
      LWIND = .TRUE.
      LWINDSET = .TRUE.
C
C     Interpolate U and V
C
      NOLD = NIRESO
      NIRESO = MTRUNC
      NLENU = OUTLEN
      NLENV = OUTLEN
      IERR = INTUVXH(UV,ISZUV,ZNFLDO,KUGRIBO,KVGRIBO,NLENU,NLENV)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVP: U/V interpolation failed.',JPQUIET)
        INTUVP = JPROUTINE + 6
        GOTO 990
      ENDIF
C
      NIRESO = NOLD
C
C     -----------------------------------------------------------------|
C*    Section 9.   Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      OUTLEN = NLENU
C
  990 CONTINUE
C
C     Clear change flags for next product processing and reset wind flag
C
      LCHANGE = .FALSE.
      LSMCHNG = .FALSE.
      LWINDSET  = LOLDWIND
      LWIND = .FALSE.
C
      RETURN
      END
