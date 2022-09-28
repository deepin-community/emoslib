C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INTUVU( PVYIN, PDVIN, INLEN,
     X                         PUOUT, PVOUT, OUTLEN)
C
C---->
C**** INTUVU
C
C     Purpose
C     -------
C
C     Interpolate unpacked input vorticity and divergence field to
C     unpacked U and V fields.
C
C
C     Interface
C     ---------
C
C     IERR = INTUVU( PVYIN, PDVIN, INLEN, PUOUT,PVOUT,OUTLEN)
C
C     Input
C     -----
C
C     PVYIN  - Input vorticity field  (unpacked array).
C     PDVIN  - Input divergence field (unpacked array).
C     INLEN  - Input field length (words).
C
C
C     Output
C     ------
C
C     PUOUT  - Output U field (unpacked array).
C     PVOUT  - Output V field (unpacked array).
C     OUTLEN - Output field length (words).
C
C
C     Method
C     ------
C
C     Convert spectral vorticity/divergence to spectral U/V and then
C     interpolate U and V to output fields.
C
C
C     Externals
C     ---------
C
C     JVOD2UV - Converts spectral vorticity/divergence to spectral U/V.
C     JMEMHAN - Allocate scratch memory.
C     INTFAU  - Prepare to interpolate unpacked input field.
C     INTFBU  - Interpolate unpacked input field.
C     INTLOG  - Log error message.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     Feb 1995
C
C     J.D.Chambers     ECMWF        Feb 1997
C     Allow for 64-bit pointers
C
C----<
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "grfixed.h"
#include "intf.h"
#include "current.h"
C
C     Parameters
C
      INTEGER JPROUTINE, JPALLOC, JPSCR3, JPSCR5
      PARAMETER (JPROUTINE = 27000)
      PARAMETER (JPALLOC = 1)
      PARAMETER (JPSCR3 = 3)
      PARAMETER (JPSCR5 = 5)
C
C     Function arguments
C
      INTEGER INLEN, OUTLEN
      REAL PVYIN(INLEN), PDVIN(INLEN), PUOUT(*), PVOUT(*)
C
C     Local variables
C
      CHARACTER*1 HOLDTYP
      CHARACTER*1 HTYPE
      INTEGER IDIVOFF
      INTEGER IERR
      INTEGER IOHOLD(4)
      INTEGER IPVORT
      INTEGER ISIZE
      INTEGER ISZUV
      INTEGER KK
      INTEGER KPR
      INTEGER KPTS(JPGTRUNC*2)
      INTEGER LOOP
      INTEGER MTRUNC
      INTEGER NCOUNT
      INTEGER NGAUSS
      INTEGER NLAT
      INTEGER NLON
      INTEGER NOLD
      INTEGER NTROLD
      INTEGER NTROLD2
      INTEGER NTRUNC
      INTEGER NUMPTS
      INTEGER NUVFLAG
      LOGICAL LFRAME
      LOGICAL LOLDWIND
      LOGICAL LSFCUVI
      LOGICAL LSPCUVI
      LOGICAL LSPECUV
      LOGICAL LSTYLE
      REAL AREA(4)
      REAL EAST
      REAL EW
      REAL GLATS(JPGTRUNC*2)
      REAL GRID(2)
      REAL NORTH
      REAL NS
      REAL OLDGRID(2)
      REAL POLE(2)
      REAL SOUTH
      REAL WEST
C
      LOGICAL LFIRST, LNEWUV
      CHARACTER*3 EXTRA
      DATA LFIRST/.TRUE./, LNEWUV/.TRUE./, EXTRA/'NO '/
      SAVE LFIRST, LNEWUV
C
      DATA NTROLD/-1/, NTROLD2/-1/
      SAVE NTROLD, NTROLD2
      INTEGER IP_U, IP_V

      REAL RGGRID, SWORK
      POINTER (IRGGRID, RGGRID(1) )
      POINTER (ISWORK, SWORK(1) )
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
      INTEGER*8 IUV
#endif
#endif
      REAL UV
      POINTER ( IUV, UV )
      DIMENSION UV( 1 )
C
C     Externals
C
      INTEGER INTFAU, INTFBU, AURESOL, DSSAREA, FIXAREA
      INTEGER HIRLAMW, HSH2GG
      INTEGER HRG2GGW, HLL2LLW
      EXTERNAL INTFAU, INTFBU, AURESOL, DSSAREA, FIXAREA
      EXTERNAL HIRLAMW, HSH2GG
      EXTERNAL HRG2GGW, HLL2LLW
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
      IERR     = 0
      KPR      = 0
      LOLDWIND = .FALSE.
      INTUVU   = 0
C
C
C     Save output area definitions
C
      DO 110 LOOP = 1, 4
        IOHOLD(LOOP) = NOAREA(LOOP)
  110 CONTINUE

      LFRAME = LNOFRAME.AND.
     X         ((NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPGAUSSIAN).OR.
     X          (NOREPR.EQ.JPREGROT ).OR.(NOREPR.EQ.JPFGGROT  ) )

      IF( LFIRST ) THEN
        CALL GETENV('IGNORE_UV_EXTRA_MODE', EXTRA)
        IF((EXTRA(1:1).EQ.'Y').OR.(EXTRA(1:1).EQ.'y')) LNEWUV = .FALSE.
        IF( LNEWUV ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'INTUVU: IGNORE_UV_EXTRA_MODE not turned on',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,
     X      'INTUVU: IGNORE_UV_EXTRA_MODE turned on',JPQUIET)
        ENDIF
        LFIRST = .FALSE.
      ENDIF
C
      NOLD = NIRESO
C
      LSPECUV = (NOREPR.EQ.JPSPHERE).OR.(NOREPR.EQ.JPSPHROT)
      LSPCUVI = (NIREPR.EQ.JPSPHERE).OR.(NIREPR.EQ.JPSPHROT)
      LSFCUVI = (.NOT.LSPCUVI).AND.LNOROTA

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
cs    Regular Gaussian has to be set here
      IF( NOREPR.EQ.JPNOTYPE ) THEN
        IF( (NOGAUSO.NE.NOGAUSS).OR.(HOGAUST.NE.'F') ) THEN
          HTYPE = 'F'
          CALL JGETGG(NOGAUSS,HTYPE,ROGAUSS,NOLPTS,IERR)
          IF( IERR.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTUVU: JGETGG failed, NOGAUSS = ',NOGAUSS)
            INTUVU = IERR
            GOTO 900
          ENDIF
          NOGAUSO = NOGAUSS
          HOGAUST = HTYPE
        ENDIF
        NOREPR = JPGAUSSIAN
      ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     -----------------------------------------------------------------|
C*    Section 2. Convert spectral vorticity/divergence to spectral U/V
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
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
            NTRUNC = NIRESO - 1
          ENDIF
          IF( NTRUNC.GT.(NIRESO-1) ) NTRUNC = NIRESO - 1
C
        ELSE IF( LNORESO ) THEN
          NTRUNC = NORESO - 1
        ELSE
          NTRUNC = NIRESO - 1
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
          CALL INTLOG(JP_DEBUG,'INTUVU: vo/div truncation = ', NIRESO)
C
C         Get scratch memory for U and V spectral fields.
C         U and V memory areas are adjacent.
C
          ISZUV = (NIRESO+1)*(NIRESO+2)
          IP_U = 1
          IP_V = 1 + ISZUV
          CALL JMEMHAN( JPSCR3, IUV, ISZUV*2, JPALLOC, IERR)
          IF ( IERR .NE. 0 ) THEN
            CALL INTLOG(JP_ERROR,'INTUVU: Memory allocn fail',JPQUIET)
            INTUVU = IERR
            GOTO 900
          ENDIF
C
C         Generate U and V with same truncation as input fields.
C
          CALL INTLOG(JP_DEBUG,
     X      'INTUVU: Make intermediate U/V with truncation = ', NIRESO)
C
          CALL JVOD2UV(PVYIN,PDVIN,NIRESO,UV(IP_U),UV(IP_V),NIRESO)
C
C         Is the output a truncated spectral field?
C
          IF( LNORESO ) THEN
C
            CALL INTLOG(JP_DEBUG,
     X        'INTUVU: Produce spectral output with truncation',NORESO)
C
            ISIZE = (NORESO+1)*(NORESO+2)
            CALL JMEMHAN( JPSCR5, IZNFLDO, ISIZE, JPALLOC, IERR)
            IF( IERR.NE.0 ) THEN
              CALL INTLOG(JP_FATAL,'INTUVU: Get scratch fail',JPQUIET)
              INTUVU = JPROUTINE + 2
              GOTO 900
            ENDIF
C
            CALL SH2SH( UV(IP_U), NIRESO, ZNFLDO, NORESO )
            DO LOOP = 1, ISIZE
              PUOUT(LOOP) = ZNFLDO(LOOP)
            ENDDO
C
            CALL SH2SH( UV(IP_V), NIRESO, ZNFLDO, NORESO )
            DO LOOP = 1, ISIZE
              PVOUT(LOOP) = ZNFLDO(LOOP)
            ENDDO
C
            NIRESO = NORESO
C
            OUTLEN = ISZUV

            GOTO 900
C
          ENDIF
C
C     -----------------------------------------------------------------|
C       Use new-style processing if IGNORE_UV_EXTRA_MODE not set
C     -----------------------------------------------------------------|
C
        ELSE
C
          CALL INTLOG(JP_DEBUG,'INTUVU: vo/div truncation = ', NTRUNC)
          CALL INTLOG(JP_DEBUG,'INTUVU: U/V truncation    = ', MTRUNC)
C
C         Truncate vorticity and divergence to correspond to U/V
C
          ISIZE =  (MTRUNC+1)*(MTRUNC+2)
          CALL JMEMHAN( JPSCR5, IZNFLDO, ISIZE*2, JPALLOC, IERR)
          IF( IERR.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTUVU: Scratch memory type 5 allocn failed.',JPQUIET)
            INTUVU = JPROUTINE + 2
            GOTO 900
          ENDIF
C
          IPVORT = 1
          CALL SH2SH( PVYIN, NIRESO, ZNFLDO, NTRUNC )
C
          IDIVOFF = 1 + (NTRUNC+1)*(NTRUNC+2)
          CALL SH2SH( PDVIN, NIRESO, ZNFLDO(IDIVOFF), NTRUNC )
C
C         Get scratch memory for U and V spectral fields.
C         U and V memory areas are adjacent.
C
          ISZUV = (MTRUNC+1)*(MTRUNC+2)
          IP_U = 1
          IP_V = 1 + ISZUV
C
          ISIZE = ISZUV*2
          CALL JMEMHAN( JPSCR3, IUV, ISIZE, JPALLOC, IERR)
          IF ( IERR .NE. 0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTUVU: Scratch memory type 3 allocation failed.',JPQUIET)
          INTUVU = IERR
          GOTO 900
        ENDIF
C
C       Generate U and V spectral fields
C
          CALL JVOD2UV(ZNFLDO(IPVORT),ZNFLDO(IDIVOFF),NTRUNC,
     X                 UV(IP_U),UV(IP_V),MTRUNC)
C
          DO LOOP = 1, ISZUV
            PUOUT(LOOP) = UV(LOOP)
            PVOUT(LOOP) = UV(LOOP+ISZUV)
          ENDDO
C
C
          OUTLEN = ISZUV
cs  added in case of packing after conversion
          NORESO = MTRUNC
C
          GOTO 900
C
        ENDIF
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3.   Generate grid point GRIB format U and V fields.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
cs    this is for merging with grib_api
      LUVCOMP = .FALSE.
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
     X      'INTUVU: Resolution automatically set to ', NTRUNC)
        ENDIF
      ELSE IF( LNORESO ) THEN
        NTRUNC = NORESO
      ELSE
        NTRUNC = NIRESO
      ENDIF
C
C     Check whether the output resolution is greater than the input
C
      IF( NTRUNC.GT.NIRESO ) THEN
C
C       Issue warning if the output resolution was user-supplied
C
        IF( .NOT.LARESOL ) THEN
C
C         Revert to the input truncation
C
          IF( NIRESO.NE.NTROLD2 ) THEN
            CALL INTLOG(JP_WARN,
     X        'INTUVU: spectral -> grid point interpolation',JPQUIET)
            CALL INTLOG(JP_WARN,
     X        'INTUVU: User supplied resolution = ',NTRUNC)
            CALL INTLOG(JP_WARN,
     X        'INTUVU: Input field resolution   = ',NIRESO)
            CALL INTLOG(JP_WARN,
     X        'INTUVU: User supplied resolution ignored',JPQUIET)
            CALL INTLOG(JP_WARN,
     X        'INTUVU: Input field resolution has been used',JPQUIET)
            NTROLD2 = NIRESO
          ENDIF
          NTRUNC = NIRESO
C
        ELSE
C
C         Revert to the input truncation
C
          NTRUNC = NIRESO
          IF( NTRUNC.NE.NTROLD2 ) THEN
            NTROLD2 = NTRUNC
            CALL INTLOG(JP_WARN,
     X        'INTUVU: Automatic resolution selectn too high',JPQUIET)
            CALL INTLOG(JP_WARN,
     X        'INTUVU: Resolution reset to input resolution: ',NTRUNC)
          ENDIF
C
        ENDIF
      ENDIF
C
C     IF extra mode is in use, adjust the calculated truncation.
C
      MTRUNC = NTRUNC
      IF( LNEWUV ) NTRUNC = MTRUNC - 1
C
      CALL INTLOG(JP_DEBUG,'INTUVU: vo/div truncation = ', NTRUNC)
      CALL INTLOG(JP_DEBUG,'INTUVU: U/V truncation    = ', MTRUNC)
C
      ISIZE = (MTRUNC+1)*(MTRUNC+2)
      CALL JMEMHAN( JPSCR5, IZNFLDO, ISIZE*2, JPALLOC, IERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_FATAL,
     X    'INTUVU: Get scratch space failed',JPQUIET)
        INTUVU = JPROUTINE + 3
        GOTO 900
      ENDIF
C
C     Adjust the vorticity and divergence by one wave number before
C     conversion to U and V
C
      IPVORT = 1
      IDIVOFF = 1 + (NTRUNC+1)*(NTRUNC+2)
C
      CALL SH2SH( PVYIN, NIRESO, ZNFLDO, NTRUNC )
C
      CALL SH2SH( PDVIN, NIRESO, ZNFLDO(IDIVOFF), NTRUNC )
C
C     Get scratch memory for U and V spectral fields.
C     U and V memory areas are adjacent.
C
      ISZUV = (MTRUNC+1)*(MTRUNC+2)
      IP_U  = 1
      IP_V  = IP_U + ISZUV
C
      ISIZE = ISZUV*2
      CALL JMEMHAN( JPSCR3, IUV, ISIZE, JPALLOC, IERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVU: Scratch memory type 3 allocation failed.',JPQUIET)
        INTUVU = IERR
        GOTO 900
      ENDIF
C
C     Generate U and V spectral fields
C
      CALL JVOD2UV(ZNFLDO(IPVORT),ZNFLDO(IDIVOFF),NTRUNC,
     X             UV(IP_U),UV(IP_V),MTRUNC)
C
      NIRESO = MTRUNC
C
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
              CALL INTLOG(JP_ERROR,'INTUVU: DSSAREA failed:',IERR)
              INTUVU = JPROUTINE + 3
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
     X           'INTUVU: Fixup area definition failed.',JPQUIET)
               INTUVU = IERR
               GOTO 900
             ENDIF
         ENDIF
      DO KK = 1,4
          NOAAPI(KK) = NOAREA(KK)
      ENDDO

      NIFORM = 0
      NIPARAM = JP_U
      LWIND = .TRUE.
      LOLDWIND = LWINDSET
      LWINDSET = .TRUE.
C
      IF(LSFCUVI)   GOTO 850
      IF( LNOROTA ) GOTO 700

C     -----------------------------------------------------------------|
C*    Section 4.   Interpolate U field.
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
      IERR = INTFAU( UV(IP_U), ISZUV, PUOUT, OUTLEN)
      IF ( IERR .NE. 0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVU: Prepare to interpolate failed.',JPQUIET)
        INTUVU = IERR
        GOTO 900
      ENDIF
C
      IERR = INTFBU( UV(IP_U), ISZUV, PUOUT, OUTLEN)
C
      IF ( IERR .NE. 0 ) THEN
        CALL INTLOG(JP_ERROR,'INTUVU: Interpolation failed.',JPQUIET)
        INTUVU = IERR
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 5.   Interpolate V field.
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
      NIPARAM = JP_V
      IERR = INTFAU( UV(IP_V), ISZUV, PVOUT, OUTLEN)
      IF ( IERR .NE. 0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVU: Prepare to interpolate failed.',JPQUIET)
        INTUVU = IERR
        GOTO 900
      ENDIF
C
      IERR = INTFBU( UV(IP_V), ISZUV, PVOUT, OUTLEN)
C
      IF ( IERR .NE. 0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVU: INTFBU interpolate failed.',JPQUIET)
        INTUVU = IERR
        GOTO 900
      ENDIF

cs      GOTO 900
       GOTO 890
C
C     -----------------------------------------------------------------|
C*    Section 6.  Initialise spectral to grid-point with rotation
C     -----------------------------------------------------------------|
C
  700 CONTINUE
      IF( .NOT.LUSEHIR ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVU: Unable to rotate spectral U or V:',JPQUIET)
        INTUVU  = JPROUTINE + 6
        GOTO 900
      ENDIF
C
      IF( (NOREPR.NE.JPREGROT).AND.(NOREPR.NE.JPREGULAR) ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVU: For U/V, only regular lat/long',JPQUIET)
        CALL INTLOG(JP_ERROR,
     X    'INTUVU: output rotated grids allowed',JPQUIET)
        INTUVU  = JPROUTINE + 6
        GOTO 900
      ENDIF
C
cs this is for merging with grib_api
      LUVCOMP = .TRUE.
      CALL INTLOG(JP_DEBUG,'INTUVU: Rotate the U & V fields',JPQUIET)
      CALL INTLOG(JP_DEBUG,'INTUVU: South pole lat  ',NOROTA(1))
      CALL INTLOG(JP_DEBUG,'INTUVU: South pole long ',NOROTA(2))
C
C     Fill area limits (handles case when default 0/0/0/0 given)
C
cssssssssss
cs      IERR = FIXAREA()
cs      IF( IERR.NE.0 ) THEN
cs        CALL INTLOG(JP_ERROR,'INTUVU: area fixup failed',JPQUIET)
cs        INTUVU = JPROUTINE + 6
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
C*    Section 7.   Convert spectral to suitable global reduced gaussian
C     -----------------------------------------------------------------|
C
  800 CONTINUE
C
      NTRUNC = NIRESO
      NGAUSS = 0
      HTYPE  = ''
      NS = 0.
      EW = 0.
      IERR = HSH2GG(NS,EW,NTRUNC,NGAUSS,HTYPE,KPTS,GLATS,ISIZE)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVU: problem getting data for reduced grid',NTRUNC)
        INTUVU = JPROUTINE + 7
        GOTO 900
      ENDIF
      NCOUNT = ISIZE
C
C     Dynamically allocate memory for global reduced gaussian grid
C
      CALL JMEMHAN( 18, IRGGRID, (NCOUNT*2), 1, IERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVU: memory alloc for reduced grid fail',JPQUIET)
        INTUVU = JPROUTINE + 7
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
      CALL JAGGGP(UV(IP_U),NTRUNC,GLATS(1),GLATS(NGAUSS*2),WEST,
     X            EAST,NGAUSS,HTYPE,KPTS,RGGRID,NUVFLAG,IERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVU: spectral to reduced gaussian failed',JPQUIET)
        INTUVU = JPROUTINE + 7
        GOTO 900
      ENDIF
C
      HOGAUST = HOLDTYP
C
C     V component
C
      CALL JAGGGP(UV(IP_V),NTRUNC,GLATS(1),GLATS(NGAUSS*2),WEST,
     X            EAST,NGAUSS,HTYPE,KPTS,RGGRID(1+NCOUNT),NUVFLAG,IERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVU: spectral to reduced gaussian failed',JPQUIET)
        INTUVU = JPROUTINE + 7
        GOTO 900
      ENDIF

      HOGAUST = HOLDTYP


C     -----------------------------------------------------------------|
C*    Section 8.   Rotate using 12-point horizontal interpolation
C     -----------------------------------------------------------------|
C
  810 CONTINUE
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
      CALL JMEMHAN( 11, ISWORK, ISIZE, 1, IERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVU: memory alloc for lat/long grid fail',JPQUIET)
        INTUVU = JPROUTINE + 8
        GOTO 900
      ENDIF
C
      IERR = HIRLAMW(LO12PT,RGGRID,RGGRID(1+NCOUNT),NCOUNT,NGAUSS,HTYPE,
     X  AREA,POLE,GRID,SWORK,SWORK(1+NUMPTS),NUMPTS,NLON,NLAT)
C
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTUVU: HIRLAMW rotation failed',JPQUIET)
        INTUVU = JPROUTINE + 8
        GOTO 900
      ENDIF
c
      DO LOOP = 1, NUMPTS
          PUOUT(LOOP) = SWORK(LOOP)
          PVOUT(LOOP) = SWORK(LOOP+NUMPTS)
      ENDDO

      OUTLEN = NUMPTS

cs       GOTO 900
       GOTO 890
c     -----------------------------------------------------------------|
C*    Section 8.1   Grid to rotated grid  point
C     -----------------------------------------------------------------|
  850 CONTINUE
C*    8.1a   Generate interpolated lat/long U and V fields.
C     -----------------------------------------------------------------|
C
cs this is for merging with grib_api
      LUVCOMP = .TRUE.
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
     X    'INTUVU: Rotate reduced gaussian to lat/long',JPQUIET)
          IERR = HIRLAMW(LO12PT,
     X                   PVYIN,PDVIN,INLEN,
     X                   NOGAUSS,HTYPE,AREA,POLE,GRID,
     X                   PUOUT,PVOUT,OUTLEN,NLON,NLAT)
          IF( IERR.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTUVU: HIRLAMW rotation failed',JPQUIET)
            INTUVU = JPROUTINE + 8
            GOTO 900
          ENDIF
C
C       Rotate lat/long to lat/long
C
        ELSE
        CALL INTLOG(JP_DEBUG,
     X    'INTUVU: Rotate lat/long to lat/long',JPQUIET)
          OLDGRID(1) = REAL(NIGRID(1)) / PPMULT
          OLDGRID(2) = REAL(NIGRID(2)) / PPMULT
          IERR = HLL2LLW(LO12PT,PVYIN,PDVIN,
     X                   OLDGRID,AREA,POLE,GRID,
     X                   PUOUT,PVOUT,OUTLEN,NLON,NLAT)
          IF( IERR.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTUVU: HLL2LLW rotation failed',JPQUIET)
            INTUVU = JPROUTINE + 8
            GOTO 900
          ENDIF
        ENDIF
C
      ELSE

*    Section 8.1b   Generate interpolated gaussian U and V fields.
C     -----------------------------------------------------------------|
C
C
        CALL INTLOG(JP_DEBUG,
     X    'INTUVU: Rotate gaussian to gaussian',JPQUIET)
C
C       Dynamically allocate memory for rotated gaussian grids
C
        NUMPTS = NOGAUSS * NOGAUSS
        OUTLEN = 2 * NUMPTS * 8
C
cs        NGAUSS = ISEC2(10)
        IERR = HRG2GGW(LO12PT,
     X                 PVYIN,PDVIN,INLEN,
     X                 NIGAUSS,AREA,POLE,NOGAUSS,HOGAUST,
     X                 PUOUT,PVOUT,OUTLEN,NUMPTS)
        IF( IERR.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X        'INTUVU: HRG2GGW rotation failed',JPQUIET)
            INTUVU = JPROUTINE + 8
            GOTO 900
          ENDIF
C
      ENDIF
C
  890 CONTINUE
      IF( LFRAME ) THEN
        NLON = 1 + NINT(FLOAT(NOAREA(JPEAST)  - NOAREA(JPWEST)) /
     X         NOGRID(JPWESTEP))
        NLAT = 1 + NINT(FLOAT(NOAREA(JPNORTH) - NOAREA(JPSOUTH)) /
     X         NOGRID(JPNSSTEP))
        ISEC1(5) = 192
        ISEC3(2) = NINT(RMISSGV)
        ZSEC3(2) = RMISSGV
        LIMISSV = .TRUE.
        CALL MKFRAME(NLON,NLAT,PUOUT,RMISSGV,NOFRAME)
        CALL MKFRAME(NLON,NLAT,PVOUT,RMISSGV,NOFRAME)
      ENDIF

C
C
C     -----------------------------------------------------------------|
C*    Section 9.   Closedown.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
C     Clear change flags for next product processing
      LCHANGE = .FALSE.
      LSMCHNG = .FALSE.
      LWINDSET = LOLDWIND
      DO 910 LOOP = 1, 4
        NOAREA(LOOP) = IOHOLD(LOOP)
  910 CONTINUE
C
      NIRESO = NOLD
C
      RETURN
      END
