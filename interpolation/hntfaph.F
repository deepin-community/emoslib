C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HNTFAPH(INGRIB,INLEN)
C
C---->
C**** HNTFAPH
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
C     IRET = HNTFAPH(INGRIB,INLEN)
C
C     Input
C     -----
C
C     INGRIB - Input field (packed).
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
C     Unpack field.
C
C     If the input is a spectral field and the output is a rotated
C     grid-point field, create a global reduced gaussian field and
C     then create the rotated grid-point field from it.
C
C
C     Externals
C     ---------
C
C     GRIBEX  - Decode/encode GRIB product.
C     GRSVCK  - Turn off GRIB checking
C     INTLOG  - Log error message.
C     INTLOGR - Log error message.
C     JDEBUG  - Checks environment to switch on/off debug
C     FIXAREA - Fixup input/output field area definitions.
C     HSH2GG  - Find suitable gaussian grid/spectral truncation
C     HIRLAM  - Creates rotated lat/long field from reduced gaussian
C     HIRLSM  - Creates rotated lat/long field from reduced gaussian
C               using land-sea mask
C     HRG2GG  - Creates rotated gaussian field from reduced gaussian
C     HLL2LL  - Creates rotated lat/long field from lat/long field
C     LSMFLD  - Determines whether a field is to be interpolated using
C               a land-sea mask
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     January 31, 2001
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
      INTEGER INGRIB(*),INLEN
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
      PARAMETER (JPROUTINE = 40110 )
C
C     Local variables
C
      CHARACTER*1 HOLDTYP
      CHARACTER*1 HTYPE
      INTEGER     IERR
      INTEGER     IRET
      INTEGER     ISHIZE
      INTEGER     ISIZE
      INTEGER     ITEMP,K,I,J,IM,JM
      INTEGER     IWORD
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
      REAL        OLDGRID(2)
      REAL        PLATS(JPGTRUNC*2)
      REAL        POLE(2)
      REAL        RGGRID(1)
      REAL        SWORK(1)
      REAL        TEMP(1440,1440)
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
C     Externals
C
      CHARACTER*1 GGHTYPE
      INTEGER HSH2GG, HIRLAM, HIRLSM, HRG2GG, FIXAREA, PDDEFS
      INTEGER HLL2LL
      LOGICAL LSMFLD
      EXTERNAL GGHTYPE
      EXTERNAL HSH2GG, HIRLAM, HIRLSM, HRG2GG, FIXAREA, PDDEFS
      EXTERNAL HLL2LL
      EXTERNAL LSMFLD
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      HNTFAPH = 0
      LSP2RGG = .FALSE.
C
C     -----------------------------------------------------------------|
C*    Section 2.   Decode data from the GRIB code
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Decode data from GRIB code (no checking)
C
      IERR  =  0
      CALL GRSVCK(0)
C
      IERR = 1
      IWORD = INLEN
      ISEC3(2) = NINT(RMISSGV)
      ZSEC3(2) = RMISSGV

      CALL GRIBEX(ISEC0, ISEC1, ISEC2, ZSEC2, ISEC3, ZSEC3, ISEC4,
     X            ZNFELDI, JPEXPAND, INGRIB, INLEN, IWORD, 'D',IERR)

      IF (ISEC2(11).EQ.64) THEN
        CALL INTLOG(JP_DEBUG,
     X    'HNTFAPH: Scanning flag west-east/south-north',ISEC2(11))
        ITEMP = NIAREA(1)
        NIAREA(1) = NIAREA(3)
        NIAREA(3) = ITEMP

        IM = ISEC2(2)
        JM = ISEC2(3)
        K=0
        DO J=JM,1,-1
          DO I=1,IM
            K=K+1
            TEMP(I,J) = ZNFELDI(K)
          END DO
        END DO
        K=0
        DO J=1,JM
          DO I=1,IM
            K=K+1
            ZNFELDI(K) = TEMP(I,J)
          END DO
        END DO

      ENDIF

C
      IF( IERR.LT.0) THEN
        IF( (IERR.EQ.-2).OR.(IERR.EQ.-4) ) THEN
          CALL INTLOG(JP_DEBUG,'HNTFAPH: Use missing value',JPQUIET)
          LIMISSV = .TRUE.
        ELSE
          CALL INTLOG(JP_ERROR,'HNTFAPH: GRIBEX decoding fail.',IERR)
          HNTFAPH = JPROUTINE + 2
          GOTO 900
        ENDIF
      ELSE IF( IERR.GT.0 ) THEN
        CALL INTLOG(JP_ERROR,'HNTFAPH: GRIBEX decoding failed.',IERR)
        HNTFAPH = JPROUTINE + 2
        GOTO 900
      ENDIF
C
      NCOUNT = ISEC4(1)
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
      CALL INTLOG(JP_DEBUG,'HNTFAPH: Rotate field.',JPQUIET)
      CALL INTLOG(JP_DEBUG,'HNTFAPH: South pole lat  ',NOROTA(1))
      CALL INTLOG(JP_DEBUG,'HNTFAPH: South pole long ',NOROTA(2))
C
C     Fill area limits (handles case when default 0/0/0/0 given)
C
      IRET = FIXAREA()
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'HNTFAPH: area fixup failed',JPQUIET)
        HNTFAPH = JPROUTINE + 3
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
      IF( (NIREPR.EQ.JPSPHERE).OR.(NIREPR.EQ.JPSPHROT) ) THEN
C
C       Convert spectral to suitable global reduced gaussian grid
C
        CALL INTLOG(JP_DEBUG,
     X    'HNTFAPH: Spectral to suitable reduced gaussian',JPQUIET)
C
        NIRESO = ISEC2(2)
        NTRUNC = ISEC2(2)
        IF( LNORESO ) THEN
          NTRUNC = NORESO
          NGAUSS = 0
          HTYPE  = ''
          NS = 0.
          EW = 0.
          IRET = HSH2GG(NS,EW,NTRUNC,NGAUSS,HTYPE,KPTS,PLATS,ISIZE)
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'HNTFAPH: problem getting data for reduced grid',NTRUNC)
            HNTFAPH = JPROUTINE + 4
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
     X        'HNTFAPH: problem getting data for reduced grid',NTRUNC)
            HNTFAPH = JPROUTINE + 4
            GOTO 900
          ENDIF
        ENDIF

C       Truncate if a smaller resolution has been requested
C
  401 CONTINUE
      IF( NTRUNC.LT.NIRESO ) THEN
       CALL INTLOG(JP_DEBUG,'HNTFAPH: Truncation changed from:',NIRESO)
       CALL INTLOG(JP_DEBUG,'HNTFAPH: to: ',NTRUNC)
       CALL INTLOG(JP_DEBUG,'HNTFAPH: Gaussian number is:',NGAUSS)
C
        ISHIZE =  (NTRUNC+1)*(NTRUNC+4)
        CALL JMEMHAN( 3, IZNFLDO, ISHIZE, 1, IERR)
        IF( IERR.NE.0 ) THEN
          CALL INTLOG(JP_FATAL,
     X      'HNTFAPH: Get scratch space failed',JPQUIET)
          HNTFAPH = JPROUTINE + 4
          GOTO 900
        ENDIF

C       Generate spherical harmonics with output truncation
        CALL SH2SH( ZNFELDI, NIRESO, ZNFLDO, NTRUNC )

C       Move new spherical harmonics to 'input' array
        ZNFELDI(1:ISHIZE) = ZNFLDO(1:ISHIZE)

      ELSE
          CALL INTLOG(JP_DEBUG,
     X      'HNTFAPH: Spectral to suitable reduced gaussian',JPQUIET)
C
          NTRUNC = ISEC2(2)
          NGAUSS = 0
          HTYPE  = ''
          NS = 0.
          EW = 0.
          IRET = HSH2GG(NS,EW,NTRUNC,NGAUSS,HTYPE,KPTS,PLATS,ISIZE)
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'HNTFAPH: problem getting data for reduced grid',NTRUNC)
            HNTFAPH = JPROUTINE + 4
            GOTO 900
          ENDIF

        ENDIF

C
C       Dynamically allocate memory for global reduced gaussian grid
C
        CALL JMEMHAN( 18, IRGGRID, ISIZE, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'HNTFAPH: memory alloc for reduced grid fail',JPQUIET)
          HNTFAPH = JPROUTINE + 4
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
     X      'HNTFAPH: spectral to reduced gaussian failed',JPQUIET)
          HNTFAPH = JPROUTINE + 4
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
     X      'HNTFAPH: Convert gaussian to rotated gaussian',JPQUIET)
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
      IF( (LLATOUT) .AND. (
     X        (NIREPR.EQ.JPQUASI) .OR.
     X        (NIREPR.EQ.JPGAUSSIAN) .OR.
     X        (LSP2RGG) )) THEN
C
        CALL INTLOG(JP_DEBUG,'HNTFAPH: Gauss to lat/lon',JPQUIET)
C
C       Dynamically allocate memory for rotated lat/long grid
C
        NLON = 1 + NINT((AREA(JPEAST)  - AREA(JPWEST)) /
     X         GRID(JPWESTEP)) !SC
        NLAT = 1 + NINT((AREA(JPNORTH) - AREA(JPSOUTH)) /
     X         GRID(JPNSSTEP)) !SC
C
        NOWE = NLON
        NONS = NLAT
C
        NUMPTS = NLON * NLAT
        ISIZE  = NUMPTS
        CALL JMEMHAN( 11, ISWORK, ISIZE, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'HNTFAPH: memory alloc for lat/long grid fail',JPQUIET)
          HNTFAPH = JPROUTINE + 5
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
          NGAUSS = ISEC2(10)
          HTYPE  = GGHTYPE(NIREPR,NGAUSS,MILLEN)
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
     X      'HNTFAPH: HIRLAM rotation failed',JPQUIET)
          HNTFAPH = JPROUTINE + 7
          GOTO 900
        ENDIF
C
        ISEC2(1) = JPREGROT
        ISEC4(1) = NOWE * NONS
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
     X    (LSP2RGG.AND.(NOREPR.EQ.JPQGGROT)) .OR.
     X    (((NIREPR.EQ.JPQUASI).OR.(NIREPR.EQ.JPGAUSSIAN)) .AND.
     X     (NOREPR.EQ.JPFGGROT).OR.(NOREPR.EQ.JPQGGROT)) ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'HNTFAPH: Gaussian to reduced gaussian',JPQUIET)
C
C       Dynamically allocate memory for rotated lat/long grid
C
C       ISIZE = NOGAUSS * NOGAUSS * 8
        ISIZE = (2*NOGAUSS) * (4*NOGAUSS + 20)  ! account for RGG/octahedral
        CALL JMEMHAN( 11, ISWORK, ISIZE, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'HNTFAPH: memory alloc for gaussian grid fail',JPQUIET)
          HNTFAPH = JPROUTINE + 6
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
          NGAUSS = ISEC2(10)
          IRET = HRG2GG(LO12PT,ZNFELDI,NGAUSS,AREA,POLE,
     X                  NOGAUSS,HOGAUST,SWORK,ISIZE,NUMPTS)
        ENDIF
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'HNTFAPH: HRG2GG rotation failed',JPQUIET)
          HNTFAPH = JPROUTINE + 6
          GOTO 900
        ENDIF
C
        IF( (NOREPR.EQ.JPQUASI).OR.
     X      (NOREPR.EQ.JPQGGROT).OR.
     X      (NOREPR.EQ.JPFGGROT).OR.
     X      (NOREPR.EQ.JPGAUSSIAN) ) THEN
          ISEC2(1) = JPFGGROT
        ELSE
          ISEC2(1) = NOREPR
        ENDIF
C
        ISEC4(1) = NUMPTS
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 7.   Lat/long to rotated lat/long
C     -----------------------------------------------------------------|
C
  700 CONTINUE
C
      IF( (NIREPR.EQ.JPREGULAR) ) THEN
C
C       Dynamically allocate memory for rotated lat/long grid
C
        NLON = 1 + NINT((AREA(JPEAST)  - AREA(JPWEST)) /
     X         GRID(JPWESTEP))
        NLAT = 1 + NINT((AREA(JPNORTH) - AREA(JPSOUTH)) /
     X         GRID(JPNSSTEP))
C
        NOWE = NLON
        NONS = NLAT
C
        NUMPTS = NLON * NLAT
        ISIZE  = NUMPTS
        CALL JMEMHAN( 11, ISWORK, ISIZE, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'HNTFAPH: memory alloc for lat/long grid fail',JPQUIET)
          HNTFAPH = JPROUTINE + 7
          GOTO 900
        ENDIF
C
        OLDGRID(1) = REAL(NIGRID(1))/100000.0
        OLDGRID(2) = REAL(NIGRID(2))/100000.0
        IRET = HLL2LL(LO12PT,ZNFELDI,OLDGRID,AREA,POLE,GRID,SWORK,ISIZE,
     X                NLON,NLAT)
C
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'HNTFAPH: HLL2LL rotation failed',JPQUIET)
          HNTFAPH = JPROUTINE + 7
          GOTO 900
        ENDIF
C
        ISEC2(1) = JPREGROT
        ISEC4(1) = NOWE * NONS
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
