C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INTFB(INGRIB,INLEN,OUTGRIB,OUTLEN,FLDOUT)
C
C---->
C**** INTFB
C
C     Purpose
C     -------
C
C     Interpolate input field...
C
C
C     Interface
C     ---------
C
C     IRET = INTFB( INGRIB,INLEN,OUTGRIB,OUTLEN,FLDOUT)
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
C     OUTGRIB - Output field (packed).
C     OUTLEN  - Output field length (words).
C     FLDOUT  - Output field (unpacked).
C
C
C     Method
C     ------
C
C     Call interpolation routines; then repack if GRIB output.
C
C     Currently, subareas are not supported for reduced gaussian fields.
C     (Works OK; but need to setup values in GRIB product for number of
C      points in each latitude row)
C
C
C     Externals
C     ---------
C
C     IARCNTL - Control interp. quasi reg.Gauss -> Gaussian or lat/long
C     IAGCNTL - Control interpolation reg.Gaussian or lat/long fields.
C     JALLGP  - Converts spectral fields to lat/long grid fields.
C     JALLWN  - Converts spectral wind fields to lat/long grids.
C     JSLLGP  - Converts strectched spectral fields to lat/long grid fields.
C     JAGGGP  - Converts spectral fields to a gaussian grid field.
C     JSGGGP  - Converts strectched spectral fields to a gaussian grid field.
C     GRIBEX  - Decode/encode GRIB product.
C     IGSETUP - Setup GRIB sections for the output product.
C     JMEMHAN - Handles memory allocation.
C     IGLSIZE - Evaluate array sizes for reg. lat/long and area.
C     IGSIZE  - Evaluate array sizes for a regular Gaussian.
C     IRSIZE  - Evaluate array sizes for a quasi reg Gaussian.
C     FIXAREA - Fixup area definition to correspond to grid definitions
C     ISCRSZ  - Calculate number of values in generated field.
C     JDEBUG  - Checks environment to switch on/off debug
C     INTLOG  - Log error message.
C     OCEANP  - Interpolate GRIB ocean field.
C     MKBITMP - Apply a bitmap to a rectangular field.
C     MKFRAME - Create a 'frame' from a rectangular field.
C     DDSTYLE - Create subgrid by selecting pts from rectangular grid.
C     DSSAREA - Fixup field area (dissemination style).
C     INTISL  - Interpolate a field using ISLSCP processing.
C     KRG2RG  - Interpolate reduced gaussian to reduced gaussian (packed)
C     KRG2RGU - Interpolate reduced gaussian to reduced gaussian (unpacked)
C     KRG2RGD - Interpolate reduced gaussian to reduced gaussian
c               using intermediate interpolation
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     Aug 1994
C
C     J.D.Chambers     ECMWF        Feb 1997
C     Allow for 64-bit pointers
C
C----<
C
C     IMPLICIT NONE
C
C     Function arguments
C
      INTEGER INGRIB(*),OUTGRIB(*),INLEN,OUTLEN
      REAL FLDOUT(*)
C
#include "parim.h"
#include "nifld.common"
#include "nifld.extracommon"
#include "nofld.common"
#include "grfixed.h"
#include "intf.h"
#include "jparams.h"
C
C     Parameters
C
      INTEGER JPROUTINE
      INTEGER JUNPACK
      PARAMETER (JPROUTINE = 26400 )
      PARAMETER (JUNPACK = 3000000)
C
C     Local variables
C
      REAL EW, NS, NORTH, SOUTH, WEST, EAST, RTEMP
      INTEGER IWORD, IERR, ILENF, ISIZE, IUV, IDISTEP
      REAL PNORTH, PSOUTH, PWEST, PEAST, PLATINC, PLONINC
      LOGICAL OIWEGLOBE, OINPOLE, OISPOLE
      LOGICAL OOWEGLOBE, OONPOLE, OOSPOLE
      LOGICAL LBITMP,LFRAME,LSTYLE,LGLOBAL,LSAMERS,LIQUASI
      LOGICAL LOLDLSP, LOLDLSM
      INTEGER INORTH, ISOUTH, IEAST
      INTEGER HOLDEW, HOLDNS, N_NOWE, N_NONS
      INTEGER KILN, KITOTAL, KOLN, KOTOTAL, KPR, KERR
      INTEGER LOOP, LOOPI, NEXT, ISWAP, NTRUNC, NTROLD, NTROLD2
      INTEGER LENMED
      INTEGER ILENS
      REAL*8 DISSRES
C
      DATA NTROLD/-1/, NTROLD2/-1/
      SAVE NTROLD, NTROLD2
      CHARACTER*1 HTYPE
      CHARACTER*1 HFUNC
#ifndef _CRAYFTN
#ifdef POINTER_64
#ifndef FUJITSU
      INTEGER*8 IZNFLDO
#endif
#endif
#endif
      REAL ZNFLDO , RWORK
      DIMENSION RWORK(JUNPACK)
      POINTER ( IZNFLDO, ZNFLDO )
      DIMENSION ZNFLDO( 1 )
      INTEGER NEWLSM(1), OLDLSM(1)
      POINTER( NIPNGRB, NEWLSM )
      POINTER( NIPOGRB, OLDLSM )
      LOGICAL LFIRST
      DATA LFIRST/.TRUE./
      SAVE LFIRST
C
C     Externals
C
      INTEGER IGLSIZE, IGSIZE, IRSIZE, IARCNTL, IAGCNTL, DSSAREA
      INTEGER IGSETUP, FIXAREA, ISCRSZ, AURESOL, MKBITMP !, OCEANP
      INTEGER INTISL, KRG2RG, KRG2RGU,KRG2RGZ, KRG2RGD, KRG2RGY
      CHARACTER*1 GGHTYPE
      EXTERNAL GGHTYPE
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
      HOLDEW = 0
      HOLDNS = 0
      IERR   = 0
      N_NONS = 0
      N_NOWE = 0
      INTFB = 0
C
C     Check if debug option turned on
      CALL JDEBUG()
      KPR = NDBG
C
      LBITMP = LNOBITMP.AND.
     X         ((NOREPR.EQ.JPREGROT).OR.(NOREPR.EQ.JPREGULAR).OR.
     X          (NOREPR.EQ.JPGAUSSIAN))
C
      LFRAME = LNOFRAME.AND.
     X         ((NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPGAUSSIAN).OR.
     X          (NOREPR.EQ.JPREGROT ).OR.(NOREPR.EQ.JPFGGROT  ) )
C
      LSTYLE = LNOSTYLE.AND.
     X         (NOSTYLE.EQ.JPSDISM).AND.
     X         (NOREPR.EQ.JPREGULAR)
C
      IF( .NOT.LWINDSET ) THEN
        LWIND = ( ((NOTABLE.EQ.128).OR.(NOTABLE.EQ.129)).AND.
     X            ((NOPARAM.EQ.131).OR.(NOPARAM.EQ.132).OR.
     X            (NOPARAM.EQ.165).OR.(NOPARAM.EQ.166)) )
      ENDIF
      CALL INTLOG(JP_DEBUG,'INTFB: Wind flag = ', LWIND)
      IF( LWIND ) THEN
        IUV = 1
      ELSE
        IUV = 0
      ENDIF
C
        IF(NIFORM.EQ.1) THEN
          ILENS = ISEC4(1)
        ELSE
          ILENS = INLEN
        ENDIF

C
C     If dissemination style selection of grid points is required,
C     adjust the sub-area limits to the dissemination grid.
C
      IF( LSTYLE ) THEN
        IDISTEP = NINT(JPDISTP*PPMULT)
        EW    = NOGRID(1) / PPMULT
        NS    = NOGRID(2) / PPMULT
        NORTH = REAL(NOAREA(1)) / PPMULT
        WEST  = REAL(NOAREA(2)) / PPMULT
        SOUTH = REAL(NOAREA(3)) / PPMULT
        EAST  = REAL(NOAREA(4)) / PPMULT
C
        IERR = DSSAREA( EW, NS, NORTH, WEST, SOUTH, EAST)
        IF( IERR.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFB: DSSAREA failed:',IERR)
          INTFB = IERR
          GOTO 900
        ENDIF
C
        N_NOWE = NINT((EAST-WEST)/EW) + 1
        N_NONS = NINT((NORTH-SOUTH)/NS) + 1
C
        NOAREA(1) = NINT(NORTH * PPMULT)
        NOAREA(2) = NINT(WEST  * PPMULT)
        NOAREA(3) = NINT(SOUTH * PPMULT)
        NOAREA(4) = NINT(EAST  * PPMULT)
      ENDIF
C
C     If Hirlam 12-point rotation involved, it has already been done
C     (in INTFAPH), so just copy values to array for packing.
C
      IF( LUSEHIR.AND.LNOROTA ) THEN
        ISIZE = ISEC4(1)
        IF( ISIZE.EQ.0 ) ISIZE = OUTLROT
        CALL JMEMHAN( 3, IZNFLDO, ISIZE, 1, IERR)
        IF( IERR.NE.0 ) THEN
          CALL INTLOG(JP_FATAL,'INTFB: Get scratch space fail',JPQUIET)
          INTFB = JPROUTINE + 1
          GOTO 900
        ENDIF
C
        DO LOOP = 1, ISIZE
          ZNFLDO(LOOP) = ZNFELDI(LOOP)
        ENDDO
        GOTO 700
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2.   Special cases!
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Allocate work array ZNFELDI if not already done.
C
      IF( IZNJDCI.NE.1952999238 ) THEN
        CALL JMEMHAN( 19, IZNFLDI, JPEXPAND, 1, ierr)
        IF( IERR.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFB: znfeldi allocate fail',JPQUIET)
          INTFB = IERR
          GOTO 900
        ENDIF
        IZNJDCI = 1952999238
      ENDIF
C
C     Is the interpolation from SH to grid point
C
      IF( ((NIREPR.EQ.JPSPHERE).OR.(NIREPR.EQ.JPSPHROT).OR.
     X     (NIREPR.EQ.JPSTRSH))
     X    .AND.
     X    ((NOREPR.NE.JPSPHERE).AND.(NOREPR.NE.JPSPHROT)) ) THEN
C
        NTRUNC = NORESO
C
C       Check whether a truncation is required before interpolation
C
        IF( .NOT.LNORESO ) THEN
C
C         If user did not supply a truncation value, see whether or
C         not the 'autoresol' flag is set. If not, use the input
C         truncation.
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
     X          'INTFB: Resolution automatically set to ', NTRUNC)
            ENDIF
          ELSE
            NTRUNC = NIRESO
          ENDIF
C
        ELSE
C
C
        ENDIF
C
C       Check whether the output resolution is greater than the input
C
        IF( NTRUNC.GT.NIRESO ) THEN
C
C         Issue warning if the output resolution was user-supplied
C
          IF( .NOT.LARESOL ) THEN
C
C           Revert to the input truncation
C
            IF( NIRESO.NE.NTROLD2 ) THEN
              CALL INTLOG(JP_WARN,
     X          'INTFB: spectral -> grid point interpolation',JPQUIET)
              CALL INTLOG(JP_WARN,
     X          'INTFB: User supplied resolution = ',NTRUNC)
              CALL INTLOG(JP_WARN,
     X          'INTFB: Input field resolution   = ',NIRESO)
              CALL INTLOG(JP_WARN,
     X          'INTFB: User supplied resolution ignored',JPQUIET)
              CALL INTLOG(JP_WARN,
     X          'INTFB: Input field resolution has been used',JPQUIET)
              NTROLD2 = NIRESO
            ENDIF
            NTRUNC = NIRESO
C
          ELSE
C
C           Revert to the input truncation if auto RESOL
C
            NTRUNC = NIRESO
            IF( NTRUNC.NE.NTROLD2 ) THEN
              NTROLD2 = NTRUNC
              CALL INTLOG(JP_WARN,
     X          'INTFB: Automatic resolution selectn too high:',JPQUIET)
              CALL INTLOG(JP_WARN,
     X          'INTFB: Resolution reset to input resolution: ',NTRUNC)
            ENDIF
          ENDIF
C
        ENDIF
C
C       Truncate if a smaller resolution has been requested
C
        IF( NTRUNC.LT.NIRESO ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'INTFB: Spectral coefficents truncated to ', NTRUNC)
C
          ISIZE =  (NTRUNC+1)*(NTRUNC+4)
          CALL JMEMHAN( 3, IZNFLDO, ISIZE, 1, IERR)
          IF( IERR.NE.0 ) THEN
            CALL INTLOG(JP_FATAL,
     X        'INTFB: Get scratch space failed',JPQUIET)
            INTFB = JPROUTINE + 2
            GOTO 900
          ENDIF
C
C         Generate spherical harmonics with output truncation
C
          CALL SH2SH( ZNFELDI, NIRESO, ZNFLDO, NTRUNC )
C
C         Move new spherical harmonics to 'input' array
C
          DO LOOP = 1, ISIZE
            ZNFELDI(LOOP) = ZNFLDO(LOOP)
          ENDDO
C
          NIRESO = NTRUNC
C
        ENDIF
C
      ENDIF
C
C     Is it an ocean field interpolation?
C
c sinisa this is all done in intf
c      IF( (ISEC1(24).EQ.1)       .AND.
c     X    (ISEC1(37).EQ.4)       .AND.
c     X    (NIFORM.EQ.1)          .AND.
c     X    (NOFORM.EQ.1) ) THEN
c        IERR = OCEANP(INGRIB,INLEN,OUTGRIB,OUTLEN)
c        IF( IERR.NE.0 ) THEN
c          CALL INTLOG(JP_ERROR,
c     X      'INTFB: Ocean interpolation failed.',JPQUIET)
c          INTFB = JPROUTINE + 2
c        ELSE
c          CALL INTLOG(JP_DEBUG,
c     X      'INTFB: Ocean interpolation finished successfully.',JPQUIET)
c        ENDIF
c        GOTO 900
c      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3.   Interpolation spherical harmonics to spherical
C                  harmonics
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      IF( ((NIREPR.EQ.JPSPHERE).OR.(NIREPR.EQ.JPSPHROT)) .AND.
     X    ((NOREPR.EQ.JPSPHERE).OR.(NOREPR.EQ.JPSPHROT) ) ) THEN
C
C       Get scratch space for interpolation
C
        CALL INTLOG(JP_DEBUG,'INTFB: SH -> SH, input is ',NIRESO)
        CALL INTLOG(JP_DEBUG,'INTFB:          output is ',NORESO)
        ISIZE = ISCRSZ()
        IF( ISIZE.LE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFB: Get scratch space failed.',ISIZE)
          INTFB = JPROUTINE + 2
          GOTO 900
        ENDIF
        CALL JMEMHAN( 3, IZNFLDO, ISIZE, 1, IERR)
        IF( IERR.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFB: Get scratch space fail.',JPQUIET)
          INTFB = JPROUTINE + 2
          GOTO 900
        ENDIF
C
        CALL SH2SH( ZNFELDI, NIRESO, ZNFLDO, NORESO )
C
        GOTO 700
C
C     If not spectral -> spectral transformation, make sure the input
C     and output area definitions have been setup.
C
      ELSE
C
C       Fixup area definition to correspond to grid definitions
C
        IF( .NOT.LSTYLE ) THEN
          IERR = FIXAREA()
          IF( IERR.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTFB: Fixup area definition failed.',JPQUIET)
            INTFB = JPROUTINE + 9
            GOTO 900
          ENDIF
        ENDIF
C
C       Setup geographical limits
C
        PNORTH = FLOAT( NOAREA(1) ) / PPMULT
        PWEST  = FLOAT( NOAREA(2) ) / PPMULT
        PSOUTH = FLOAT( NOAREA(3) ) / PPMULT
        PEAST  = FLOAT( NOAREA(4) ) / PPMULT
C
      ENDIF
C
C     -----------------------------------------------------------------|
C     Section 4.   Interpolation spherical harmonics to grid point.
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
      IF( (NIREPR.EQ.JPSPHERE).OR.
     X    (NIREPR.EQ.JPSTRSH) .OR.
     X    (NIREPR.EQ.JPSPHROT) ) THEN
#ifndef REAL_8
C
C       Check that double precision floats are in use if spectral
C       resolution is greater than T213
C
        IF( NIRESO.GT.213 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTFB: Double precision must be used',JPQUIET)
          CALL INTLOG(JP_ERROR,'INTFB: for spectral resolution ',NIRESO)
          INTFB = JPROUTINE + 2
          GOTO 900
        ENDIF
#endif
        CALL INTLOG(JP_DEBUG,'INTFB: SH -> grid point',JPQUIET)
C
C       Get scratch space for interpolation
C       If dissemination style, space has to be for a 0.5x0.5 grid
C
        IF( LSTYLE ) THEN
c to be consistant with product generation
           IF(NIRESO.EQ.1279) THEN
              DISSRES = 0.125
           ELSEIF(NIRESO.EQ.799) THEN
              DISSRES = 0.25
           ELSEIF(NIRESO.EQ.639) THEN
              DISSRES = 0.25
           ELSEIF(NIRESO.EQ.511) THEN
              DISSRES = 0.5
           ELSEIF(NIRESO.EQ.399) THEN
              DISSRES = 0.5
           ELSEIF(NIRESO.EQ.319) THEN
              DISSRES = 0.5
           ELSEIF(NIRESO.EQ.255) THEN
              DISSRES = 0.5
           ELSEIF(NIRESO.EQ.155) THEN
              DISSRES = 1.5
           ELSE
              DISSRES = NOGRID(1) / PPMULT
            CALL INTLOG(JP_WARN,'INTFB: Diss resol set to: ',NOGRID(1))
           ENDIF
          IDISTEP = NINT(DISSRES*PPMULT)
C
          HOLDEW = NOGRID(1)
          HOLDNS = NOGRID(2)
          NOGRID(1) = IDISTEP
          NOGRID(2) = IDISTEP
        ENDIF
        ISIZE = ISCRSZ()
        IF( ISIZE.LE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFB: Get scratch space failed.',ISIZE)
          INTFB = JPROUTINE + 4
          GOTO 900
        ENDIF
C
        CALL JMEMHAN( 3, IZNFLDO, ISIZE, 1, IERR)
        IF( IERR.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFB: Get scratch space fail.',JPQUIET)
          INTFB = JPROUTINE + 4
          GOTO 900
        ENDIF
        IF( LSTYLE ) THEN
          NOGRID(1) = HOLDEW
          NOGRID(2) = HOLDNS
        ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 4.1  Interpolation spherical harmonics to grid point.
C     -----------------------------------------------------------------|
C
  410 CONTINUE
C
        IF( (NOREPR.EQ.JPREGULAR) .OR. (NOREPR.EQ.JPREGROT) ) THEN
          CALL INTLOG(JP_DEBUG,'INTFB: SH -> lat/long',JPQUIET)
C
C         -> latitude/longitude grid
C         If dissemination style, generated grid is 0.5x0.5
C
          IF( LSTYLE ) THEN
            HOLDEW = NOGRID(1)
            HOLDNS = NOGRID(2)
            NOGRID(1) = IDISTEP
            NOGRID(2) = IDISTEP
          ENDIF
          PLONINC = FLOAT( NOGRID(1) ) / PPMULT
          PLATINC = FLOAT( NOGRID(2) ) / PPMULT
C
C         Special handling for stretched fields
C
          IF( RISTRET.NE.0 ) THEN
            CALL JSLLGP( ZNFELDI, NIRESO, PNORTH, PSOUTH, PWEST, PEAST,
     X                   PLATINC, PLONINC, ZNFLDO, IERR)
C
          ELSE
            IF( LWIND ) THEN
              CALL JALLWN(ZNFELDI, NIRESO, PNORTH, PSOUTH, PWEST, PEAST,
     X                    PLATINC, PLONINC, ZNFLDO, IERR)
            ELSE
              CALL JALLGP(ZNFELDI, NIRESO, PNORTH, PSOUTH, PWEST, PEAST,
     X                    PLATINC, PLONINC, ZNFLDO, IERR)
            ENDIF
          ENDIF
C
          IF( IERR.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTFB: Interpolation to lat/long field failed.',JPQUIET)
            INTFB = JPROUTINE + 4
            GOTO 900
          ENDIF
C
          IF( LSTYLE ) THEN
            NOGRID(1) = HOLDEW
            NOGRID(2) = HOLDNS
          ENDIF
C
          GOTO 700
C
        ENDIF
C
C
        IF ((NOREPR.EQ.JPGAUSSIAN).OR.(NOREPR.EQ.JPQUASI)) THEN
C
C         -> regular/reduced gaussian grid
C
          HTYPE = ''
          IF (NOREPR.EQ.JPGAUSSIAN) THEN
          CALL INTLOG(JP_DEBUG,'INTFB: SH -> regular gaussian',JPQUIET)
C           Is it a user_regular_gaussian (or standard regular gaussian)..
            HTYPE = 'F'
            IF (LOGIVEL) HTYPE = 'U'
          ELSE
          CALL INTLOG(JP_DEBUG,'INTFB: SH -> reduced gaussian',JPQUIET)
            HTYPE = 'R'
            IF (LOCTAHEDRAL) HTYPE = 'O'
            IF (LOGIVEP)     HTYPE = 'U'
          ENDIF
C
C         Special handling for stretched fields
C
          IF( RISTRET.NE.0 ) THEN
            CALL JSGGGP(ZNFELDI, NIRESO, PNORTH, PSOUTH, PWEST, PEAST,
     X                  NOGAUSS, HTYPE, NOLPTS, ZNFLDO, IUV, IERR)
          ELSE
            CALL JAGGGP(ZNFELDI, NIRESO, PNORTH, PSOUTH, PWEST, PEAST,
     X                  NOGAUSS, HTYPE, NOLPTS, ZNFLDO, IUV, IERR)
C
          ENDIF
          IF( IERR .NE. 0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTFB: Interpolation to gaussian field failed.',JPQUIET)
            INTFB = JPROUTINE + 4
            GOTO 900
          ENDIF
C
          GOTO 700
C
        ENDIF
C
C
C       Dropthrough => unrecognized output field type
C
        IERR  = JPROUTINE + 4
        CALL INTLOG(JP_ERROR,
     X    'INTFB: Unrecognized output field type.',NOREPR)
        INTFB = IERR
        GOTO 900
C
      ENDIF
C
C     -----------------------------------------------------------------|
C     Section 5.   Interpolate Gaussian grid to grid point.
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
C       If Hirlam 12-point rotation involved, it has already been done
C       (in INTFAPH), so just copy values to array for packing.
C
        IF( LUSEHIR.AND.LNOROTA ) THEN
C
          ISIZE = ISCRSZ()
          IF( ISIZE.LE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTFB: Get scratch space fail',ISIZE)
            INTFB = JPROUTINE + 5
            GOTO 900
          ENDIF
          CALL JMEMHAN( 3, IZNFLDO, ISIZE, 1, IERR)
          IF( IERR.NE.0 ) THEN
            CALL INTLOG(JP_FATAL,
     X        'INTFB: Get scratch space failed',JPQUIET)
            INTFB = JPROUTINE + 5
            GOTO 900
          ENDIF
C
          DO LOOP = 1, ISIZE
            ZNFLDO(LOOP) = ZNFELDI(LOOP)
          ENDDO
          GOTO 700
        ENDIF
C
C     Handle ISLSCP processing
C
      IF( LNIISCL ) THEN
        CALL INTLOG(JP_DEBUG,'INTFB: Using ISLSCP processing',JPQUIET)
        IERR = INTISL(OLDLSM, INGRIB, NEWLSM, OUTGRIB)
        IF( IERR.LT.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFB: ISLSCP processing fail',NIGAUSS)
          INTFB = JPROUTINE + 10
          GOTO 900
        ENDIF
        OUTLEN = (IERR+JPBYTES-1)/JPBYTES
        GOTO 900
      ENDIF
C
C     Reduced gaussian to reduced gaussian
C
      LIQUASI = (NIREPR.EQ.JPQUASI)
C
      IF( (LIQUASI.AND.(NOREPR.EQ.JPQUASI)) ) THEN
        IF( LDOUBLE ) THEN
          CALL INTLOG(JP_DEBUG,'INTFB: RG to RG with intermediate'
     X    ,JPQUIET)
            IF(NIFORM.EQ.1)  THEN
               IERR = KRG2RGD(INGRIB,OUTGRIB,NOMEGA,NOGAUSS)
               IF(IERR.LT.0) THEN
                 CALL INTLOG(JP_ERROR,
     X             'INTFB: KRG2RGD processing failed',JPQUIET)
                 INTFB = JPROUTINE + 10
               ELSE
                 OUTLEN = (IERR+JPBYTES-1)/JPBYTES
               ENDIF
             ELSE
               CALL INTLOG(JP_DEBUG,
     X         'INTF: RG to RG intermediate unpacked',JPQUIET)
              IERR = KRG2RGZ(INGRIB,RWORK,NIGAUSS,NOMEGA)
              IF(IERR.LT.0) THEN
                CALL INTLOG(JP_ERROR,
     X            'INTFB: KRG2RGZ processing failed',JPQUIET)
                INTFB = JPROUTINE + 10
                GOTO 900
              ELSE
                OUTLEN = IERR
              ENDIF
C             Second step
              LDOUBLE = .FALSE.
              IERR = KRG2RGZ(RWORK,FLDOUT,NOMEGA,NOGAUSS)
              LDOUBLE = .TRUE.
              IF(IERR.LT.0) THEN
                CALL INTLOG(JP_ERROR,
     X            'INTFB: KRG2RGZ processing failed',JPQUIET)
                INTFB = JPROUTINE + 10
                GOTO 900
              ELSE
                OUTLEN = IERR
              ENDIF

             ENDIF
        ELSE
            IF(NIFORM.EQ.1)  THEN
              CALL INTLOG(JP_DEBUG,'INTFB: RG to RG packed',JPQUIET)
              IERR = KRG2RG(INGRIB,OUTGRIB,NOGAUSS)
              IF(IERR.LT.0) THEN
                CALL INTLOG(JP_ERROR,
     X            'INTFB: KRG2RG processing failed',JPQUIET)
                INTFB = JPROUTINE + 10
              ELSE
                OUTLEN = (IERR+JPBYTES-1)/JPBYTES
              ENDIF
            ELSE
              CALL INTLOG(JP_DEBUG,'INTFB: RG to RG unpacked',JPQUIET)
              IERR = KRG2RGU(INGRIB,FLDOUT,NIGAUSS,NOGAUSS)
              IF(IERR.LT.0) THEN
                CALL INTLOG(JP_ERROR,
     X            'INTFB: KRG2RGU processing failed',JPQUIET)
                INTFB = JPROUTINE + 10
              ELSE
                OUTLEN = IERR
              ENDIF
            ENDIF
        ENDIF
        GOTO 900
      ENDIF
C
C     Setup flags for input and output fields
C
      CALL INTLOG(JP_DEBUG,'INTFB: grid point -> grid point.',JPQUIET)
      KERR = 1
      IF( NIREPR .EQ. JPQUASI ) THEN
        CALL INTLOG(JP_DEBUG,'INTFB: quasi input ',NIGAUSS)
        IERR = IRSIZE( NIGAUSS, NIAREA, MILLEN, NIWE, NINS, KILN,
     X                 KITOTAL, RIGAUSS, OIWEGLOBE, OINPOLE, OISPOLE,
     X                 KPR, KERR)
      ELSE IF( NIREPR.EQ.JPGAUSSIAN ) THEN
        CALL INTLOG(JP_DEBUG,'INTFB: regular gauss input ',NIGAUSS)
        IERR = IGSIZE( NIGAUSS, NIAREA, NIWE, NINS, KILN, RIGAUSS,
     X                    OIWEGLOBE, OINPOLE, OISPOLE, KPR, KERR)
      ELSE
        CALL INTLOG(JP_DEBUG,'INTFB: lat/long input lat ',NIGRID(1))
        CALL INTLOG(JP_DEBUG,'INTFB: lat/long input long ',NIGRID(2))
        IERR = IGLSIZE( NIGRID, NIAREA, NIWE, NINS, KILN, OIWEGLOBE,
     X                  OINPOLE, OISPOLE, KPR, KERR)
      ENDIF
C
      NONS = 0
      NOWE = 0
      IF( (NOREPR.EQ.JPQUASI).OR.(NOREPR.EQ.JPQGGROT) ) THEN
        CALL INTLOG(JP_DEBUG,'INTFB: quasi output ',NOGAUSS)
        IERR = IRSIZE( NOGAUSS, NOAREA, NOLPTS, NOWE, NONS, KOLN,
     X                 KOTOTAL, ROGAUSS, OOWEGLOBE, OONPOLE, OOSPOLE,
     X                 KPR, KERR)
      ELSE IF( (NOREPR.EQ.JPGAUSSIAN).OR.(NOREPR.EQ.JPFGGROT) ) THEN
        CALL INTLOG(JP_DEBUG,'INTFB: regular gauss output ',NOGAUSS)
        IERR = IGSIZE( NOGAUSS, NOAREA, NOWE, NONS, KOLN, ROGAUSS,
     X                    OOWEGLOBE, OONPOLE, OOSPOLE, KPR, KERR)
      ELSE
        CALL INTLOG(JP_DEBUG,'INTFB: lat/long output lat ',NOGRID(1))
        CALL INTLOG(JP_DEBUG,'INTFB: lat/long output long ',NOGRID(2))
C
C       If dissemination style, space has to be for a 0.5x0.5 grid
C
        IF( LSTYLE ) THEN
          IF( NIREPR.EQ.JPQUASI ) THEN
             IF(NIGAUSS.EQ.640) THEN
                DISSRES = 0.125
             ELSEIF(NIGAUSS.EQ.400) THEN
                DISSRES = 0.25
             ELSEIF(NIGAUSS.EQ.320) THEN
                DISSRES = 0.25
             ELSEIF(NIGAUSS.EQ.200) THEN
                DISSRES = 0.5
             ELSEIF(NIGAUSS.EQ.160) THEN
                DISSRES = 0.5
             ELSEIF(NIGAUSS.EQ.128) THEN
                DISSRES = 0.5
             ELSEIF(NIGAUSS.EQ.96) THEN
                DISSRES = 1.
             ELSEIF(NIGAUSS.EQ.80) THEN
                DISSRES = 1.5
             ELSE
                DISSRES = JPDISTP
               CALL INTLOG(JP_WARN,'INTFB: Diss resol set to: ',
     X          NINT(DISSRES*PPMULT))
             ENDIF
            IDISTEP = NINT(DISSRES*PPMULT)
          ENDIF
          HOLDEW = NOGRID(1)
          HOLDNS = NOGRID(2)
          If (.Not.LNOROTA) Then ! D.Jokic; Mar-2005
          NOGRID(1) = IDISTEP
          NOGRID(2) = IDISTEP
          End if ! D.Jokic; Mar-2005
        ENDIF
        IERR = IGLSIZE( NOGRID, NOAREA, NOWE, NONS, KOLN, OOWEGLOBE,
     X                  OONPOLE, OOSPOLE, KPR, KERR)
      ENDIF
C
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTFB: Setup of gaussian field flags failed.',IERR)
        INTFB = JPROUTINE + 6
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C     Section 5.1  Interpolate reduced Gaussian grid to grid point.
C     -----------------------------------------------------------------|
C
  510 CONTINUE
C
      ISIZE = ISCRSZ()
      IF( ISIZE.LE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTFB: Get scratch space failed.',ISIZE)
        INTFB = JPROUTINE + 2
        GOTO 900
      ENDIF
C
      CALL JMEMHAN( 3, IZNFLDO, ISIZE, 1, IERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTFB: Get scratch space failed.',JPQUIET)
        INTFB = JPROUTINE + 2
        GOTO 900
      ENDIF
C
      IF( NIREPR.EQ.JPQUASI ) THEN
C
C       If Hirlam 12-point rotation involved, it has already been done
C       (in INTFAPH), so just copy values to array for packing.
C
       IF( LUSEHIR.AND.LNOROTA ) THEN
         DO LOOP = 1, ISIZE
           ZNFLDO(LOOP) = ZNFELDI(LOOP)
         ENDDO
       ELSE
C        Sinisa - if double interpolation involved
        IF( LDOUBLE ) THEN
         IF( NOMEGA.EQ.NIGAUSS ) THEN
          CALL INTLOG(JP_DEBUG,
     X   'INTFB: Input and Intermed. Gaussian number are same',JPQUIET)
          CALL INTLOG(JP_DEBUG,
     X    'INTFB: Double interpolation is not performed',JPQUIET)
         ELSE
           IF(NIFORM.EQ.1)  THEN
             CALL INTLOG(JP_DEBUG,
     X      'INTFB: RG2RG to unpacked - double interpolation',JPQUIET)
               IERR = KRG2RGY(INGRIB,RWORK,NOMEGA)
               IF(IERR.LT.0) THEN
                   CALL INTLOG(JP_ERROR,
     X               'INTFB: KRG2RGY processing failed',JPQUIET)
                   INTFB = JPROUTINE + 10
                   GOTO 900
               ELSE
                   LENMED = IERR
               ENDIF
            ELSE
               CALL INTLOG(JP_DEBUG,
     X          'INTFB: RG to RG intermediate unpacked',JPQUIET)
               IERR = KRG2RGZ(INGRIB,RWORK,NIGAUSS,NOMEGA)
               IF(IERR.LT.0) THEN
                 CALL INTLOG(JP_ERROR,
     X             'INTFB: KRG2RGZ processing failed',JPQUIET)
                 INTFB = JPROUTINE + 10
                 GOTO 900
               ELSE
                 LENMED = IERR
               ENDIF
            ENDIF

               DO LOOP = 1, LENMED
                 ZNFELDI(LOOP) = RWORK(LOOP)
               ENDDO
            IERR = FIXAREA()
            IF( IERR.NE.0 ) THEN
              CALL INTLOG(JP_ERROR,
     X          'INTFB: Fixup area definition failed. DI',JPQUIET)
              INTFB = JPROUTINE + 9
              GOTO 900
            ENDIF
            CALL INTLOG(JP_DEBUG,
     X        'INTFB: quasi input double interolation ',NIGAUSS)
            HTYPE = GGHTYPE(NIREPR,NIGAUSS,MILLEN)
            CALL JGETGG(NIGAUSS,HTYPE,RIGAUSS,MILLEN,IERR)
            IF ( IERR .NE. 0 ) THEN
              CALL INTLOG(JP_ERROR,
     X          'INTFB: JGETGG fail - double interpolation ',JPQUIET)
               INTFB = JPROUTINE + 10
              GOTO 900
            ENDIF

            IERR = IRSIZE( NIGAUSS, NIAREA, MILLEN, NIWE, NINS, KILN,
     X                 KITOTAL, RIGAUSS, OIWEGLOBE, OINPOLE, OISPOLE,
     X                 KPR, KERR)
         ENDIF
       ENDIF
C
C         If reduced gaussian to regular gaussian of same resolution,
C         turn off the land-sea mask processing temporarily
C
          LSAMERS = (NIGAUSS.EQ.NOGAUSS)
          LOLDLSM = LSM
          LOLDLSP = LSMPAR
          IF( (LIQUASI.AND.(NOREPR.EQ.JPGAUSSIAN).AND.LSAMERS) ) THEN
            CALL INTLOG(JP_DEBUG,
     X      'INTFB: turn off any LSM processing temporarily ', JPQUIET)
            LSM    = .FALSE.
            LSMPAR = .FALSE.
          ENDIF
          IERR = IARCNTL( ZNFELDI, JPEXPAND, ZNFLDO, ISIZE, KILN,
     X                    OIWEGLOBE, OINPOLE, OISPOLE, KOLN, OOWEGLOBE,
     X                    OONPOLE, OOSPOLE, KPR ,KERR)
          LSM    = LOLDLSM
          LSMPAR = LOLDLSP
          IF( IERR.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTFB: Interpolation from quasi GG field failed',JPQUIET)
            INTFB = JPROUTINE + 7
            GOTO 900
          ENDIF
C
          IF( LSTYLE ) THEN
            NOGRID(1) = HOLDEW
            NOGRID(2) = HOLDNS
          ENDIF
        ENDIF
C
        GOTO 700
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 6.   Interpolate regular grid point to grid point.
C     -----------------------------------------------------------------|
C
  600 CONTINUE
C
      IERR = IAGCNTL( ZNFELDI, JPEXPAND, ZNFLDO, ISIZE, KILN,
     X                OIWEGLOBE, OINPOLE, OISPOLE, KOLN, OOWEGLOBE,
     X                OONPOLE, OOSPOLE, KPR, KERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTFB: Interpolation from regular field failed.',JPQUIET)
        INTFB = JPROUTINE + 8
        GOTO 900
      ENDIF
C
      IF( LSTYLE ) THEN
        NOGRID(1) = HOLDEW
        NOGRID(2) = HOLDNS
      ENDIF


C     -----------------------------------------------------------------|
C*    Section 7.   Pack field into GRIB if necessary.
C     -----------------------------------------------------------------|
C
  700 CONTINUE
C
C     If dissemination 'style' selection has been chosen, select points.
C
      IF( LSTYLE ) THEN
        CALL DDSTYLE(ZNFLDO,IDISTEP,IDISTEP,NOGRID(1),NOGRID(2),IERR)
        IF( IERR.NE.0 ) THEN
         CALL INTLOG(JP_ERROR,
     X   'INTFB: Wrong requested grid increment for dissemination style'
     X    ,JPQUIET)
          INTFB = JPROUTINE + 3
          GOTO 900
        ENDIF
        NOWE = N_NOWE
        NONS = N_NONS
cs to check if it ruin gribex packing
        ISIZE = NOWE*NONS
      ENDIF
C
C     If a 'bitmap' has been specified, build the bitmap
C

        IF( LFRAME.OR.LBITMP.OR.LIMISSV ) THEN
          ISEC1(5) = 192
          ISEC3(2) = NINT(RMISSGV)
          ZSEC3(2) = RMISSGV
        ENDIF

      IF( LBITMP ) THEN
        IERR = MKBITMP(NOWE,NONS,ZNFLDO,RMISSGV)
        IF( IERR.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFB: Problem applying bitmap',JPQUIET)
          INTFB = IERR
          GOTO 900
        ENDIF
      ENDIF
C
C     If a 'frame' has been specified, build the frame
C
      IF( LFRAME ) CALL MKFRAME(NOWE,NONS,ZNFLDO,RMISSGV,NOFRAME)

C
C     If the output is a GRIB product
C
      IF( NOFORM.EQ.1 ) THEN
        CALL INTLOG(JP_DEBUG,'INTFB: pack field for output',JPQUIET)
C
C       Setup GRIB sections for the output product
C
        IERR = IGSETUP(ISEC1,ISEC2,ISEC3,ISEC4)
        IF( IERR.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFB: Setup GRIB section fail',JPQUIET)
          INTFB = IERR
          GOTO 900
        ENDIF
C
C
C       If there are missing values, set all values to zero
C
        IF( NOMISS.NE.0 ) THEN
          DO LOOP = 1, ISEC2(2)*ISEC2(3)
            ZNFLDO(LOOP) = 0.0
          ENDDO
        ENDIF
C
C       Make adjustments for 'dissemination style'
C
        IF( LNOSTYLE.AND.(NOSTYLE.EQ.JPSDISM) ) THEN
C
C         Turn off use of ECMWF local definition
C
Cjdc      ISEC1(24) = 0
C
C         For regular lat/long or gaussian grids, calculate the grid
C         i-direction increment using truncation
C
          IF( (ISEC2(1).NE.50).AND.(ISEC2(1).NE.60).AND.
     X        (ISEC2(1).NE.70).AND.(ISEC2(1).NE.80) ) THEN
C
            IF( (ISEC2(1).NE.5).AND.(ISEC2(17).EQ.0) ) THEN
              IF( NOSTYLE.NE.JPSDISM)
     X          ISEC2(9) = (ISEC2(8) - ISEC2(5)) / (ISEC2(2) - 1)
            ENDIF
C
          ENDIF
C
        ENDIF
C
C       Code data into GRIB
C
        ILENF = ISIZE
        IERR = 1
C
C       If grid-point output, setup for 2nd order packing if requested.
C
        IF( (NOREPR.NE.JPSPHERE) .AND. (NOREPR.NE.JPSPHROT) ) THEN
          HFUNC = 'C'
          IF( NOHFUNC.EQ.'K' ) THEN
            HFUNC = 'K'
            ISEC4(4)  = 64
            ISEC4(6)  = 16
            ISEC4(9)  = 32
            ISEC4(10) = 16
            ISEC4(12) = 8
            ISEC4(13) = 4
            ISEC4(14) = 0
            ISEC4(15) = -1
C
C
            IF( NOREPR.EQ.JPQUASI ) THEN
C
C             Complex packing does not support sub-areas of reduced
C             gaussian grids. If output is reduced gaussian, check if
C             it is global to within a tolerance of 0.1 degrees.
C
              INORTH = NINT( ROGAUSS(1) * PPMULT )
              ISOUTH = -INORTH
c             EMOS-199: adjusted for reduced_gg/octahedral
c             IEAST  = JP360 - (JP360/NOLPTS(NOGAUSS))
              IEAST  = JP360 - (JP90/NOGAUSS)
              LGLOBAL = (NOAREA(2).EQ.0)
     X          .AND. (IABS(NOAREA(1)-INORTH).LE.1000)
     X          .AND. (IABS(NOAREA(3)-ISOUTH).LE.1000)
     X          .AND. (IABS(NOAREA(4)-IEAST ).LE.1000)
C
C             Use simple packing if not global.
C
              IF( .NOT. LGLOBAL ) THEN
                HFUNC = 'C'
                ISEC4(4)  = 0
                ISEC4(6)  = 0
              ENDIF
            ENDIF
C
          ELSE IF( NOHFUNC.EQ.'S' ) THEN
            ISEC4(4)  = 0
            ISEC4(6)  = 0
          ENDIF
C
        ELSE
          HFUNC = 'C'
          IF( NOHFUNC.EQ.'C' ) THEN
            ISEC2(6) = 2
            ISEC4(4) = 64
          ELSE IF( NOHFUNC.EQ.'S' ) THEN
            ISEC2(6) = 1
            ISEC4(4) = 0
          ENDIF
        ENDIF
C
        IF( LIMISSV ) THEN
          ISEC1(5) = 192
          ISEC3(2) = NINT(RMISSGV)
          ZSEC3(2) = RMISSGV
        ENDIF
C
C       For interpolated regular lat/long field, flip latitudes if
C       they run from south to north
C       (handles special case , eg KWBC SST fields)
C
        IF( LNOGRID.AND.
     X      (NOREPR.EQ.JPREGULAR).AND.
     X      (NOAREA(1).LT.NOAREA(3)) ) THEN
C
          CALL INTLOG(JP_DEBUG,
     X      'INTFB: Flip latitudes to run North to South', JPQUIET)
          DO LOOP = 1, ISEC2(3)/2
            NEXT  = 1 + (LOOP - 1)*ISEC2(2)
            ISWAP = (ISEC2(3) - LOOP)*ISEC2(2) + 1
            DO LOOPI = 1, ISEC2(2)
              RTEMP = ZNFLDO(ISWAP)
              ZNFLDO(ISWAP) = ZNFLDO(NEXT)
              ZNFLDO(NEXT) = RTEMP
              NEXT  = NEXT + 1
              ISWAP = ISWAP + 1
            ENDDO
          ENDDO
        ENDIF
C
        CALL GRIBEX( ISEC0, ISEC1, ISEC2, ZSEC2, ISEC3, ZSEC3, ISEC4,
     X               ZNFLDO,ILENF,OUTGRIB,OUTLEN,IWORD,HFUNC,IERR)
        IF( IERR.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFB: GRIBEX encoding failed.',IERR)
          INTFB = JPROUTINE + 5
          GOTO 900
        ENDIF
        OUTLEN = IWORD
C
C       Switch off use of missing value
C
        LIMISSV = .FALSE.
C
      ELSE
C
C       Otherwise, move unpacked values to user array
C
        DO LOOP = 1, ISIZE
          FLDOUT( LOOP ) = ZNFLDO( LOOP )
        ENDDO
C
C       and return the number of values, the unpacked array length
C
        OUTLEN = ISIZE
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9.   Closedown.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
C     Clear change flags for next product processing
C
      LCHANGE = .FALSE.
      LSMCHNG = .FALSE.
C
      RETURN
      END
