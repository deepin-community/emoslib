C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

       INTEGER FUNCTION INTF( INGRIB,INLEN,FLDIN,OUTGRIB,OUTLEN,FLDOUT)
C
C---->
C**** INTF
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
C     IRET = INTF( INGRIB,INLEN,FLDIN,OUTGRIB,OUTLEN,FLDOUT)
C
C     Input
C     -----
C
C     INGRIB - Input field (packed).
C     INLEN  - Input field length (words).
C     FLDIN  - Input field (unpacked).
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
C     Call interpolation routines.
C
C
C     Externals
C     ---------
C
C     IBASINI - Ensure basic interpolation setup is done.
C     INSANE  - Ensure no outrageous values given for interpolation.
C     PDDEFS  - Setup interpolation using parameter dependent options.
C     PRECIP  - Says if field is to have 'precipitation' treatment
C     INTFAU  - Prepare to interpolate unpacked input field.
C     INTFAP  - Prepare to interpolate packed input field.
C     HNTFAU  - Prepare to interpolate unpacked input field (using
C               Hirlam 12-point for rotations).
C     HNTFAP  - Prepare to interpolate packed input field (using
C               Hirlam 12-point for rotations).
C     INTFB   - Interpolate input field.
C     INTLOG  - Log error message.
C     JDEBUG  - Checks environment to switch on/off debug
C     INTWAVE - Interpolate quasi-regular lat/long wave field
C               to a regular lat/long field.
C     OCEANP  - Interpolate GRIB ocean field.
C     RESET_C - Reset interpolation handling options using GRIB product.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     Aug 1994
C
C----<
C
      IMPLICIT NONE
C
C     Function arguments
C
      INTEGER INGRIB(*),OUTGRIB(*),INLEN,OUTLEN
      REAL FLDIN(*),FLDOUT(*)
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
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 26000 )
C
C     Local variables
C
      INTEGER IWORD, IRET, IORIGLN, ISAME
      INTEGER LOOP, IIHOLD, IOHOLD
      DIMENSION IIHOLD(4), IOHOLD(4)
      INTEGER NUMTABL, NUMPROD
      LOGICAL L98WAVE, LUNROT
      CHARACTER*1 HTYPE
C
C     Externals
C
      INTEGER IBASINI, PDDEFS, INSANE
      INTEGER INTFAU, INTFAP, INTFB, HNTFAU, HNTFAP
      INTEGER INTWAVE2, INTWAVU, OCEANP, OCEANU, RESET_C !, INTWAVE
      LOGICAL PRECIP
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      INTF = 0
      IRET = 0
      IORIGLN = OUTLEN
      NOMISS = 0
      LUNROT = .FALSE.
      OUTLROT = 0

C
C     Save input and output area definitions
C
      DO 110 LOOP = 1, 4
        IIHOLD(LOOP) = NIAREA(LOOP)
        IOHOLD(LOOP) = NOAREA(LOOP)
  110 CONTINUE
C
C     Check if debug option turned on
C
      CALL JDEBUG()
C
C     If the input is a set of U and V rotated lat/long fields,
C     set return length to zero and do not copy input to output
C
      IF( (NIREPR.EQ.JPREGULAR).AND.LWIND.AND.LNOROTA ) THEN
        OUTLEN = 0
        IRET   = 0
        CALL INTLOG(JP_DEBUG,
     X    'INTF: Input U and V rotated lat/long fields ...',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'INTF: ... no further interpolation has been done',JPQUIET)
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2.   Prepare to interpolate input field.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Ensure that basic initialisation has been done
C
      IRET = IBASINI(0)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTF: basic initialisation fail.',JPQUIET)
        INTF = IRET
        GOTO 900
      ENDIF
C
C     Allocate work array ZNFELDI if not already done.
C
       LACCUR = .FALSE.
      IF( NIFORM.EQ.1 ) THEN
        IF( IZNJDCI.NE.1952999238 ) THEN
          CALL JMEMHAN( 19, IZNFLDI, JPEXPAND, 1, IRET)
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_WARN,'INTF: ZNFELDI allocation fail',JPQUIET)
            INTF = IRET
            GOTO 900
          ENDIF
          IZNJDCI = 1952999238
        ENDIF
C
C       Unpack the field headers for packed input.
C
        CALL INTLOG(JP_DEBUG, 'INTF: Unpack GRIB headers.',JPQUIET)
        IRET = 1
        IWORD = 0
        CALL GRIBEX(ISEC0, ISEC1, ISEC2, ZSEC2, ISEC3, ZSEC3, ISEC4,
     X              ZNFELDI, JPEXPAND, INGRIB, INLEN, IWORD, 'J',IRET)
C
        IF( (IRET.NE.0).AND.(IRET.NE.811) ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTF: Failed to unpack GRIB heders.',JPQUIET)
          INTF = IRET
          GOTO 900
        ENDIF
C
C       Reset interpolation handling options using GRIB values.
C
        IRET = RESET_C( ISEC1, ISEC2, ZSEC2, ISEC4)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_WARN,
     X      'INTF: Setup of interp. options from GRIB failed',JPQUIET)
          INTF = IRET
          GOTO 900
        ENDIF
C end of NIFORM = 1
       LACCUR = .TRUE.
      ENDIF
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
cs   Regular Gaussian has to be set here for unpacked fields
      IF( NIFORM.NE.1 ) THEN
cs    this is for merging with grib_api
        LUVCOMP = .FALSE.
        IF( NOREPR.EQ.JPNOTYPE ) THEN
         IF( (NOGAUSO.NE.NOGAUSS).OR.(HOGAUST.NE.'F') ) THEN
            HTYPE = 'F'
            CALL JGETGG(NOGAUSS,HTYPE,ROGAUSS,NOLPTS,IRET)
            IF( IRET.NE.0 ) THEN
              CALL INTLOG(JP_ERROR,
     X          'INTF: JGETGG failed, NOGAUSS = ',NOGAUSS)
              INTF = IRET
              GOTO 900
            ENDIF
            NOGAUSO = NOGAUSS
            HOGAUST = HTYPE
          ENDIF
          NOREPR = JPGAUSSIAN
        ENDIF
      ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C     Check that no outrageous values given for interpolation
C
      ISAME = INSANE()
      IF ( ISAME .GT. 0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTF: Interpolation cannot use given values.',JPQUIET)
        INTF = ISAME
        GOTO 900
      ENDIF
Cs setting of out Date
      NODATE = NIDATE
C
C     If output is same as the input, set return length to zero and
C     do not copy input to output
C
      IF( ISAME.EQ.-1 ) THEN
        OUTLEN = 0
        IRET   = 0
        CALL INTLOG(JP_DEBUG,
     X    'INTF: Output is same as the input.',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'INTF: No interpolation carried out.',JPQUIET)
        GOTO 900
      ENDIF
C
C     Set precipitation flag if user hasn't
C
      IF( .NOT.LPRECSET ) LPREC = PRECIP()
C
C     Handle packed fields
C
C
C     -----------------------------------------------------------------|
C*    Section 3.   Use special interpolation for:
C                  - an ECMWF wave field.
C                  - a reduced latitude-longitude field
C     -----------------------------------------------------------------|
C
  300   CONTINUE
C
        NUMTABL = NITABLE
        NUMPROD = NUMTABL*1000 + NIPARAM
        L98WAVE = (NUMTABL.EQ.140).OR.
     X            (NUMPROD.EQ.131229).OR.
     X            (NUMPROD.EQ.131232).OR.
     X            (NIREPR.EQ.26)
      IF( L98WAVE ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'INTF: Wave-type interpolation required.',JPQUIET)
C
        OUTLEN = IORIGLN
        IF( NIFORM.EQ.1 ) THEN
             IRET = INTWAVE2(INGRIB,INLEN,OUTGRIB,OUTLEN)
cs             IRET = INTWAVE(INGRIB,INLEN,OUTGRIB,OUTLEN)
        ELSE
            IRET = INTWAVU(FLDIN,INLEN,FLDOUT,OUTLEN)
        ENDIF
          IF( IRET.EQ.0 ) THEN
            CALL INTLOG(JP_DEBUG,
     X        'INTF: Wave-type interpolated OK.',JPQUIET)
          ELSE
            CALL INTLOG(JP_DEBUG,
     X        'INTF: Wave-type interpolation failed.',JPQUIET)
          ENDIF
          INTF = IRET
          GOTO 900
        ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 4.   Use special interpolation for an ECMWF ocean field.
C     -----------------------------------------------------------------|
C
  400   CONTINUE
C
        IF( ((ISEC1(24).EQ.1).AND.(ISEC1(37).EQ.4)).OR.LOCEAN ) THEN
C
          CALL INTLOG(JP_DEBUG,
     X      'INTF: Ocean field interpolation required.',JPQUIET)
C
          OUTLEN = IORIGLN
          IF( NIFORM.EQ.1 ) THEN
            IRET = OCEANP(INGRIB,INLEN,OUTGRIB,OUTLEN)
          ELSE
            IRET = OCEANU(FLDIN,INLEN,FLDOUT,OUTLEN)
          ENDIF
          IF( IRET.EQ.0 ) THEN
            CALL INTLOG(JP_DEBUG,
     X        'INTF: Ocean field interpolated OK.',JPQUIET)
          ELSE
            CALL INTLOG(JP_DEBUG,
     X        'INTF: Ocean field interpolation failed.',JPQUIET)
          ENDIF
          INTF = IRET
          GOTO 900
        ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 7.   Continue interpolation setup in other cases.
C     -----------------------------------------------------------------|
C
  500   CONTINUE
      IF( NIFORM.EQ.1 ) THEN
C
C       Unpack (and rotate) field if necessary
C
        IF( LUSEHIR ) THEN
          IRET = HNTFAP(INGRIB,INLEN)
        ELSE
          IRET = INTFAP(INGRIB,INLEN)
        ENDIF
C
C       If a bitmap encountered with some missing values (IRET=-4),
C       product cannot be interpolated unless 'missingvalue'
C       specified via INTIN
C
        IF( .NOT.LIMISSV ) THEN
          IF( (IRET.NE.0).AND.(IRET.NE.-2) ) THEN
            IF( IRET.EQ.-4 ) THEN
              CALL INTLOG(JP_WARN,
     X        'INTF: Product has bitmap and missing data.',JPQUIET)
              CALL INTLOG(JP_WARN,
     X        'INTF: Try Using INTIN "missingvalue" option',JPQUIET)
            ENDIF
            INTF = -4
            GOTO 900
          ENDIF
        ELSE
          IF( IRET.GT.0 ) THEN
            CALL INTLOG(JP_WARN,
     X        'INTF: Problems preparing for interpolation.',JPQUIET)
            INTF = IRET
            GOTO 900
          ENDIF
        ENDIF
C
C     Handle unpacked fields
C
      ELSE
        LUNROT = .TRUE.
        IF( LUSEHIR ) THEN
          IRET = HNTFAU(FLDIN,INLEN)
        ELSE
          IRET = INTFAU(FLDIN,INLEN)
        ENDIF
      ENDIF
C
      IF( (IRET.NE.0).AND.(IRET.NE.-2) ) THEN
        CALL INTLOG(JP_WARN,'INTF: Prepare interpolate fail',JPQUIET)
        INTF = IRET
        GOTO 900
      ENDIF
C
C     Field values are now in ZNFELDI.
C
C     Setup output length same as input GRIB length in case straight
C     copy is done later (ie input is transferred direct to output
C     without postprocessing).
C
      OUTLEN = INLEN

C
C     Setup interpolation options based on parameter in field.
C
      IRET = PDDEFS()
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTF: Setup interpolation options from param failed',JPQUIET)
        INTF = IRET
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 8.   Interpolate input field.
C     -----------------------------------------------------------------|
C
  700 CONTINUE
C
C     If all values missing, set flag to ensure all are missing in the
C     interpolated field.
C
      IF( ISEC4(1).LT.0 ) NOMISS = ISEC4(1)
C
C     Perform the interpolation.
C
      CALL INTLOG(JP_DEBUG,'INTF: Perform the interpolation.',JPQUIET)
C
      OUTLEN = IORIGLN
      IF( NIFORM.EQ.1 ) THEN
        IRET = INTFB( INGRIB,INLEN,OUTGRIB,OUTLEN,FLDOUT)
      ELSE
        IRET = INTFB( ZNFELDI,INLEN,OUTGRIB,OUTLEN,FLDOUT)
      ENDIF
C
      IF( LUNROT.AND.LNOROTA ) THEN
          OUTLEN = OUTLROT
      ENDIF
C
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTF: Interpolation failed.',JPQUIET)
        INTF = IRET
        GOTO 900
      ELSE
C
        CALL INTLOG(JP_DEBUG,
     X    'INTF: Interpolation finished successfully.',JPQUIET)
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
C     Restore input and output area definitions
C
      DO 910 LOOP = 1, 4
        NIAREA(LOOP) = IIHOLD(LOOP)
        NOAAPI(LOOP) = NOAREA(LOOP)
        NOAREA(LOOP) = IOHOLD(LOOP)
  910 CONTINUE
C
      RETURN
      END
