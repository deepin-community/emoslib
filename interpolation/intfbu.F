C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INTFBU(FLDIN,INLEN,FLDOUT,OUTLEN)
C
C---->
C**** INTFBU
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
C     IRET = INTFBU( FLDIN,INLEN,FLDOUT,OUTLEN)
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
C     FLDOUT  - Output field (unpacked).
C     OUTLEN  - Output field length (words).
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
C     JSH2SH  - Changes resolution of a field of spectral coefficients.
C     JALLGP  - Converts spectral input fields to lat/long grid fields.
C     JALLWN  - Converts spectral input wind fields to lat/long grids.
C     JAGGGP  - Converts spectral input fields to a gaussian grid field.
C     IBASINI - Ensure basic interpolation setup is done.
C     RESET_C - Reset interpolation handling options using GRIB product.
C     JMEMHAN - Handles memory allocation.
C     IGLSIZE - Evaluate array sizes for reg. lat/long and area.
C     IGSIZE  - Evaluate array sizes for a regular Gaussian.
C     IRSIZE  - Evaluate array sizes for a quasi reg Gaussian.
C     ISCRSZ  - Calculate number of values in generated field.
C     INTLOG  - Log error message.
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
      IMPLICIT NONE
C
C     Function arguments
      INTEGER INLEN,OUTLEN
      REAL FLDIN(*),FLDOUT(*)
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "grfixed.h"
#include "intf.h"
C
C     Parameters
      INTEGER JPROUTINE
      INTEGER JPALLOC, JPSCR16
      PARAMETER (JPROUTINE = 26600 )
      PARAMETER (JPALLOC = 1 )
      PARAMETER (JPSCR16 = 16 )
C
C     Local variables
C
      INTEGER IERR, ISIZE, IUV
      REAL PNORTH, PSOUTH, PWEST, PEAST, PLATINC, PLONINC
      LOGICAL OIWEGLOBE, OINPOLE, OISPOLE
      LOGICAL OOWEGLOBE, OONPOLE, OOSPOLE
      LOGICAL LSTYLE
      INTEGER KILN, KITOTAL, KOLN, KOTOTAL, KPR, KERR
      INTEGER LOOP
      INTEGER HOLDEW, HOLDNS, IDISTEP
      REAL*8 DISSRES
      CHARACTER*1 HTYPE
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IZNFLDO
#endif
#endif
      REAL ZNFLDO
      POINTER ( IZNFLDO, ZNFLDO )
      DIMENSION ZNFLDO( 1 )
C
C     Externals
      INTEGER IGLSIZE, IGSIZE, IRSIZE, IARCNTL, IAGCNTL
      INTEGER ISCRSZ
C
C ------------------------------------------------------------------
C*    Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
      HOLDEW = 0
      HOLDNS = 0
      IERR   = 0
      KPR    = 0
      LSTYLE = .FALSE.
      INTFBU = 0
C
C     Allocate work array ZNFELDI if not already done.
C
      IF( IZNJDCI.NE.1952999238 ) THEN
        CALL JMEMHAN( 19, IZNFLDI, JPEXPAND, 1, IERR)
        IF( IERR.NE.0 ) THEN
          CALL INTLOG(JP_WARN,'INTFBU: znfeldi allocate fail',JPQUIET)
          INTFBU = IERR
          GOTO 900
        ENDIF
        IZNJDCI = 1952999238
      ENDIF
C
C ------------------------------------------------------------------
C*    Section 3.   Interpolation spherical harmonics to spherical
C                  harmonics
C ------------------------------------------------------------------
C
  300 CONTINUE
C
      IF ( ((NIREPR.EQ.JPSPHERE).OR.(NIREPR.EQ.JPSPHROT)) .AND.
     X     ((NOREPR.EQ.JPSPHERE).OR.(NOREPR.EQ.JPSPHROT)) ) THEN

C       Get scratch space for interpolation
C
        ISIZE = ISCRSZ()
        IF ( ISIZE .LE. 0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFBU: Get scratch space failed',ISIZE)
          INTFBU = JPROUTINE + 2
          GOTO 900
        ENDIF
        CALL JMEMHAN( JPSCR16, IZNFLDO, ISIZE, JPALLOC, IERR)
        IF ( IERR .NE. 0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFBU: Get scratch space fail',JPQUIET)
          INTFBU = JPROUTINE + 2
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
       LSTYLE = LNOSTYLE.AND.
     X         (NOSTYLE.EQ.JPSDISM).AND.
     X         (NOREPR.EQ.JPREGULAR)

        IF( LSTYLE ) THEN
c to be consistant with product generation
              IF(NIRESO.EQ.1279) THEN
                 DISSRES = 0.125
              ELSEIF(NIRESO.EQ.799) THEN
                 DISSRES = 0.25
              ELSEIF(NIRESO.EQ.639) THEN
                 DISSRES = 0.25
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
            CALL INTLOG(JP_WARN,'INTFBU: Diss resol set to: ',NOGRID(1))
              ENDIF
            IDISTEP = NINT(DISSRES*PPMULT)
            HOLDEW = NOGRID(1)
            HOLDNS = NOGRID(2)
            NOGRID(1) = IDISTEP
            NOGRID(2) = IDISTEP
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
C ------------------------------------------------------------------
C     Section 4.   Interpolation spherical harmonics to grid point.
C ------------------------------------------------------------------
C
  400 CONTINUE
C
      IF ( (NIREPR.EQ.JPSPHERE).OR.(NIREPR.EQ.JPSPHROT) ) THEN
C
C       Get scratch space for interpolation
C
        ISIZE = ISCRSZ()
        IF ( ISIZE .LE. 0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFBU: Get scratch space failed',ISIZE)
          INTFBU = JPROUTINE + 4
          GOTO 900
        ENDIF
C
        CALL JMEMHAN( JPSCR16, IZNFLDO, ISIZE, JPALLOC, IERR)
        IF ( IERR .NE. 0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFBU: Get scratch space fail',JPQUIET)
          INTFBU = JPROUTINE + 4
          GOTO 900
        ENDIF
C
C       Setup wind flag
C
        IF( .NOT.LWINDSET ) THEN
          LWIND = ( ( NOTABLE.EQ.128 ).AND.
     X            ((NOPARAM.EQ.131).OR.(NOPARAM.EQ.132)) )
        ENDIF
        CALL INTLOG(JP_DEBUG,'INTFBU: Wind flag = ', LWIND)
        IF ( LWIND ) THEN
          IUV = 1
        ELSE
          IUV = 0
        ENDIF
C
C ------------------------------------------------------------------
C*    Section 4.1  Interpolation spherical harmonics to grid point.
C ------------------------------------------------------------------
C
  410 CONTINUE
C
        IF ( (NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPREGROT) ) THEN
          CALL INTLOG(JP_DEBUG,'INTFBU: SH -> lat/long',JPQUIET)
C
C         -> latitude/longitude grid
C
          PLONINC = FLOAT( NOGRID(1) ) / PPMULT
          PLATINC = FLOAT( NOGRID(2) ) / PPMULT
          IF ( LWIND ) THEN
            CALL JALLWN( ZNFELDI, NIRESO, PNORTH, PSOUTH, PWEST, PEAST,
     X                   PLATINC, PLONINC, ZNFLDO, IERR)
          ELSE
            CALL JALLGP( ZNFELDI, NIRESO, PNORTH, PSOUTH, PWEST, PEAST,
     X                   PLATINC, PLONINC, ZNFLDO, IERR)
          ENDIF
          IF ( IERR .NE. 0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTFBU: Interpolation of gaussian field failed.',JPQUIET)
            INTFBU = JPROUTINE + 4
            GOTO 900
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
          CALL INTLOG(JP_DEBUG,'INTFBU: SH -> regular gaussian',JPQUIET)
cs          HTYPE = 'U'
            HTYPE = 'F'
          ELSE
          CALL INTLOG(JP_DEBUG,'INTFBU: SH -> reduced gaussian',JPQUIET)
            HTYPE = 'U'
          ENDIF

          CALL JAGGGP(ZNFELDI, NIRESO, PNORTH, PSOUTH, PWEST, PEAST,
     X                NOGAUSS, HTYPE, NOLPTS, ZNFLDO, IUV, IERR)
          IF ( IERR .NE. 0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTFBU: Interpolation of gaussian field failed.',JPQUIET)
            INTFBU = JPROUTINE + 4
            GOTO 900
          ENDIF
          GOTO 700

        ENDIF
C
C       Dropthrough => unrecognized output field type
C
        IERR   = JPROUTINE + 4
        CALL INTLOG(JP_ERROR,
     X    'INTFBU: Unrecognized output field type.',NOREPR)
        INTFBU = IERR
        GOTO 900
C
      ENDIF
C
C ------------------------------------------------------------------
C     Section 5.   Interpolate Gaussian grid to grid point.
C ------------------------------------------------------------------
C
  500 CONTINUE
C
C     Setup flags for input and output fields
C
      KERR = 1
      IF (NIREPR .EQ. JPQUASI) THEN
        IERR = IRSIZE( NIGAUSS, NIAREA, MILLEN, NIWE, NINS, KILN,
     X                 KITOTAL, RIGAUSS, OIWEGLOBE, OINPOLE, OISPOLE,
     X                 KPR, KERR)
      ELSE IF ( NIREPR .EQ. JPGAUSSIAN ) THEN
          IERR = IGSIZE( NIGAUSS, NIAREA, NIWE, NINS, KILN, RIGAUSS,
     X                    OIWEGLOBE, OINPOLE, OISPOLE, KPR, KERR)
      ELSE
        IERR = IGLSIZE( NIGRID, NIAREA, NIWE, NINS, KILN, OIWEGLOBE,
     X                  OINPOLE, OISPOLE, KPR, KERR)
      ENDIF
C
      NONS = 0
      NOWE = 0
      IF (NOREPR .EQ. JPQUASI) THEN
        IERR = IRSIZE( NOGAUSS, NOAREA, NOLPTS, NOWE, NONS, KOLN,
     X                 KOTOTAL, ROGAUSS, OOWEGLOBE, OONPOLE, OOSPOLE,
     X                 KPR, KERR)
      ELSE IF ( NOREPR .EQ. JPGAUSSIAN ) THEN
          IERR = IGSIZE( NOGAUSS, NOAREA, NOWE, NONS, KOLN, ROGAUSS,
     X                    OOWEGLOBE, OONPOLE, OOSPOLE, KPR, KERR)
      ELSE
        IERR = IGLSIZE( NOGRID, NOAREA, NOWE, NONS, KOLN, OOWEGLOBE,
     X                  OONPOLE, OOSPOLE, KPR, KERR)
      ENDIF
C
      IF ( IERR .NE. 0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTFBU: Interpolation of gaussian field failed.',IERR)
        INTFBU = JPROUTINE + 6
        GOTO 900
      ENDIF
C
C
C ------------------------------------------------------------------
C     Section 5.1  Interpolate reduced Gaussian grid to grid point.
C ------------------------------------------------------------------
C
  510 CONTINUE
C
C
      ISIZE = ISCRSZ()
      IF ( ISIZE .LE. 0 ) THEN
        CALL INTLOG(JP_ERROR,'INTFBU: Get scratch space failed.',ISIZE)
        INTFBU = JPROUTINE + 2
        GOTO 900
      ENDIF
      CALL JMEMHAN( JPSCR16, IZNFLDO, ISIZE, JPALLOC, IERR)
      IF ( IERR .NE. 0 ) THEN
        CALL INTLOG(JP_ERROR,'INTFBU: Get scratch space fail.',JPQUIET)
        INTFBU = JPROUTINE + 2
        GOTO 900
      ENDIF
C
      IF (NIREPR .EQ. JPQUASI) THEN
        IERR = IARCNTL( ZNFELDI, JPEXPAND, ZNFLDO, ISIZE, KILN,
     X                  OIWEGLOBE, OINPOLE, OISPOLE, KOLN, OOWEGLOBE,
     X                  OONPOLE, OOSPOLE, KPR ,KERR)
        IF ( IERR .NE. 0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTFBU: Interpolation of gaussian field failed.',IERR)
          INTFBU = JPROUTINE + 7
          GOTO 900
        ENDIF
C
        GOTO 700
C
      ENDIF
C
C ------------------------------------------------------------------
C*    Section 6.   Interpolate regular grid point to grid point.
C ------------------------------------------------------------------
C
  600 CONTINUE
C
      IERR = IAGCNTL( ZNFELDI, JPEXPAND, ZNFLDO, ISIZE, KILN,
     X                OIWEGLOBE, OINPOLE, OISPOLE, KOLN, OOWEGLOBE,
     X                OONPOLE, OOSPOLE, KPR, KERR)
      IF ( IERR .NE. 0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTFBU: Interpolation of gaussian field failed.',IERR)
        INTFBU = JPROUTINE + 8
        GOTO 900
      ENDIF
C
C ------------------------------------------------------------------
C*    Section 7.   Move unpacked values to user array.
C ------------------------------------------------------------------
C
  700 CONTINUE
C     If dissemination 'style' selection has been chosen, select points.
C
      IF( LSTYLE ) THEN
          NOGRID(1) = HOLDEW
          NOGRID(2) = HOLDNS
        CALL DDSTYLE(ZNFLDO,IDISTEP,IDISTEP,NOGRID(1),NOGRID(2),IERR)
        IF( IERR.NE.0 ) THEN
         CALL INTLOG(JP_ERROR,
     X  'INTFBU: Wrong requested grid increment for dissemination style'
     X   ,JPQUIET)
         INTFBU = JPROUTINE + 3
          GOTO 900
        ENDIF
        PLONINC = FLOAT(NOGRID(1))/PPMULT
        PLATINC = FLOAT(NOGRID(2))/PPMULT
        PNORTH  = FLOAT(NOAREA(1))/PPMULT
        PWEST   = FLOAT(NOAREA(2))/PPMULT
        PSOUTH  = FLOAT(NOAREA(3))/PPMULT
        PEAST   = FLOAT(NOAREA(4))/PPMULT

        NOWE = NINT((PEAST  - PWEST )/PLONINC) + 1
        NONS = NINT((PNORTH - PSOUTH)/PLATINC) + 1
        ISIZE = NOWE*NONS
      ENDIF
c
      DO LOOP = 1, ISIZE
        FLDOUT( LOOP ) = ZNFLDO( LOOP )
      ENDDO
C
C     Return the number of values, the unpacked array length
C
      OUTLEN = ISIZE
C
C ------------------------------------------------------------------
C*    Section 9.   Closedown.
C ------------------------------------------------------------------
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
