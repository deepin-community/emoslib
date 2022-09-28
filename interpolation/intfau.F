C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INTFAU(FLDIN,INLEN)
C
C---->
C**** INTFAU
C
C     Purpose
C     -------
C
C     Prepare to interpolate input field...
C
C
C     Interface
C     ---------
C
C     IRET = INTFAU( FLDIN,INLEN)
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
C    Field unpacked values are in ZNFELDI.
C
C
C     Method
C     ------
C
C     Unpack field if GRIB.
C
C
C     Externals
C     ---------
C
C     None.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     Jan 1995
C
C----<
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
#include "grfixed.h"
#include "intf.h"
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 26300 )
      INTEGER JPLEN, JPNM
      PARAMETER ( JPNM = JPSTRUNC )
      PARAMETER ( JPLEN = (JPNM+1)*(JPNM+2) )
C
C     Local variables
C
      REAL POLELAT, POLELON
      CHARACTER*1 HTYPE
      INTEGER NUMPTS, NGAUSS
      INTEGER ISIZE, NBYTES, ITRUNC
      DATA ISIZE/0/
C
      REAL SWORK
      DIMENSION SWORK(1)
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 ISWORK
#endif
#endif
      POINTER ( ISWORK, SWORK)
C
      REAL*8 DWORK
      DIMENSION DWORK(1)
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IDWORK
#endif
#endif
      POINTER ( IDWORK, DWORK )
C
      REAL*8 DATA
      DIMENSION DATA(1)
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IDATA
#endif
#endif
      POINTER ( IDATA, DATA)
C
      SAVE ISIZE, ISWORK, IDWORK, IDATA
C
      REAL*8 DLON, DLAT
      LOGICAL LOK
      DATA NBYTES/4/
C
C     Externals
      INTEGER GGROTAT
      LOGICAL JACOBI
      EXTERNAL JACOBI
      INTEGER IRET
      INTEGER LOOP
C
C ------------------------------------------------------------------
C*    Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
      INTFAU = 0
      IRET = 0
C
C ------------------------------------------------------------------
C*    Section 2.   Decode data from the GRIB code
C ------------------------------------------------------------------
C
  200 CONTINUE
C
C     Allocate work array ZNFELDI if not already done.
C
      IF( IZNJDCI.NE.1952999238 ) THEN
        CALL JMEMHAN( 19, IZNFLDI, JPEXPAND, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_WARN,'INTFAU: ZNFELDI allocate fail',JPQUIET)
          INTFAU = IRET
          GOTO 900
        ENDIF
        IZNJDCI = 1952999238
      ENDIF
C
C     Move unpacked values in from user array
      DO 210 LOOP = 1, INLEN
        ZNFELDI( LOOP ) = FLDIN( LOOP )
 210  CONTINUE
C
C ------------------------------------------------------------------
C*    Section 3.   Handle spectral rotation, if necessary.
C ------------------------------------------------------------------
C
  300 CONTINUE
C
C     Is the interpolation from SH with a rotation of the SH first?
C
      IF( ((NIREPR.EQ.JPSPHERE).OR.(NIREPR.EQ.JPSPHROT)) .AND.
     X    LNOROTA ) THEN
C
C       Avoid rotating U or V spectral coefficients
C
        IF( (NIPARAM.EQ.131).OR.(NIPARAM.EQ.132) ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTFAU: Unable to rotate spectral U or V:',NIPARAM)
          INTFAU = JPROUTINE + 6
          GOTO 900
        ENDIF
C
C       Dynamically allocate memory for workspace.
C
C       (Note the multiplication by 2 for REAL*8).
C
        NUMPTS = INLEN
        ISIZE = NUMPTS*2
        CALL JMEMHAN( 11, IDWORK, ISIZE, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFAU: memory allocation fail',IDWORK)
          INTFAU = JPROUTINE + 1
          GOTO 900
        ENDIF
C
C       (Note the multiplication by 2 for REAL*8).
C
        ISIZE = JPLEN*2
        CALL JMEMHAN( 15, IDATA, ISIZE, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFAU: memory allocation fail',IDATA)
          INTFAU = JPROUTINE + 2
          GOTO 900
        ENDIF
C
        CALL INTLOG(JP_DEBUG,'INTFAU: Rotate SH field.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'INTFAU: South pole latitude  ',NOROTA(1))
        CALL INTLOG(JP_DEBUG,'INTFAU: South pole longitude ',NOROTA(2))
        IF ( NOREPR.EQ.JPSPHERE )  NOREPR = JPSPHROT
        IF ( NOREPR.EQ.JPREGULAR )  NOREPR = JPREGROT
        ITRUNC = NIRESO
#if (defined REAL_8)
        DLAT = -90.0 - REAL(NOROTA(1))/JPMULT
        DLON = - REAL(NOROTA(2))/JPMULT
C
C       Rotate the spectral field by longitude.
        CALL RPHI( ZNFELDI, ITRUNC, DWORK, DLON)
C
C       Rotate the spectral field by latitude.
        LOK = JACOBI( ZNFELDI, ITRUNC, DWORK, DLAT)
#else
        DLAT = -90.0 - DBLE(NOROTA(1))/JPMULT
        DLON = - DBLE(NOROTA(2))/JPMULT
C
C       Expand spectral coefficients to REAL*8
C
        DO LOOP = 1, NUMPTS
          DATA(LOOP) = DBLE(ZNFELDI(LOOP))
        ENDDO
C
C       Rotate the spectral field by longitude.
        CALL RPHI( DATA, ITRUNC, DWORK, DLON)
C
C       Rotate the spectral field by latitude.
C
        LOK = JACOBI( DATA, ITRUNC, DWORK, DLAT)
#endif
        IF(.NOT.LOK) THEN
          CALL INTLOG(JP_ERROR,'INTFAU: Spectral rotation fail',JPQUIET)
          INTFAU = JPROUTINE + 3
          GOTO 900
        ENDIF
#if (!defined REAL_8)
C
C       Repack spectral coefficients to REAL*4.
C
        DO LOOP = 1, NUMPTS
          ZNFELDI(LOOP) = SNGL(DATA(LOOP))
        ENDDO
#endif
C
      ENDIF
C
C ------------------------------------------------------------------
C*    Section 4.   Handle gaussian rotation, if necessary.
C ------------------------------------------------------------------
C
  400 CONTINUE
C
C     Is the interpolation from gaussian with a rotation of the
C     gaussian first?
C
      IF( ((NIREPR.EQ.JPGAUSSIAN).OR.(NIREPR.EQ.JPQUASI)) .AND.
     X    LNOROTA ) THEN
C
        CALL INTLOG(JP_DEBUG,'INTFAU: Rotate gaussian field.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'INTFAU: South pole latitude  ',NOROTA(1))
        CALL INTLOG(JP_DEBUG,'INTFAU: South pole longitude ',NOROTA(2))
        IF ( NOREPR.EQ.JPGAUSSIAN )  NOREPR = JPFGGROT
        IF ( NOREPR.EQ.JPQUASI )     NOREPR = JPQGGROT
C
C       Dynamically allocate memory for workspace.
C
        NUMPTS = INLEN
        ISIZE  = NUMPTS
        CALL JMEMHAN( 11, ISWORK, ISIZE, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFAU: memory allocation fail',ISWORK)
          INTFAU = JPROUTINE + 4
          GOTO 900
        ENDIF
C
C       Rotate the gaussian field.
C
        POLELAT = REAL(NOROTA(1))/JPMULT
        POLELON = REAL(NOROTA(2))/JPMULT
        NGAUSS = NIGAUSS
        IF( NIREPR.EQ.JPGAUSSIAN ) THEN
          HTYPE = 'F'
        ELSE
          HTYPE = 'R'
        ENDIF
        IRET=GGROTAT(ZNFELDI,SWORK,NUMPTS,POLELAT,POLELON,HTYPE,NGAUSS)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFAU: Gaussian rotation fail',JPQUIET)
          INTFAU = JPROUTINE + 5
          GOTO 900
        ENDIF
C
C       Move rotated field back into field original array.
C
        DO LOOP = 1, NUMPTS
          ZNFELDI(LOOP) = SWORK(LOOP)
        ENDDO
        OUTLROT = NUMPTS
C
      ENDIF
C
C
C ------------------------------------------------------------------
C*    Section 9.   Closedown.
C ------------------------------------------------------------------
C
  900 CONTINUE
C
C     Clear change flags for next product processing
C
      RETURN
      END
