C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HNTFAUS(FLDIN,INLEN)
C
C---->
C**** HNTFAUS
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
C     IRET = HNTFAUS( FLDIN,INLEN)
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
C     Field unpacked values are in ZNFELDI, rotated if necessary.
C
C     Returns: 0, if OK. Otherwise, an error occured in rotation.
C
C
C     Method
C     ------
C
C     Rotate the field values if necessary.
C
C     Some rotation coefficients cannot be created because the
C     algorithm does not converge for rotations which are smaller
C     than a limit which depends on the spectral truncation.
C
C     If the rotation is too small, it has to be split into three
C     steps (two forward and one backward).
C
C     Current know limits are:
C
C     Truncation    Max South pole lat   Minimum rotation
C     ----------    ------------------   ----------------
C         63          -89.5                  0.5
C        106          -85                    5.0
C        159          -77                   13.0
C        213          -68                   22.0
C        319          -51                   39.0
C        511          -30.5                 59.5
C
C
C     Externals
C     ---------
C
C     INTLOG  - Log error message.
C     INTLOGR - Log error message.
C     RPHI    - Rotate a spectral field by longitude.
C     JACOBI  - Rotates spectral coefficients by latitude.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     February 2001
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
      PARAMETER (JPROUTINE = 40140 )
      INTEGER JPLEN, JPNM
      PARAMETER ( JPNM = JPSTRUNC )
      PARAMETER ( JPLEN = (JPNM+1)*(JPNM+6) )
C
C     Local variables
C
      INTEGER     IRET
      INTEGER     ISIZE
      INTEGER     ITRUNC
      INTEGER     LOOP
      INTEGER     NBYTES
      INTEGER     NUMPTS
      LOGICAL     LOK
      LOGICAL     LSPLIT
      REAL        POLELAT
      REAL        SWORK(1)
      REAL*8      DATA(1)
      REAL*8      DLAT
      REAL*8      DLON
      REAL*8      DWORK(1)
      REAL*8      LATLIM
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IDATA
      INTEGER*8 IDWORK
      INTEGER*8 ISWORK
#endif
#endif
      POINTER (IDATA,  DATA)
      POINTER (IDWORK, DWORK)
      POINTER (ISWORK, SWORK)
C
      SAVE IDATA
      SAVE IDWORK
      SAVE ISIZE
      SAVE ISWORK
C
      DATA ISIZE/0/
      DATA NBYTES/4/
C
C     Externals
      LOGICAL JACOBI
      EXTERNAL JACOBI
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      HNTFAUS = 0
      IRET    = 0
C
C     -----------------------------------------------------------------|
C*    Section 2.   Store input data.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Allocate work array ZNFELDI if not already done.
C
      IF( IZNJDCI.NE.1952999238 ) THEN
        CALL JMEMHAN( 19, IZNFLDI, JPEXPAND, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_WARN,'HNTFAUS: ZNFELDI allocate fail',JPQUIET)
          HNTFAUS = IRET
          GOTO 900
        ENDIF
        IZNJDCI = 1952999238
      ENDIF
C
C     Move unpacked values in from user array
C
      DO LOOP = 1, INLEN
        ZNFELDI( LOOP ) = FLDIN( LOOP )
      ENDDO
C
C     -----------------------------------------------------------------|
C*    Section 3.   Handle rotation, if necessary.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
C     Is a rotation of the SH required?
C
      IF( LNOROTA ) THEN
C
C       Avoid rotating U or V spectral coefficients
C
        IF( (NIPARAM.EQ.JP_U).OR.(NIPARAM.EQ.JP_V) ) THEN
          CALL INTLOG(JP_ERROR,
     X      'HNTFAUS: Unable to rotate spectral U or V:',NIPARAM)
          HNTFAUS = JPROUTINE + 3
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
          CALL INTLOG(JP_ERROR,'HNTFAUS: memory allocation fail',IDWORK)
          HNTFAUS = JPROUTINE + 3
          GOTO 900
        ENDIF
C
C       (Note the multiplication by 2 for REAL*8).
C
        ISIZE = JPLEN*2
        CALL JMEMHAN( 15, IDATA, ISIZE, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'HNTFAUS: memory allocation fail',IDATA)
          HNTFAUS = JPROUTINE + 3
          GOTO 900
        ENDIF
C
        CALL INTLOG(JP_DEBUG,'HNTFAUS: Rotate SH field.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'HNTFAUS: South pole latitude  ',NOROTA(1))
        CALL INTLOG(JP_DEBUG,'HNTFAUS: South pole longitude ',NOROTA(2))
        IF ( NOREPR.EQ.JPSPHERE )  NOREPR = JPSPHROT
        IF ( NOREPR.EQ.JPREGULAR )  NOREPR = JPREGROT
        ITRUNC = NIRESO
C
C       Some rotation coefficients cannot be created because the
C       algorithm does not converge for rotations which are smaller
C       than a limit which depends on the spectral truncation.
C
        IF( ITRUNC.LE.63 ) THEN
          LATLIM = -89.5
        ELSE IF( ITRUNC.LE.106 ) THEN
          LATLIM = -85.0
        ELSE IF( ITRUNC.LE.159 ) THEN
          LATLIM = -77.0
        ELSE IF( ITRUNC.LE.213 ) THEN
          LATLIM = -68.0
        ELSE IF( ITRUNC.LE.319 ) THEN
          LATLIM = -51.0
        ELSE IF( ITRUNC.LE.511 ) THEN
          LATLIM = -29.5
        ELSE
          HNTFAUS = JPROUTINE + 3
          GOTO 900
        ENDIF
        CALL INTLOGR(JP_DEBUG,
     X    'HNTFAUS: South pole latitude limit = ',LATLIM)
C
        POLELAT = (REAL(NOROTA(1))/JPMULT)
C
C       If the rotation is too small, it has to be split into three
C       steps (two forward and one backward).
C
        LSPLIT = POLELAT.LT.LATLIM
        IF( LSPLIT ) THEN
          DLAT = -90.0 - LATLIM
          CALL INTLOG(JP_DEBUG,
     X      'HNTFAUS: Rotation has been split into three steps',JPQUIET)
        ELSE
          DLAT = -90.0 - POLELAT
        ENDIF
#if (defined REAL_8)
C
C       Double precision REALs
C
        DLON = - REAL(NOROTA(2))/JPMULT
C
C       Rotate the spectral field by longitude.
        CALL RPHI( ZNFELDI, ITRUNC, DWORK, DLON)
C
C       Rotate the spectral field by latitude.
C
        IF( LSPLIT ) THEN
C
C         Two rotations forward ..
C
          CALL INTLOGR(JP_DEBUG,
     X      'HNTFAUS: Twice forward through angle ', DLAT)
          LOK = JACOBI(ZNFELDI, ITRUNC, DWORK, DLAT)
          IF(.NOT.LOK) THEN
            CALL INTLOG(JP_ERROR,
     X        'HNTFAUS: Spectral rotation failed',JPQUIET)
            HNTFAUS = JPROUTINE + 3
            GOTO 900
          ENDIF
          LOK = JACOBI(ZNFELDI, ITRUNC, DWORK, DLAT)
          IF(.NOT.LOK) THEN
            CALL INTLOG(JP_ERROR,
     X       'HNTFAUS: Spectral rotation failed',JPQUIET)
            HNTFAUS = JPROUTINE + 3
            GOTO 900
          ENDIF
C
C         .. and one backward
C
          DLAT = -90.0 - ( (LATLIM*2.0) - POLELAT)
          DLAT= -DLAT
          CALL INTLOGR(JP_DEBUG,
     X      'HNTFAUS: Once backward through angle ', DLAT)
        ENDIF
C
        LOK = JACOBI( ZNFELDI, ITRUNC, DWORK, DLAT)
          IF(.NOT.LOK) THEN
            CALL INTLOG(JP_ERROR,
     X        'HNTFAUS: Spectral rotation failed',JPQUIET)
            HNTFAUS = JPROUTINE + 3
            GOTO 900
          ENDIF
#else
C
C       Single precision REALs; so convert to double precision.
C
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
C
        CALL RPHI( DATA, ITRUNC, DWORK, DLON)
C
C       Rotate the spectral field by latitude.
C
        IF( LSPLIT ) THEN
C
C         Two rotations forward ..
C
          CALL INTLOGR(JP_DEBUG,
     X      'HNTFAUS: Twice forward through angle ', DLAT)
          LOK = JACOBI(ZNFELDI, ITRUNC, DWORK, DLAT)
          IF(.NOT.LOK) THEN
            CALL INTLOG(JP_ERROR,
     X        'HNTFAUS: Spectral rotation failed',JPQUIET)
            HNTFAUS = JPROUTINE + 3
            GOTO 900
          ENDIF
          LOK = JACOBI(ZNFELDI, ITRUNC, DWORK, DLAT)
          IF(.NOT.LOK) THEN
            CALL INTLOG(JP_ERROR,
     X        'HNTFAUS: Spectral rotation failed',JPQUIET)
            HNTFAUS = JPROUTINE + 3
            GOTO 900
          ENDIF
C
C         .. and one backward
C
          DLAT = -90.0 - ( (LATLIM*2.0) - POLELAT)
          DLAT= -DLAT
          CALL INTLOGR(JP_DEBUG,
     X      'HNTFAUS: Once backward through angle ', DLAT)
        ENDIF
C
        LOK = JACOBI( DATA, ITRUNC, DWORK, DLAT)
#endif
C
        IF( .NOT.LOK ) THEN
          CALL INTLOG(JP_ERROR,'HNTFAUS: Rotation failed',JPQUIET)
          HNTFAUS = JPROUTINE + 3
          GOTO 900
        ENDIF
#if (!defined REAL_8)
C
C       Single precision REALs; repack spectral coefficients to REAL*4.
C
        DO LOOP = 1, NUMPTS
          ZNFELDI(LOOP) = SNGL(DATA(LOOP))
        ENDDO
#endif
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9.   Closedown.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
