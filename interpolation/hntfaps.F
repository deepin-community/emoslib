C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HNTFAPS(INGRIB,INLEN)
C
C---->
C**** HNTFAPS
C
C     Purpose
C     -------
C
C     Prepare to interpolate input spectral field.
C
C
C     Interface
C     ---------
C
C     IRET = HNTFAPS(INGRIB,INLEN)
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
C     Returns: 0, if OK. Otherwise, an error occured in rotation.
C
C
C     Method
C     ------
C
C     Unpack field.
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
C     GRIBEX  - Decode/encode GRIB product.
C     GRSVCK  - Turn off GRIB checking
C     INTLOG  - Log error message.
C     INTLOGR - Log error message.
C     RPHI    - Rotate a spectral field by longitude.
C     JACOBI  - Rotates spectral coefficients by latitude.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     January 31, 2001
C
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
#include "grfixed.h"
#include "intf.h"
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 40120 )
      INTEGER JPLEN, JPNM
      PARAMETER ( JPNM = JPSTRUNC )
      PARAMETER ( JPLEN = (JPNM+1)*(JPNM+6) )
C
C     Local variables
C
      INTEGER IERR
      INTEGER IRET
      INTEGER ISIZE
      INTEGER ITRUNC
      INTEGER IWORD
      INTEGER KPR
      INTEGER NBYTES
      LOGICAL LOK
      LOGICAL LSPLIT
      REAL    POLELAT
      REAL*8  DLAT
      REAL*8  DLON
      REAL*8  LATLIM
#if (!defined REAL_8)
      INTEGER LOOP
#endif
      DATA NBYTES/4/
      DATA ISIZE/0/
      SAVE ISIZE
C
      REAL   SWORK(1)
      REAL*8 DWORK(1)
      REAL*8 DATA(1)
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 ISWORK
      INTEGER*8 IDWORK
      INTEGER*8 IDATA
#endif
#endif
      POINTER (ISWORK, SWORK)
      POINTER (IDWORK, DWORK)
      POINTER (IDATA,  DATA)
      SAVE ISWORK
      SAVE IDWORK
      SAVE IDATA
C
C     Externals
C
      LOGICAL JACOBI
      EXTERNAL JACOBI
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
      HNTFAPS = 0
      IERR = 0
      KPR = 0
C
C     -----------------------------------------------------------------|
C*    Section 2.   Decode data from the GRIB code
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Decode data from GRIB code (no checking)
C
      IWORD = INLEN
      IERR  =  0
      CALL GRSVCK(0)
      IERR = 1
      ISEC3(2) = NINT(RMISSGV)
      ZSEC3(2) = RMISSGV
      CALL GRIBEX(ISEC0, ISEC1, ISEC2, ZSEC2, ISEC3, ZSEC3, ISEC4,
     X            ZNFELDI, JPEXPAND, INGRIB, INLEN, IWORD, 'D',IERR)
C
      IF( IERR.LT.0) THEN
        IF( (IERR.EQ.-2).OR.(IERR.EQ.-4) ) THEN
          CALL INTLOG(JP_DEBUG,'HNTFAPS: Use missing value',JPQUIET)
          LIMISSV = .TRUE.
        ELSE
          CALL INTLOG(JP_ERROR,'HNTFAPS: GRIBEX decoding fail.',IERR)
          HNTFAPS = IERR
          GOTO 900
        ENDIF
      ELSE IF( IERR.GT.0 ) THEN
        CALL INTLOG(JP_ERROR,'HNTFAPS: GRIBEX decoding failed.',IERR)
        HNTFAPS = IERR
        GOTO 900
      ENDIF
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
     X      'HNTFAPS: Unable to rotate spectral U or V:',NIPARAM)
          HNTFAPS = JPROUTINE + 3
          GOTO 900
        ENDIF
C
C       Dynamically allocate memory for workspace.
C
C       (Note the multiplication by 2 for REAL*8).
C
        ISIZE =  (2*(JPNM+1)*(JPNM+6))*2
        CALL JMEMHAN( 11, IDWORK, ISIZE, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'HNTFAPS: memory allocation fail',IDWORK)
          HNTFAPS = JPROUTINE + 3
          GOTO 900
        ENDIF
C
C       (Note the multiplication by 2 for REAL*8).
C
        ISIZE =  JPLEN*2
        CALL JMEMHAN( 15, IDATA, ISIZE, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'HNTFAPS: memory allocation fail',IDATA)
          HNTFAPS = JPROUTINE + 3
          GOTO 900
        ENDIF
C
        CALL INTLOG(JP_DEBUG,'HNTFAPS: Rotate SH field.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'HNTFAPS: South pole latitude  ',NOROTA(1))
        CALL INTLOG(JP_DEBUG,'HNTFAPS: South pole longitude ',NOROTA(2))
        IF ( NOREPR.EQ.JPSPHERE )  NOREPR = JPSPHROT
        IF ( NOREPR.EQ.JPREGULAR )  NOREPR = JPREGROT
        ITRUNC = ISEC2(2)
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
          HNTFAPS = JPROUTINE + 3
          GOTO 900
        ENDIF
        CALL INTLOGR(JP_DEBUG,
     X    'HNTFAPS: South pole latitude limit = ',LATLIM)
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
     X      'HNTFAPS: Rotation has been split into three steps',JPQUIET)
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
C
        CALL RPHI( ZNFELDI, ITRUNC, DWORK, DLON)
C
C       Rotate the spectral field by latitude.
C
        IF( LSPLIT ) THEN
C
C         Two rotations forward ..
C
          CALL INTLOGR(JP_DEBUG,
     X      'HNTFAPS: Twice forward through angle ', DLAT)
          LOK = JACOBI(ZNFELDI, ITRUNC, DWORK, DLAT)
          IF(.NOT.LOK) THEN
            CALL INTLOG(JP_ERROR,
     X        'HNTFAPS: Spectral rotation failed',JPQUIET)
            HNTFAPS = JPROUTINE + 3
            GOTO 900
          ENDIF
          LOK = JACOBI(ZNFELDI, ITRUNC, DWORK, DLAT)
          IF(.NOT.LOK) THEN
            CALL INTLOG(JP_ERROR,
     X       'HNTFAPS: Spectral rotation failed',JPQUIET)
            HNTFAPS = JPROUTINE + 3
            GOTO 900
          ENDIF
C
C         .. and one backward
C
          DLAT = -90.0 - ( (LATLIM*2.0) - POLELAT)
          DLAT= -DLAT
          CALL INTLOGR(JP_DEBUG,
     X      'HNTFAPS: Once backward through angle ', DLAT)
        ENDIF
C
        LOK = JACOBI( ZNFELDI, ITRUNC, DWORK, DLAT)
          IF(.NOT.LOK) THEN
            CALL INTLOG(JP_ERROR,
     X        'HNTFAPS: Spectral rotation failed',JPQUIET)
            HNTFAPS = JPROUTINE + 3
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
        DO LOOP = 1, ISEC4(1)
          DATA(LOOP) = DBLE(ZNFELDI(LOOP))
        ENDDO
C
C       Rotate the spectral field by longitude.
        CALL RPHI( DATA, ITRUNC, DWORK, DLON)
C
C       Rotate the spectral field by latitude.
C
        IF( LSPLIT ) THEN
C
C         Two rotations forward ..
C
          CALL INTLOGR(JP_DEBUG,
     X      'HNTFAPS: Twice forward through angle ', DLAT)
          LOK = JACOBI(ZNFELDI, ITRUNC, DWORK, DLAT)
          IF(.NOT.LOK) THEN
            CALL INTLOG(JP_ERROR,
     X        'HNTFAPS: Spectral rotation failed',JPQUIET)
            HNTFAPS = JPROUTINE + 3
            GOTO 900
          ENDIF
          LOK = JACOBI(ZNFELDI, ITRUNC, DWORK, DLAT)
          IF(.NOT.LOK) THEN
            CALL INTLOG(JP_ERROR,
     X        'HNTFAPS: Spectral rotation failed',JPQUIET)
            HNTFAPS = JPROUTINE + 3
            GOTO 900
          ENDIF
C
C         .. and one backward
C
          DLAT = -90.0 - ( (LATLIM*2.0) - POLELAT)
          DLAT= -DLAT
          CALL INTLOGR(JP_DEBUG,
     X      'HNTFAPS: Once backward through angle ', DLAT)
        ENDIF
C
        LOK = JACOBI( DATA, ITRUNC, DWORK, DLAT)
#endif
C
        IF( .NOT.LOK ) THEN
          CALL INTLOG(JP_ERROR,'HNTFAPS: Rotation failed',JPQUIET)
          HNTFAPS = JPROUTINE + 3
          GOTO 900
        ENDIF
#if (!defined REAL_8)
C
C       Single precision REALs; repack spectral coefficients to REAL*4.
C
        DO LOOP = 1, ISEC4(1)
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
