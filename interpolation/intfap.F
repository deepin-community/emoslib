C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INTFAP(INGRIB,INLEN)
C
C---->
C**** INTFAP
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
C     IRET = INTFAP(INGRIB,INLEN)
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
C     J.D.Chambers     ECMWF     Jan 1995
C
C     J.D.Chambers     ECMWF        Feb 1997
C     Allow for 64-bit pointers
C
C     J.D.Chambers     ECMWF        Oct 2000
C     Split rotations if less than minimum for the truncation
C
C----<
C
C     -----------------------------------------------------------------|
C*    Section 0.   Variables
C
C
      IMPLICIT NONE
C
C     Function arguments
      INTEGER INGRIB(*),INLEN
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "grfixed.h"
#include "intf.h"
C
C     Parameters
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 26200 )
      INTEGER JPLEN, JPNM
      PARAMETER ( JPNM = JPSTRUNC )
      PARAMETER ( JPLEN = (JPNM+1)*(JPNM+6) )
C
C     Local variables
C
      REAL POLELAT, POLELON
      CHARACTER*1 HTYPE
      INTEGER NUMPTS, NGAUSS
      INTEGER IWORD, IERR, KPR, NBYTES, ITRUNC, LOOP, IRET
      INTEGER ISIZE
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
      POINTER ( IDWORK, DWORK)
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
      REAL*8 DLON, DLAT, LATLIM
      LOGICAL LOK, LSPLIT
      DATA NBYTES/4/
C
C     Externals
      INTEGER NGROTAT
      EXTERNAL NGROTAT
      LOGICAL JACOBI
      EXTERNAL JACOBI
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
      INTFAP = 0
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
          CALL INTLOG(JP_DEBUG,'INTFAP: Use missing value',JPQUIET)
          LIMISSV = .TRUE.
        ELSE
          CALL INTLOG(JP_ERROR,'INTFAP: GRIBEX decoding fail.',IERR)
          INTFAP = IERR
          GOTO 900
        ENDIF
      ELSE IF( IERR.GT.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTFAP: GRIBEX decoding failed.',IERR)
        INTFAP = IERR
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3.   Handle spectral rotation, if necessary.
C     -----------------------------------------------------------------|
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
     X      'INTFAP: Unable to rotate spectral U or V:',NIPARAM)
          INTFAP = JPROUTINE + 6
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
          CALL INTLOG(JP_ERROR,'INTFAP: memory allocation fail',IDWORK)
          INTFAP = JPROUTINE + 1
          GOTO 900
        ENDIF
C
C       (Note the multiplication by 2 for REAL*8).
C
        ISIZE =  JPLEN*2
        CALL JMEMHAN( 15, IDATA, ISIZE, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFAP: memory allocation fail',IDATA)
          INTFAP = JPROUTINE + 2
          GOTO 900
        ENDIF
C
        CALL INTLOG(JP_DEBUG,'INTFAP: Rotate SH field.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'INTFAP: South pole latitude  ',NOROTA(1))
        CALL INTLOG(JP_DEBUG,'INTFAP: South pole longitude ',NOROTA(2))
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
          INTFAP = JPROUTINE + 7
          GOTO 900
        ENDIF
        CALL INTLOGR(JP_DEBUG,
     X    'INTFAP: South pole latitude limit = ',LATLIM)
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
     X      'INTFAP: Rotation has been split into three steps',JPQUIET)
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
     X      'INTFAP: Twice forward through angle ', DLAT)
          LOK = JACOBI(ZNFELDI, ITRUNC, DWORK, DLAT)
          IF(.NOT.LOK) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTFAP: Spectral rotation failed',JPQUIET)
            INTFAP = JPROUTINE + 3
            GOTO 900
          ENDIF
          LOK = JACOBI(ZNFELDI, ITRUNC, DWORK, DLAT)
          IF(.NOT.LOK) THEN
            CALL INTLOG(JP_ERROR,
     X       'INTFAP: Spectral rotation failed',JPQUIET)
            INTFAP = JPROUTINE + 3
            GOTO 900
          ENDIF
C
C         .. and one backward
C
          DLAT = -90.0 - ( (LATLIM*2.0) - POLELAT)
          DLAT= -DLAT
          CALL INTLOGR(JP_DEBUG,
     X      'INTFAP: Once backward through angle ', DLAT)
        ENDIF
C
        LOK = JACOBI( ZNFELDI, ITRUNC, DWORK, DLAT)
          IF(.NOT.LOK) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTFAP: Spectral rotation failed',JPQUIET)
            INTFAP = JPROUTINE + 3
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
     X      'INTFAP: Twice forward through angle ', DLAT)
          LOK = JACOBI(ZNFELDI, ITRUNC, DWORK, DLAT)
          IF(.NOT.LOK) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTFAP: Spectral rotation failed',JPQUIET)
            INTFAP = JPROUTINE + 3
            GOTO 900
          ENDIF
          LOK = JACOBI(ZNFELDI, ITRUNC, DWORK, DLAT)
          IF(.NOT.LOK) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTFAP: Spectral rotation failed',JPQUIET)
            INTFAP = JPROUTINE + 3
            GOTO 900
          ENDIF
C
C         .. and one backward
C
          DLAT = -90.0 - ( (LATLIM*2.0) - POLELAT)
          DLAT= -DLAT
          CALL INTLOGR(JP_DEBUG,
     X      'INTFAP: Once backward through angle ', DLAT)
        ENDIF
C
        LOK = JACOBI( DATA, ITRUNC, DWORK, DLAT)
#endif
C
        IF(.NOT.LOK) THEN
          CALL INTLOG(JP_ERROR,'INTFAP: Spectral rotation fail',JPQUIET)
          INTFAP = JPROUTINE + 3
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
C*    Section 4.   Handle gaussian rotation, if necessary.
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
C     Is the interpolation from gaussian with a rotation of the
C     gaussian first?
C
      IF( ((NIREPR.EQ.JPGAUSSIAN).OR.(NIREPR.EQ.JPQUASI)) .AND.
     X    LNOROTA ) THEN
C
        CALL INTLOG(JP_DEBUG,'INTFAP: Rotate gaussian field.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'INTFAP: South pole latitude  ',NOROTA(1))
        CALL INTLOG(JP_DEBUG,'INTFAP: South pole longitude ',NOROTA(2))
C
C       Dynamically allocate memory for workspace.
C
        NUMPTS = ISEC4(1)
        ISIZE  = NUMPTS
        CALL JMEMHAN( 11, ISWORK, ISIZE, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFAP: memory allocation fail',ISWORK)
          INTFAP = JPROUTINE + 4
          GOTO 900
        ENDIF
C
C       Rotate the gaussian field.
C
        POLELAT = REAL(NOROTA(1))/JPMULT
        POLELON = REAL(NOROTA(2))/JPMULT
        NGAUSS = ISEC2(3)/2
        IF( NIREPR.EQ.JPGAUSSIAN ) THEN
          HTYPE = 'F'
        ELSE
          HTYPE = 'R'
        ENDIF
        IRET =NGROTAT(ZNFELDI,SWORK,NUMPTS,POLELAT,POLELON,HTYPE,NGAUSS)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFAP: Gaussian rotation fail',JPQUIET)
          INTFAP = JPROUTINE + 5
          GOTO 900
        ENDIF
C
C       Move rotated field back into field original array.
C
        DO LOOP = 1, NUMPTS
          ZNFELDI(LOOP) = SWORK(LOOP)
        ENDDO
C
      ENDIF
C
C
C     -----------------------------------------------------------------|
C*    Section 9.   Closedown.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
