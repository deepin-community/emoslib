C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HGETLSM(KGAUSS,HTYPE,OLDLSM,KSIZE)
C
C---->
C**** HGETLSM
C
C     Purpose
C     -------
C
C     This routine reads a file of reduced gaussian field land-sea
C     mask values.
C
C
C     Interface
C     ---------
C
C     IRET = HGETLSM(KGAUSS,OLDLSM,KSIZE)
C
C
C     Input parameters
C     ----------------
C
C     KGAUSS - Gaussian number of the reduced gaussian field
C     HTYPE  - Gaussian type of the reduced gaussian field ('R','O')
C     KSIZE  - Number of bytes to read into OLDLSM
C
C
C     Output parameters
C     -----------------
C
C     OLDLSM - The array of values for the reduced gaussian field
C
C     Returns 0 if function successful, non-zero otherwise.
C
C     Common block usage
C     ------------------
C
C     None
C
C
C     Method
C     ------
C
C     Gets pathname from HIRLAM_LSM_PATH and reads land-sea mask
C     from a file.
C
C
C     Externals
C     ---------
C
C     INTLOG  - Log error message.
C     PBOPEN  - Open land-sea mask file
C     PBREAD  - Read land-sea mask file
C     PBCLOSE - Close land-sea mask file
C
C
C     Reference
C     ---------
C
C     None.
C
C
C     Comments
C     --------
C
C     None.
C
C
C     Author
C     ------
C
C     J.D.Chambers      ECMWF      March 2001
C
C
C     Modifications
C     -------------
C
C     Replace PBOPEN, etc by PBOPEN3, etc on VPP.
C     J.D.Chambers      ECMWF      December 2001
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
C
C     Function arguments
C
      INTEGER KGAUSS
      CHARACTER*1 HTYPE
      REAL OLDLSM(*)
      INTEGER KSIZE
C
C     Local variables
C
      CHARACTER*256 FILENAME
      CHARACTER*4 YGAUSS
      INTEGER IOFFS, IUNIT, IRET
C
C     Externals
C
      INTEGER DPATH_TABLES_INTERPOL
      EXTERNAL DPATH_TABLES_INTERPOL
C
C     -----------------------------------------------------------------|
C     Section 1.  Initialise.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      HGETLSM = 0
C
      CALL JDEBUG()
C
C     -----------------------------------------------------------------|
C     Section 2.  Build the land-sea mask file pathname
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Build LSM directory path
C
      FILENAME = ' '
      IOFFS = DPATH_TABLES_INTERPOL(FILENAME)
      IF (IOFFS.EQ.0) THEN
        CALL INTLOG(JP_DEBUG,
     X    'HGETLSM: unable to build LSM directory path.',JPQUIET)
        HGETLSM = 2
        GOTO 900
      ENDIF
C
C     Complete the filename
C
      IF( HTYPE.EQ.'R' ) THEN
        WRITE(YGAUSS,'(I4.4)') KGAUSS
        FILENAME = FILENAME(1:IOFFS) // 'LSM_GG_' // YGAUSS
      ELSE
        WRITE(YGAUSS,'(I0)') KGAUSS
        FILENAME = FILENAME(1:IOFFS) // 'LSM_' // HTYPE // YGAUSS
      ENDIF
      IOFFS = INDEX(FILENAME,' ')
C
      IF (LNNLSM_CY41R1) FILENAME(IOFFS:) = '_CY41R1'
      IOFFS = INDEX(FILENAME,' ')
C
      CALL INTLOG(JP_DEBUG,'HGETLSM: land-sea mask filename',JPQUIET)
      CALL INTLOG(JP_DEBUG,'HGETLSM: ' // FILENAME(1:(IOFFS-1)),JPQUIET)
C
C     -----------------------------------------------------------------|
C     Section 3.  Read the land-sea mask values
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      CALL PBOPEN(IUNIT,FILENAME(1:(IOFFS-1)),'r',IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_DEBUG,'HGETLSM: PBOPEN Iret',IRET)
        CALL INTLOG(JP_DEBUG,'HGETLSM: PBOPEN failed',JPQUIET)
        HGETLSM = 3
        GOTO 900
      ENDIF
C
      CALL PBREAD(IUNIT,OLDLSM,KSIZE,IRET)
      IF( IRET.NE.KSIZE ) THEN
        CALL INTLOG(JP_DEBUG,'HGETLSM: PBREAD failed',JPQUIET)
        HGETLSM = 3
        GOTO 900
      ENDIF
C
      CALL PBCLOSE(IUNIT,IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_DEBUG,'HGETLSM: PBCLOSE failed',JPQUIET)
        HGETLSM = 3
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C     Section 9.  Return.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
