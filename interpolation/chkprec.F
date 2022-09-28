C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CHKPREC()
C
C---->
C**** CHKPREC
C
C     Purpose
C     -------
C
C     Determines whether or not the current field is to be
C     considered for 'precipitation' treatment.
C
C
C     Interface
C     ---------
C
C     CALL CHKPREC()
C
C     Input
C     -----
C
C     None
C
C
C     Output
C     ------
C
C     Sets the precipitation threshold if give by environment variable
C     PRECIPITATION_THRESHOLD. Otherwise gives it a default value.
C
C
C     Method
C     ------
C
C     See below.
C
C
C     Externals
C     ---------
C
C     INTLOG  - Logs messages
C
C     Uses common block nifld.common
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     December 2003
C
C----<
C
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nifld.common"
C
C     Local variables
C
      CHARACTER*20 PTHRESH
      INTEGER IBLANK
      LOGICAL LFIRST
      DATA LFIRST/.TRUE./
      SAVE LFIRST
C
C     -----------------------------------------------------------------|
C*    Section 1. Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      IF( LFIRST ) THEN
        LFIRST = .FALSE.
        CALL GETENV('PRECIPITATION_THRESHOLD',PTHRESH)
        IBLANK = INDEX(PTHRESH, ' ')
        IF( IBLANK.GT.1 ) THEN
          READ(PTHRESH,'(F15.8)') ZPRECIP
        ELSE
          ZPRECIP = 0.00005
        ENDIF
C
        CALL INTLOGR(JP_DEBUG,
     X    'CHKPREC: PRECIPITATION_THRESHOLD = ',ZPRECIP)
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9.   Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
      RETURN
      END
