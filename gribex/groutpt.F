C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GROUTPT()
C
C---->
C**** GROUTPT
C
C     Purpose.
C     --------
C     Changes diagnostic output stream if defined by environment variable.
C
C**   Interface.
C     ----------
C     CALL GROUTPT()
C
C
C     Input Parameters.
C     -----------------
C     None.
C
C
C     Output Parameters.
C     ------------------
C     None.
C
C
C     Method.
C     -------
C     Checks environment variable GRPRS_STREAM for output stream number
C     for printing (default is 6)
C
C
C     Externals.
C     ----------
C     Common block GRPRSCM.
C
C
C     Reference.
C     ----------
C     None.
C
C
C     Comments.
C     ---------
C     None.
C
C
C     Author.
C     -------
C
C     J.D.Chambers   ECMWF    June 2002
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C
C     Local variables
C
      INTEGER IOFFSET
      CHARACTER*10 YNUMBER
C
C     -----------------------------------------------------------------|
C*    Section 1 .
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
C     See if output stream needs changing
C
      CALL GETENV('GRPRS_STREAM', YNUMBER)
      IOFFSET = INDEX( YNUMBER, ' ')
      IF( IOFFSET.GT.1 ) THEN
        IF( IOFFSET.EQ.2 ) THEN
          READ(YNUMBER,'(I1.1)') GRPRSM
        ELSE IF( IOFFSET.EQ.3 ) THEN
          READ(YNUMBER,'(I2.2)') GRPRSM
        ELSE
          WRITE(GRPRSM,*)
     X    'GROUTPT: Invalid value for GRPRS_STREAM: ' // YNUMBER
          GRPRSM = 6
        ENDIF
      ELSE
        GRPRSM = 6
      ENDIF
C
      IF( GRPRSM.LT.1 ) THEN
        WRITE(*,*) 'GROUTPT: Invalid number for GRPRS_STREAM: ', GRPRSM
        GRPRSM = 6
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9 . Format statements. Return to calling routine.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
