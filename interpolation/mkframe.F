C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE MKFRAME(KWE,KNS,ZSEC4,ZMISS,KFRAME)
C
C---->
C**** MKFRAME
C
C     Purpose
C     -------
C
C     Remove points from inside a frame boundary.
C
C
C     Interface
C     ---------
C
C     CALL  MKFRAME(KWE,KNS,ZSEC4,ZMISS,KFRAME)
C
C     Input
C     -----
C
C     KWE    - Number of points west-east in input field.
C     KNS    - Number of points north-south in input field.
C     ZSEC4  - GRIBEX section 4 values in the field.
C     ZMISS  - Value to be used as the missing data value..
C     KFRAME - Number of points across the frame.
C
C
C     Output
C     ------
C
C     ZSEC4  - GRIBEX section 4 values with the points inside the frame
C              replaced by missing data values.
C
C
C     Method
C     ------
C
C     The field and frame are rectangular.
C
C
C     Externals
C     ---------
C
C     INTLOG  - Log error message.
C     INTLOGR - Log error message.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     May 1999.
C
C----<
C
      IMPLICIT NONE
C
C     Subroutine arguments
C
      INTEGER KWE,KNS, KFRAME
      REAL ZSEC4(*), ZMISS
C
C     Local variables
C
      INTEGER TOP, BOTTOM, LEFT, RIGHT, LOOPO, LOOPI, NEXT
C
#include "parim.h"
#include "jparams.h"
C
C ------------------------------------------------------------------
C*    Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
C
      IF( KFRAME.LT.1 ) GOTO 900
C
      TOP    = KFRAME + 1
      BOTTOM = KNS - KFRAME
      LEFT   = KFRAME + 1
      RIGHT  = KWE - KFRAME
C
      IF( NDBG.GT.0 ) THEN
        CALL INTLOG(JP_DEBUG,'MKFRAME: KFRAME = ', KFRAME)
        CALL INTLOG(JP_DEBUG,'MKFRAME: TOP = ', TOP)
        CALL INTLOG(JP_DEBUG,'MKFRAME: BOTTOM = ', BOTTOM)
        CALL INTLOG(JP_DEBUG,'MKFRAME: LEFT = ', LEFT)
        CALL INTLOG(JP_DEBUG,'MKFRAME: RIGHT = ', RIGHT)
        CALL INTLOGR(JP_DEBUG,'MKFRAME: ZMISS = ', ZMISS)
      ENDIF
C
      DO LOOPO = TOP, BOTTOM
        NEXT = (LOOPO-1)*KWE + (LEFT-1)
        DO LOOPI = LEFT, RIGHT
          NEXT = NEXT + 1
          ZSEC4(NEXT) = ZMISS
        ENDDO
      ENDDO
C
C ------------------------------------------------------------------
C*    Section 9.   Closedown.
C ------------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END
