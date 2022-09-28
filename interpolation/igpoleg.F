C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGPOLEG(PIFELD, KIWE, POFELD, KOWE, KPR, KERR)
C
C---->
C**** *IGPOLEG*
C
C     Purpose
C     -------
C
C     Calculate the values at the pole of a regular
C     latitude/longitude field when the input is a Gaussian field.
C
C     This routine is used for any field except a wind field.
C
C     Interface
C     ---------
C
C     IERR = IGPOLEG(PIFELD, KIWE, POFELD, KOWE, KPR, KERR)
C
C     Input parameters
C     ----------------
C
C     PIFELD     - A "polar" row of the input field provided by the
C                  calling routine.
C
C     KIWE       - The number of points in the West-East direction in
C                  the input field.
C
C     KOWE       - The number of points in the West-East direction in
C                  the output field.
C
C     KPR        - The debug print switch.
C                  0  , No debugging output.
C                  1  , Produce debugging output.
C
C     KERR       - The error control flag.
C                  -ve, No error message. Return error code.
C                  0  , Hard failure with error message.
C                  +ve, Print error message. Return error code.
C
C     Output parameters
C     -----------------
C
C     POFELD     - The corresponding "polar" row of the output field
C                  returned to the calling routine.
C
C     Return value
C     ------------
C
C     The error indicator (INTEGER).
C
C
C     Common block usage
C     ------------------
C
C     None
C
C     Externals
C     ---------
C
C     INTLOG(R)    - Logs messages.
C
C     Method
C     ------
C
C     A Gaussian field does not have a line of latitude at the pole
C     so this routine forces all the values on the polar line of
C     latitude to the average of the values at the nearest Gaussian
C     line in the input field.
C
C     This method was originally implemented by
C
C     K. RIDER            * ECMWF *     OCTOBER 1991.
C
C     Reference
C     ---------
C
C     None
C
C     Comments
C     --------
C
C     Program contains sections 0 to 2 and 9
C
C     Author
C     ------
C
C     K. Fielding      *ECMWF*      Oct 1993
C
C     Modifications
C     -------------
C
C     Allow for missing data values
C     J.D.Chambers      ECMWF       August 2000
C
C----<
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nifld.common"
C
C     Function arguments
C
      INTEGER KIWE, KOWE, KPR, KERR
      REAL PIFELD(KIWE), POFELD(KOWE)
C
C     Local variables
C
      INTEGER JILON, JOLON, COUNT
      REAL ZSUM
C
C     Statement function
C
      REAL A, B
      LOGICAL NOTEQ
      NOTEQ(A,B) = (ABS((A)-(B)).GT.(ABS(A)*1E-3))
C
C     -----------------------------------------------------------------|
C*    Section 1. Initialisation
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGPOLEG: Section 1.',JPQUIET)
C
      IGPOLEG = 0
C
      IF( KPR.GE.1 ) THEN
        CALL INTLOG(JP_DEBUG,'IGPOLEG: No. of input fld longs. = ',KIWE)
        CALL INTLOG(JP_DEBUG,'IGPOLEG: No.of output fld longs. = ',KOWE)
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2. Use average value for non-winds
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGPOLEG: Section 2.',JPQUIET)
C
      ZSUM = PPZERO
      COUNT = 0
C
      IF( LIMISSV ) THEN
        DO JILON = 1, KIWE
          IF( NOTEQ(PIFELD(JILON),RMISSGV) ) THEN
            ZSUM = ZSUM + PIFELD(JILON)
            COUNT = COUNT + 1
          ENDIF
        ENDDO
      ELSE
        DO JILON = 1, KIWE
          ZSUM = ZSUM + PIFELD(JILON)
          COUNT = COUNT + 1
        ENDDO
      ENDIF
C
      IF( COUNT.GT.0 ) THEN
        ZSUM = ZSUM / REAL(COUNT)
      ELSE
        ZSUM = RMISSGV
      ENDIF
C
      DO JOLON = 1, KOWE
         POFELD(JOLON) = ZSUM
      ENDDO
C
C     -----------------------------------------------------------------|
C*    Section 9. Return to calling routine. Format statements
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGPOLEG: Section 9.',JPQUIET)
C
C
      RETURN
      END
