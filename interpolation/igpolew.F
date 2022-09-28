C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGPOLEW(PIFELD, KIWE, POFELD, KOWE, KWEIND,
     1   KWEDIST, KPR, KERR)
C
C---->
C**** *IGPOLEW*
C
C     Purpose
C     -------
C
C     Calculates the values at the pole of a regular
C     latitude/longitude wind field when the input is a Gaussian field.
C
C     This routine is only used for a wind field.
C
C     Interface
C     ---------
C
C     IERR = IGPOLEW(PIFELD, KIWE, POFELD, KOWE, KWEIND, KWEDIST,
C    1   KPR, KERR)
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
C     KWEIND     - This array contains the array offsets of the West
C                  and East points in the input array required for
C                  interpolation.
C
C     KWEDIST    - This array holds the distances to longitude values
C                  in the input field from the associated longitude
C                  values in the output field.
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
C     Error and Warning Return Values
C
C     None
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
C     so this routine performs a linear interpolation of points on the
C     nearest Gaussian line and then puts these values into the output
C     array. The intention is to provide U and V values at the pole
C     which have a "directional" value.
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
C*    Section 0. Definition of variables.
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
      INTEGER KWEIND(2, KOWE), KWEDIST(2, KOWE)
      REAL PIFELD(KIWE), POFELD(KOWE)
C
C     Local variables
C
      INTEGER JOLON, COUNT
      REAL ZLOW, ZHIGH
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
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGPOLEW: Section 1.',JPQUIET)
C
      IGPOLEW = 0
C
      IF( KPR.GE.1 ) THEN
        CALL INTLOG(JP_DEBUG,'IGPOLEW: No.input fld longs = ',KIWE)
        CALL INTLOG(JP_DEBUG,'IGPOLEW: No.output fld longs = ',KOWE)
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2. Interpolation code for winds
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGPOLEW: Section 2.',JPQUIET)
C
      DO JOLON = 1, KOWE
C
C       Compute weigths for west and east neighbours
C
        ZLOW = REAL(KWEDIST(JP_I_E,JOLON)) /
     X      REAL(KWEDIST(JP_I_E,JOLON) + KWEDIST(JP_I_W,JOLON) )
C
        ZHIGH = PPONE - ZLOW
C
C       Count non-missing data values
C
        IF( LIMISSV ) THEN
          COUNT = 0
          IF( NOTEQ(PIFELD(KWEIND(JP_I_W,JOLON)),RMISSGV) )
     X      COUNT = COUNT + 1
          IF( NOTEQ(PIFELD(KWEIND(JP_I_E,JOLON)),RMISSGV) )
     X      COUNT = COUNT + 1
        ELSE
          COUNT = 2
        ENDIF
C
C       Interpolate using both neighbours if neither is missing
C
        IF( COUNT.EQ.2 ) THEN
          POFELD(JOLON) = ZLOW  * PIFELD(KWEIND(JP_I_W,JOLON)) +
     X                    ZHIGH * PIFELD(KWEIND(JP_I_E,JOLON))
C
C       Set missing if all neighbours are missing
C
        ELSE IF( COUNT.EQ.0 ) THEN
          POFELD(JOLON) = RMISSGV
C
C       Otherwise, use the nearest neighbour
C
        ELSE IF( ZLOW.GT.ZHIGH ) THEN
          POFELD(JOLON) = PIFELD(KWEIND(JP_I_W,JOLON))
        ELSE
          POFELD(JOLON) = PIFELD(KWEIND(JP_I_E,JOLON))
        ENDIF
C
      ENDDO
C
C     -----------------------------------------------------------------|
C*    Section 9. Return to calling routine. Format statements
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGPOLEW: Section 9.',JPQUIET)
C
      RETURN
      END
