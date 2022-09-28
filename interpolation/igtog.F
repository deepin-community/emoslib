C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGTOG (PIFELD, KIWE, KINS, KOWE, KONS, KWEIND,
     1   KNSIND, PWFACT, POFELD, KPR, KERR)
C
C---->
C**** *IGTOG*
C
C     Purpose
C     -------
C
C     Perform basic interpolation between regular input and output
C     fields.
C
C     Interface
C     ---------
C
C     IERR = IGTOG (PIFELD, KIWE, KINS, KOWE, KONS, KWEIND, KNSIND,
C    1   PWFACT, POFELD, KPR, KERR)
C
C     Input parameters
C     ----------------
C
C     PIFELD     - The input field provided by the calling routine.
C
C     KIWE       - The number of points in the West-East direction in
C                  the input field.
C
C     KINS       - The number of points in the North-South direction
C                  in the input field.
C
C     KOWE       - The number of points in the West-East direction in
C                  the output field.
C
C     KONS       - The number of points in the North-South direction
C                  in the output field.
C
C     KWEIND     - This array contains the array offsets of the West
C                  and East points in the input array required for
C                  interpolation.
C
C     KNSIND     - This array contains the array offsets of the North
C                  and South points in the input array required for
C                  interpolation.
C
C     PWFACT     - The array of interpolating weights to the four
C                  neighbouring points for every output point.
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
C     POFELD     - The output field returned to the calling routine.
C
C     Return value
C     ------------
C
C     The error indicator (INTEGER).
C
C     Common block usage
C     ------------------
C
C     None
C
C     Externals
C     ---------
C
C     INTLOG - Logs messages
C     FORCED_NEAREST_NEIGHBOUR - check forced interpolation method
C
C
C     Method
C     ------
C
C     This routine performs basic linear interpolation using the four
C     neighbouring points in the input array to generate the output
C     array.
C
C     Reference
C     ---------
C
C     None
C
C     Comments
C     --------
C
C     None
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
C     Force nearest neighbour processing with env variable or
C     INTOUT parameter
C     S.Curic           ECMWF       September 2005
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
      INTEGER KIWE, KINS, KOWE, KONS, KPR, KERR
      INTEGER KWEIND (2, KOWE), KNSIND (2, KONS)
      REAL PIFELD (KIWE, KINS), POFELD (KOWE, KONS)
      REAL PWFACT (4, KOWE, KONS)
C
C     Local variables
C
      INTEGER ILATN, ILATS, JOLAT, JOLON, COUNT
      REAL NEAREST
      LOGICAL LVEGGY
C
C     Externals
C
      LOGICAL FORCED_NEAREST_NEIGHBOUR
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
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGTOG: Section 1.',JPQUIET)
C
      IGTOG = 0
C
      IF( KPR.GE.1 ) THEN
        CALL INTLOG(JP_DEBUG,'IGTOG: Input parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IGTOG: No.input fld longitudes = ',KIWE)
        CALL INTLOG(JP_DEBUG,'IGTOG: No.input fld latitudes  = ',KINS)
        CALL INTLOG(JP_DEBUG,'IGTOG: No.output fld longitudes = ',KOWE)
        CALL INTLOG(JP_DEBUG,'IGTOG: No.output fld latitudes  = ',KONS)
      ENDIF
C
C     Use nearest neighbour if required
      LVEGGY = FORCED_NEAREST_NEIGHBOUR(LMETHOD,NITABLE,NIPARAM)
      IF( LVEGGY ) CALL INTLOG(JP_DEBUG,
     X  'IGTOG: nearest neighbour processing',JPQUIET)
C
C     -----------------------------------------------------------------|
C*    Section 2. Basic interpolation
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGTOG: Section 2.',JPQUIET)
C
      DO JOLAT = 1, KONS
C
        ILATN = KNSIND(JP_I_N,JOLAT)
        ILATS = KNSIND(JP_I_S,JOLAT)
C
        DO JOLON = 1, KOWE
C
C         Count non-missing data values
C
          IF( LIMISSV ) THEN
            COUNT = 0
            IF( NOTEQ(PIFELD(KWEIND(JP_I_W,JOLON),ILATN), RMISSGV) )
     X        COUNT = COUNT + 1
            IF( NOTEQ(PIFELD(KWEIND(JP_I_W,JOLON),ILATS), RMISSGV) )
     X        COUNT = COUNT + 1
            IF( NOTEQ(PIFELD(KWEIND(JP_I_E,JOLON),ILATN), RMISSGV) )
     X        COUNT = COUNT + 1
            IF( NOTEQ(PIFELD(KWEIND(JP_I_E,JOLON),ILATS), RMISSGV) )
     X        COUNT = COUNT + 1
          ELSE
            COUNT = 4
          ENDIF
C
C         Interpolate using four neighbours if none are missing
C
          IF( (COUNT.EQ.4).AND.(.NOT.LVEGGY) ) THEN
            POFELD(JOLON,JOLAT) =
     X        PIFELD(KWEIND(JP_I_W,JOLON),ILATN) *
     X          PWFACT(JP_I_NW,JOLON,JOLAT) +
     X        PIFELD(KWEIND(JP_I_W,JOLON),ILATS) *
     X          PWFACT(JP_I_SW,JOLON,JOLAT) +
     X        PIFELD(KWEIND(JP_I_E,JOLON),ILATN) *
     X          PWFACT(JP_I_NE,JOLON,JOLAT) +
     X        PIFELD(KWEIND(JP_I_E,JOLON),ILATS) *
     X          PWFACT(JP_I_SE,JOLON,JOLAT)
C
C         Set missing if all neighbours are missing
C
          ELSE IF( COUNT.EQ.0 ) THEN
            POFELD(JOLON,JOLAT) = RMISSGV
C
C         Otherwise, use the nearest neighbour
C
          ELSE
            NEAREST = PWFACT(JP_I_NW,JOLON,JOLAT)
            POFELD(JOLON,JOLAT) =
     X        PIFELD(KWEIND(JP_I_W,JOLON),ILATN)
C
            IF( PWFACT(JP_I_NE,JOLON,JOLAT).GT.NEAREST ) THEN
              NEAREST = PWFACT(JP_I_NE,JOLON,JOLAT)
              POFELD(JOLON,JOLAT) =
     X          PIFELD(KWEIND(JP_I_E,JOLON),ILATN)
            ENDIF
C
            IF( PWFACT(JP_I_SW,JOLON,JOLAT).GT.NEAREST ) THEN
              NEAREST = PWFACT(JP_I_SW,JOLON,JOLAT)
              POFELD(JOLON,JOLAT) =
     X          PIFELD(KWEIND(JP_I_W,JOLON),ILATS)
            ENDIF
C
            IF( PWFACT(JP_I_SE,JOLON,JOLAT).GT.NEAREST ) THEN
              NEAREST = PWFACT(JP_I_SE,JOLON,JOLAT)
              POFELD(JOLON,JOLAT) =
     X          PIFELD(KWEIND(JP_I_E,JOLON),ILATS)
            ENDIF
C
          ENDIF
C
        ENDDO
C
      ENDDO
C
C     -----------------------------------------------------------------|
C*    Section 9. Return to calling routine. Format statements
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGTOG: Section 9.',JPQUIET)
C
      RETURN
      END

