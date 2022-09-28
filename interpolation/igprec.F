C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGPREC (PIFELD, KIWE, KINS, POFELD, KOWE, KONS,
     1   KWEIND, KNSIND, PWFACT, PMAX, PINPNT, KPR, KERR)
C
C---->
C**** *IGPREC*
C
C     PURPOSE
C     _______
C
C     Perform additional interpolation processes for precipitation
C     fields.
C
C     INTERFACE
C     _________
C
C     IERR = IGPREC (PIFELD, KIWE, KINS, POFELD, KOWE, KONS, KWEIND,
C    1   KNSIND, PWFACT, PMAX, PINPNT, KPR, KERR)
C
C     Input parameters
C     ________________
C
C     PIFELD     - The input field provided by the calling routine.
C
C     KIWE       - The number of points in the West-East direction in
C                  the input field.
C
C     KINS       - The number of points in the North-South direction
C                  in the input field.
C
C     POFELD     - The output field as previously calculated.
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
C     PMAX       - Work array used to aid vectorisation.
C
C     PINPNT     - Work array used to aid vectorisation.
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
C     ________________
C
C     POFELD     - The output field as modified to allow for
C                  precipitation.
C
C     Return value
C     ____________
C
C     The error indicator (INTEGER).
C
C     Error and Warning Return Values
C     _______________________________
C
C     None
C
C     Common block usage
C     __________________
C
C     None
C
C     EXTERNALS
C     _________
C
C     INTLOG(R)  - Logs messages.
C     CHKPREC - Check if precipitation threshold has been redefined
C
C
C     METHOD
C     ______
C
C     The precipitation at a point is set to zero in the following
C     circumstances.
C
C     1  The interpolated precipitation is less than a threshold value.
C
C     2  There is no precipitation at the neighbouring point with the
C        highest interpolating weight.
C
C     NOTE that this operation is performed before the special
C     routines for the poles when interpolating from Gaussian grids
C     to global latitude/longitude fields. The polar values for
C     precipitation, during such interpolations, will always be the
C     average value of the nearest Gaussian line with no threshold
C     check.
C
C     REFERENCE
C     _________
C
C     None
C
C     COMMENTS
C     ________
C
C     Program contains sections 0 to 3 and 9
C
C     AUTHOR
C     ______
C
C     K. Fielding      *ECMWF*      Oct 1993
C
C     MODIFICATIONS
C     _____________
C
C     None
C
C----<
C     _______________________________________________________
C
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
C*    Prefix conventions for variable names
C
C     Logical      L (but not LP), global or common.
C                  O, dummy argument
C                  G, local variable
C                  LP, parameter.
C     Character    C, global or common.
C                  H, dummy argument
C                  Y (but not YP), local variable
C                  YP, parameter.
C     Integer      M and N, global or common.
C                  K, dummy argument
C                  I, local variable
C                  J (but not JP), loop control
C                  JP, parameter.
C     REAL         A to F and Q to X, global or common.
C                  P (but not PP), dummy argument
C                  Z, local variable
C                  PP, parameter.
C
C     Implicit statement to force declarations
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nifld.common"
C
C     Dummy arguments
      INTEGER KIWE, KOWE, KINS, KONS, KPR, KERR
      INTEGER KWEIND (2, KOWE), KNSIND (2, KONS)
      REAL PIFELD (KIWE, KINS), POFELD (KOWE, KONS)
      REAL PWFACT (4, KOWE, KONS)
      REAL PMAX (KOWE), PINPNT (KOWE)
C
C     Local variables
      LOGICAL LAT_RAIN
      INTEGER ILAT, IDIR
      INTEGER JOLAT, JOLON, JNS, JWE
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 22500)
C
C     _______________________________________________________
C
C*    Section 1. Initialisation
C     _______________________________________________________
C
  100 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IGPREC: Section 1.',JPQUIET)
C
      IGPREC = 0
C
      IF (KPR .GE. 1) THEN
        CALL INTLOG(JP_ERROR,'IGPREC: Input parameters.',JPQUIET)
        CALL INTLOG(JP_ERROR,'IGPREC: No.input fld longitudes = ',KIWE)
        CALL INTLOG(JP_ERROR,'IGPREC: No.input fld latitudes = ',KINS)
        CALL INTLOG(JP_ERROR,'IGPREC: No.output fld longitudes = ',KOWE)
        CALL INTLOG(JP_ERROR,'IGPREC: No.output fld latitudes = ',KONS)
      ENDIF
C
C     _______________________________________________________
C
C*    Section 2. eliminate output precipitation based on
C                interpolation threshhold
C     _______________________________________________________
C
  200 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IGPREC: Section 2.',JPQUIET)
C
      CALL CHKPREC()
C
      DO 360 JOLAT = 1, KONS
C
        LAT_RAIN = .FALSE.
C
        DO 210 JOLON = 1, KOWE
C
          IF (POFELD (JOLON, JOLAT) .LT. ZPRECIP) THEN
            POFELD (JOLON, JOLAT) = PPZERO
          ELSE
            LAT_RAIN = .TRUE.
          ENDIF
C
  210   CONTINUE
C
C     _______________________________________________________
C
C*    Section 3. eliminate output precipitation based on
C                "nearest" point.
C
C                Do not bother if there is no precipitation on this
C                line of latitude
C     _______________________________________________________
C
  300   CONTINUE
C
        IF ((KPR.GE.1).AND.(JOLAT.EQ.1)) CALL INTLOG(JP_DEBUG,
     X    'IGPREC: Section 3.',JPQUIET)
C
        IF (LAT_RAIN) THEN
C
          DO 310 JOLON = 1, KOWE
C
            PMAX (JOLON) = PPZERO
C
  310     CONTINUE
C
          DO 340 JNS = 1, 2
C
            ILAT = KNSIND (JNS, JOLAT)
C
            DO 330 JWE = 1, 2
C
              IDIR = (JNS - 1) * 2 + JWE
C
              DO 320 JOLON = 1, KOWE
C
                IF (PWFACT(IDIR, JOLON, JOLAT) .GT.  PMAX(JOLON)) THEN
                  PMAX (JOLON) = PWFACT (IDIR, JOLON, JOLAT)
                  PINPNT (JOLON) = PIFELD (KWEIND (JWE, JOLON), ILAT)
                ENDIF
C
  320         CONTINUE
C
  330       CONTINUE
C
  340     CONTINUE
C
          DO 350 JOLON = 1, KOWE
            IF (PINPNT(JOLON).LT.ZPRECIP) POFELD(JOLON,JOLAT) = PPZERO
  350     CONTINUE
C
        ENDIF
C
  360 CONTINUE
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
  900 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IGPREC: Section 9.',JPQUIET)
C
      RETURN
      END
