C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION ZPREC (POFELD, KOWE, KONS, KPR, KERR)
C
C---->
C**** *ZPREC*
C
C     PURPOSE
C     _______
C
C     Perform additional interpolation processes for precipitation
C     fields
C
C     INTERFACE
C     _________
C
C     IERR = ZPREC ( POFELD, KOWE, KONS, KPR, KERR)
C
C     Input parameters
C     ________________
C
C     POFELD     - The output field as previously calculated.
C
C     KOWE       - The number of points in the West-East direction in
C                  the output field.
C
C     KONS       - The number of points in the North-South direction
C                  in the output field.
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
C     INTLOG(R)    - Logs messages.
C     CHKPREC - Check if precipitation threshold has been redefined
C
C     METHOD
C     ______
C
C     The precipitation at a point is set to zero if the value is less
C     then threshold 
C
C     REFERENCE
C     _________
C
C     None
C
C     COMMENTS
C     ________
C
C     Program contains sections 0 to 3
C
C     AUTHOR
C     ______
C
C     S. Curic      *ECMWF*      Sep 2005
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
      INTEGER KOWE, KONS, KPR, KERR
      REAL POFELD (KOWE, KONS)
C
C     Local variables
      INTEGER JOLAT, JOLON
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 23500)
C
C     _______________________________________________________
C
C*    Section 1. Initialisation
C     _______________________________________________________
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'ZPREC: Section 1.',JPQUIET)
C
      ZPREC = 0
C
      IF (KPR .GE. 1) THEN
        CALL INTLOG(JP_DEBUG,'ZPREC: No. of output fld lats = ',KONS)
        CALL INTLOG(JP_DEBUG,'ZPREC: No. of output fld longs = ',KOWE)
      ENDIF
C
C     _______________________________________________________
C
C*    Section 2. eliminate output negative  precipitation
C     _______________________________________________________
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IRPREC: Section 2.',JPQUIET)
C
       CALL CHKPREC()
C
      DO 360 JOLAT = 1, KONS
C
         DO 210 JOLON = 1, KOWE
C
            IF (POFELD (JOLON, JOLAT) .LT. ZPRECIP) THEN
               POFELD (JOLON, JOLAT) = PPZERO
            ENDIF
C
  210    CONTINUE
C
C
  360 CONTINUE
C
C     _______________________________________________________
C
C
C*    Section 3. Return to calling routine. Format statements
C     _______________________________________________________
C
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'ZPREC: Section 3.',JPQUIET)
C
      RETURN
      END
