C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGPLSM (POFELD, KLEN, KPR, KERR)
C
C---->
C**** *IGPLSM*
C
C     PURPOSE
C     _______
C
C     Force an interpolated land sea mask field back to a real 0-1
C     field.
C
C     INTERFACE
C     _________
C
C     IERR = IGPLSM (POFELD, KLEN, KPR, KERR)
C
C     Input parameters
C     ________________
C
C     POFELD     - The output field as previously calculated.
C
C     KLEN       - The length of the output field.
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
C     POFELD     - The 0-1 land sea mask output field
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
C
C     METHOD
C     ______
C
C     The intrinsic routine ANINT is used to create a real array of
C     the nearest integer value to the interpolated field. An
C     additional check is that all values less than zero are forced
C     to zero and all values greater than one are forced to one.
C
C     REFERENCE
C     _________
C
C     None
C
C     COMMENTS
C     ________
C
C     Program contains sections 0 to 2 and 9
C
C     AUTHOR
C     ______
C
C     K. Fielding      *ECMWF*      Feb 1994
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
C
C     Dummy arguments
      INTEGER KLEN, KPR, KERR
      REAL POFELD (KLEN)
C
C     Local variables
      INTEGER JLEN
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 25700)
C
C     _______________________________________________________
C
C*    Section 1. Initialisation
C     _______________________________________________________
C
  100 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IGPLSM: Section 1.',JPQUIET)
C
      IGPLSM = 0
C
      IF (KPR .GE. 1)
     X  CALL INTLOG(JP_DEBUG,'IGPLSM: Total fld len = ',KLEN)
C
C
C     _______________________________________________________
C
C*    Section 2. Force land sea mask to nearest integer (0 or 1)
C     _______________________________________________________
C
  200 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IGPLSM: Section 2.',JPQUIET)
C
      DO 210 JLEN = 1, KLEN
C
         POFELD (JLEN) = ANINT (POFELD (JLEN) )
C
         IF (POFELD (JLEN) .LT. PPZERO) POFELD (JLEN) = PPZERO
         IF (POFELD (JLEN) .GT. PPONE) POFELD (JLEN) = PPONE
C
  210 CONTINUE
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
  900 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IGPLSM: Section 9.',JPQUIET)
C
      RETURN
      END
