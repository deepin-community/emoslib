C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGNORM (PWFACT, PSUM, KOWE, KONS, KPR, KERR)
C
C---->
C**** *IGNORM*
C
C     PURPOSE
C     _______
C
C     Normalise the array of interpolation weights.
C
C     INTERFACE
C     _________
C
C     IERR = IGNORM (PWFACT, PSUM, KOWE, KONS, KPR, KERR)
C
C     Input parameters
C     ________________
C
C     PWFACT     - The array of unnormalised interpolating weights to
C                  the four neighbouring points for every output point.
C
C     PSUM       - Work array used to aid vectorisation.
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
C     PWFACT     - The normalised array of interpolating weights to
C                  the four neighbouring points for every output point.
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
C     The four weights for each output point are normalised to have a
C     sum of one.
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
C
C     Dummy arguments
      INTEGER KOWE, KONS, KPR, KERR
      REAL PWFACT (4, KOWE, KONS), PSUM (KOWE)
C
C     Local variables
      INTEGER JOLAT, JOLON, JDIR
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 25600)
C
C     _______________________________________________________
C
C*    Section 1. Initialisation
C     _______________________________________________________
C
  100 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IGNORM: Section 1.',JPQUIET)
C
      IGNORM = 0
C
      IF (KPR .GE. 1) THEN
        CALL INTLOG(JP_DEBUG,'IGNORM: No of output longitudes = ',KOWE)
        CALL INTLOG(JP_DEBUG,'IGNORM: No of output latitudes  = ',KONS)
      ENDIF
C
C     _______________________________________________________
C
C*    Section 2. Calculate arrays of weights
C     _______________________________________________________
C
  200 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IGNORM: Section 2.',JPQUIET)
C
      DO 260 JOLAT = 1, KONS
C
         DO 210 JOLON = 1, KOWE
C
            PSUM (JOLON) = PPZERO
C
  210    CONTINUE
C
         DO 230 JDIR = 1, 4
            DO 220 JOLON = 1, KOWE
C
               PSUM (JOLON) = PSUM (JOLON) +
     1            PWFACT (JDIR, JOLON, JOLAT)
C
  220       CONTINUE
  230    CONTINUE
C
         DO 250 JDIR = 1, 4
            DO 240 JOLON = 1, KOWE
C
               PWFACT (JDIR, JOLON, JOLAT) =
     1            PWFACT (JDIR, JOLON, JOLAT) / PSUM (JOLON)
C
  240       CONTINUE
  250    CONTINUE
C
  260 CONTINUE
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
  900 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IGNORM: Section 9.',JPQUIET)
C
C
      RETURN
      END
