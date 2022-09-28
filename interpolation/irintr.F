C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IRINTR (KWEDIST, KOWE, KNSDIST, PWFACT, KPR,
     1   KERR)
C
C---->
C**** *IRINTR*
C
C     PURPOSE
C     _______
C
C     Calculate the basic unnormalised interpolation weights when
C     interpolating from a quasi regular grid to a regular grid for
C     one line of latitude.
C
C     INTERFACE
C     _________
C
C     IERR = IRINTR (KWEDIST, KOWE, KNSDIST, PWFACT, KPR, KERR)
C
C     Input parameters
C     ________________
C
C     KWEDIST    - This array holds the distances to longitude values
C                  in the input field from the associated longitude
C                  values in the output field.
C
C     KOWE       - The number of points in the West-East direction in
C                  the output field.
C
C     KNSDIST    - This array of length 2 gives the distances to the
C                  lines of latitude North and South in the input
C                  field from the line of latitude in the output field.
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
C     PWFACT     - The unnormalised array of interpolating weights to
C                  the four neighbouring points for every point of the
C                  output line of latitude.
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
C
C     METHOD
C     ______
C
C     The weights are formed by performing a linear fit along the
C     two lines of latitude. These two partial weights are normalised
C     before performing a linear fit along the line of meridian.
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
C     K. Fielding      *ECMWF*      Nov 1993
C
C     MODIFICATIONS
C     _____________
C
C     NONE
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
      INTEGER KOWE, KPR, KERR
      INTEGER KWEDIST (2, KOWE, 2), KNSDIST (2)
      REAL PWFACT (4, KOWE)
C
C     Local variables
      REAL ZSOUTH, ZNORTH
      INTEGER JOLON
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 23300)
C
C     _______________________________________________________
C
C*    Section 1. Initialisation
C     _______________________________________________________
C
  100 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IRINTR: Section 1.',JPQUIET)
C
      IRINTR = 0
C
      IF (KPR .GE. 1) THEN
        CALL INTLOG(JP_DEBUG,'IRINTR: Input parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IRINTR: No.output longitudes = ',KOWE)
      ENDIF
C
C     _______________________________________________________
C
C*    Section 2. Calculate arrays of weights
C     _______________________________________________________
C
  200 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IRINTR: Section 2.',JPQUIET)
C
      DO 210 JOLON = 1, KOWE
C
         ZNORTH = KWEDIST (JP_I_W, JOLON, JP_I_N) +
     X      KWEDIST (JP_I_E, JOLON, JP_I_N)
C
         ZSOUTH = KWEDIST (JP_I_W, JOLON, JP_I_S) +
     X      KWEDIST (JP_I_E, JOLON, JP_I_S)
C
         PWFACT (JP_I_NW, JOLON) =
     X      REAL (KWEDIST (JP_I_E, JOLON, JP_I_N)) *
     X      ZSOUTH * REAL (KNSDIST (JP_I_S) )
C
         PWFACT (JP_I_NE, JOLON) =
     X      REAL (KWEDIST (JP_I_W, JOLON, JP_I_N)) *
     X      ZSOUTH * REAL (KNSDIST (JP_I_S) )
C
         PWFACT (JP_I_SW, JOLON) =
     X      REAL (KWEDIST (JP_I_E, JOLON, JP_I_S)) *
     X      ZNORTH * REAL (KNSDIST (JP_I_N) )
C
         PWFACT (JP_I_SE, JOLON) =
     X      REAL (KWEDIST (JP_I_W, JOLON, JP_I_S)) *
     X      ZNORTH * REAL (KNSDIST (JP_I_N) )
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
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IRINTR: Section 9.',JPQUIET)
C
      RETURN
      END
