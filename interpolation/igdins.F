C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGDINS (KILATG, KINS, KOLATG, KONS, ONPOLE,
     1   OSPOLE, KNSIND, KNSDIST, KPR, KERR)
C
C---->
C**** *IGDINS*
C
C     PURPOSE
C     _______
C
C     Calculate the distances between points in an output latitude
C     array and its North and South neighbours in the input latitude
C     array.
C
C     INTERFACE
C     _________
C
C     IERR = IGDINS (KILATG, KINS, KOLATG, KONS, ONPOLE, OSPOLE,
C    1   KNSIND, KNSDIST, KPR, KERR)
C
C     Input parameters
C     ________________
C
C     KILATG     - The array of input latitudes.
C
C     KINS       - The number of points in the North-South direction
C                  in the input field.
C
C     KOLATG     - The array of output latitudes.
C
C     KONS       - The number of points in the North-South direction
C                  in the output field.
C
C     ONPOLE     - This variable will be true if the input field
C                  includes the most Northerly line of latitude for
C                  the field type. This means latitude 90 North for
C                  a regular field and the first line of a Gaussian
C                  field.
C
C     OSPOLE     - This variable will be true if the input field
C                  includes the most Southerly line of latitude for
C                  the field type. This means latitude 90 South for
C                  a regular field and the last line of a Gaussian
C                  field.
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
C     KNSIND     - This array will contain the array offsets of the
C                  North and South points in the input array required
C                  for interpolation.
C
C     KNSDIST    - This array will contain the distances to lines
C                  in input field from the associated lines of
C                  latitude in the output field.
C
C     Return value
C     ____________
C
C     The error indicator (INTEGER).
C
C     Error and Warning Return Values
C     _______________________________
C
C     24501 An output latitude was found that was outside the area
C           spanned by the input latitudes.
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
C     This routine will create the arrays of offsets and distance as
C     two arrays of length (2, Number of output latitudes).
C
C     If an input Gaussian grid includes the "polar" line and the
C     output field includes a line of latitude between this "polar"
C     line and the pole then the position of this "polar" Gaussian
C     line is assumed to be at the pole for that line of latitude only.
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
      LOGICAL ONPOLE, OSPOLE
      INTEGER KINS, KONS, KPR, KERR
      INTEGER KILATG (KINS), KOLATG (KONS)
      INTEGER KNSIND (2, KONS), KNSDIST (2, KONS)
C
C     Local variables
      INTEGER IBASE
      INTEGER JOUT, JINP
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 24500)
C
C     _______________________________________________________
C
C*    Section 1. Initialisation
C     _______________________________________________________
C
  100 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IGDINS: Section 1.',JPQUIET)
C
      IGDINS = 0
C
      IF (KPR .GE. 1) THEN
        CALL INTLOG(JP_DEBUG,'IGDINS: Input parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IGDINS: No.input fld lines = ',KINS)
        CALL INTLOG(JP_DEBUG,'IGDINS: No.output fld lines = ',KONS)
        CALL INTLOG(JP_DEBUG,'IGDINS: Input fld spread from ',KILATG(1))
        CALL INTLOG(JP_DEBUG,'IGDINS: to ', KILATG(KINS))
        CALL INTLOG(JP_DEBUG,'IGDINS: Outpt fld spread from ',KOLATG(1))
        CALL INTLOG(JP_DEBUG,'IGDINS: to ', KOLATG(KONS))
C
        IF ( ONPOLE ) THEN
          CALL INTLOG(JP_DEBUG,'IGDINS: N. pole in inpt field.',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IGDINS: N.pole NOT in inpt fld',JPQUIET)
        ENDIF
C
        IF ( OSPOLE ) THEN
          CALL INTLOG(JP_DEBUG,'IGDINS: S. pole in inpt field.',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IGDINS: S.pole NOT in inpt fld',JPQUIET)
        ENDIF
      ENDIF
C
C     _______________________________________________________
C
C*    Section 2. Select points along a line of meridian
C     _______________________________________________________
C
  200 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IGDINS: Section 2.',JPQUIET)
C
C     Last latitude found to be used as start of search
C     for next latitude
C
      IBASE = 1
C
      DO 220 JOUT = 1, KONS
C
C       Allow special code for points beyond North limit of
C       Gaussian grid which has all points up to pole
C
C       Sinisa remove check for pole - already exist in iagcntl
        IF (IBASE .EQ. 1 .AND. KOLATG(JOUT).GT.KILATG (1) ) THEN
C
          KNSIND (JP_I_N, JOUT) = 1
          KNSIND (JP_I_S, JOUT) = 2
          IBASE = 1
C
          KNSDIST (JP_I_N, JOUT) = JP90 - KOLATG (JOUT)
          KNSDIST (JP_I_S, JOUT) = KOLATG (JOUT) - KILATG (2)
C
          GO TO 220
C
        ENDIF
C
        DO 210 JINP = IBASE, KINS - 1
C
C         This test allows the exact match at both ends to be treated
C         correctly. Although the point is checked for equality at
C         both ends the first test will be used except for the South
C         Gaussian limit.
C
          IF (KOLATG (JOUT) .LE. KILATG (JINP) .AND.
     X         KOLATG (JOUT) .GE. KILATG (JINP + 1) ) THEN
C
            KNSIND (JP_I_N, JOUT) = JINP
            KNSIND (JP_I_S, JOUT) = JINP + 1
            IBASE = JINP
C
            KNSDIST (JP_I_N, JOUT) = KILATG (JINP) -
     X            KOLATG (JOUT)
            KNSDIST (JP_I_S, JOUT) = KOLATG (JOUT) -
     X            KILATG (JINP + 1)
C
            GO TO 220
C
          ENDIF
C
  210   CONTINUE
C
C       Allow special code for points beyond South limit of
C       Gaussian grid which has all points up to pole
C
C       Sinisa remove check for pole - already exist in iagcntl
        IF (KOLATG (JOUT) .LT. KILATG (KINS) ) THEN
C
          KNSIND (JP_I_N, JOUT) = KINS - 1
          KNSIND (JP_I_S, JOUT) = KINS
          IBASE = KINS
C
          KNSDIST (JP_I_N, JOUT) = KILATG (KINS - 1) -
     X         KOLATG (JOUT)
          KNSDIST (JP_I_S, JOUT) = KOLATG (JOUT) - JP90
C
        ELSE
C
          IGDINS = JPROUTINE + 1
          IF (KERR .GE. 0) THEN
            CALL INTLOGR(JP_ERROR,
     X        'IGDINS: Output lat. value ',REAL(KOLATG(JOUT))/PPMULT)
            CALL INTLOGR(JP_ERROR,
     X        'IGDINS: outside input range = ',REAL(KILATG(1))/PPMULT)
            CALL INTLOGR(JP_ERROR,
     X        'IGDINS: to ',REAL(KILATG(KINS))/PPMULT)
          ENDIF
C
          IF (KERR .EQ. 0) CALL INTLOGR(JP_FATAL,
     X      'IGDINS: Interpolation failing.',IGDINS)
C
          GO TO 900
C
        ENDIF
C
  220 CONTINUE
C
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
  900 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IGDINS: Section 9.',JPQUIET)
C
      RETURN
      END
