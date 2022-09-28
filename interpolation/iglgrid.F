C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGLGRID (KSTRIDE, KAREA, KLONG, KNWE, KLATG,
     1   KNNS, OWEGLOBE, KPR, KERR)
C
C---->
C**** *IGLGRID*
C
C     PURPOSE
C     _______
C
C     Generate the arrays of latitude and longitude points for a
C     regular latitude longitude grid.
C
C     INTERFACE
C     _________
C
C     IERR = IGLGRID (KSTRIDE, KAREA, KLONG, KNWE, KLATG, KNNS,
C    1   OWEGLOBE, KPR, KERR)
C
C     Input parameters
C     ________________
C
C     KSTRIDE    - The grid definition (WE, NS) provided by the
C                  calling routine.
C
C     KAREA      - The area definition (N, W, S, E) provided by the
C                  calling routine.
C
C     KNWE       - The number of points in the West-East direction in
C                  this field.
C
C     KNNS       - The number of points in the North-South direction
C                  in this field.
C
C     OWEGLOBE   - This variable will be true if the array of
C                  longitudes spans the globe and this is an input
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
C     KLONG      - The array of longitudes for the stride and area.
C                  If OEWGLOBE is true the West-East field array has
C                  element KNWE + 1 set equal to element 1 to help
C                  with recognising where an output grid 
C                  "wraps around" the break in the input grid.
C
C     KLATG      - The array of latitudes for the stride and area.
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
C     The arrays of longitudes and latitudes are generated using the
C     information provided by the other parameters.
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
C
C     Dummy arguments
      LOGICAL OWEGLOBE
      INTEGER KNWE, KNNS, KPR, KERR
      INTEGER KSTRIDE (2), KAREA (4)
      INTEGER KLONG (KNWE + 1), KLATG (KNNS)
C
C     Local variables
      INTEGER IEAST, IWEST
      INTEGER JLONG, JLAT
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 24900)
C
C     _______________________________________________________
C
C*    Section 1. Initialisation
C     _______________________________________________________
C
  100 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IGLGRID: Section 1.',JPQUIET)
C
      IGLGRID = 0
C
      IF (KPR .GE. 1) THEN
        CALL INTLOG(JP_DEBUG,'IGLGRID: Input parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IGLGRID: No.long.pts = ',KNWE)
        CALL INTLOG(JP_DEBUG,'IGLGRID: No.lat. pts = ',KNNS)
        CALL INTLOG(JP_DEBUG,'IGLGRID: Grid stride WE = ',KSTRIDE(1))
        CALL INTLOG(JP_DEBUG,'IGLGRID: Grid stride NS = ',KSTRIDE(2))
        CALL INTLOG(JP_DEBUG,'IGLGRID: Area North = ', KAREA(1))
        CALL INTLOG(JP_DEBUG,'IGLGRID: Area West  = ', KAREA(2))
        CALL INTLOG(JP_DEBUG,'IGLGRID: Area South = ', KAREA(3))
        CALL INTLOG(JP_DEBUG,'IGLGRID: Area East  = ', KAREA(4))
        IF( OWEGLOBE ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'IGLGRID: Input W-E fld is global.',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,
     X      'IGLGRID: Input W-E fld is NOT global.',JPQUIET)
        ENDIF
      ENDIF
C
      IEAST = KAREA (JPEAST)
      IWEST = KAREA (JPWEST)
C
C     _______________________________________________________
C
C
C*    Section 2. Generate points around a line of latitude.
C     _______________________________________________________
C
  200 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IGLGRID: Section 2.',JPQUIET)
C
C     Standardise range so that EAST > WEST
C
      IF (IEAST .LT. IWEST ) THEN
C
         IF (IEAST .LT. 0) THEN
            IEAST = IEAST + JP360
         ELSE
            IWEST = IWEST - JP360
         ENDIF
C
      ENDIF
C
C     Generate points in standardised range (0, jp360)
C
      DO 210 JLONG = 1, KNWE
C
         KLONG (JLONG) = IWEST + KSTRIDE (JPWESTEP) * (JLONG - 1)
C
         IF (KLONG (JLONG) .LT. 0)
     1      KLONG (JLONG) = KLONG (JLONG) + JP360
C
  210 CONTINUE
C
C     Add first point to end of list if global
C
      IF (OWEGLOBE)
     1   KLONG (KNWE + 1) = KLONG (1)
C
C     _______________________________________________________
C
C
C*    Section 3. Generate points along a line of meridian
C     _______________________________________________________
C
  300 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IGLGRID: Section 3.',JPQUIET)
C
      DO 310 JLAT = 1, KNNS
C
         KLATG (JLAT) = KAREA (JPNORTH) -
     1      KSTRIDE (JPNSSTEP) * (JLAT - 1)
C
  310 CONTINUE
C
C     _______________________________________________________
C
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
  900 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IGLGRID: Section 9.',JPQUIET)
C
      RETURN
      END
