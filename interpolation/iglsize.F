C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGLSIZE(KSTRIDE, KAREA, KNWE, KNNS, KLN,
     1   OWEGLOBE, ONPOLE, OSPOLE, KPR, KERR)
C
C---->
C**** *IGLSIZE*
C
C     PURPOSE
C     _______
C
C     Evaluate the array sizes for a regular latitude/longitude grid
C     and area.
C
C     INTERFACE
C     _________
C
C     IERR = IGLSIZE(KSTRIDE, KAREA, KNWE, KNNS, KLN, OWEGLOBE,
C    1   ONPOLE, OSPOLE, KPR, KERR)
C
C     Input parameters
C     ________________
C
C     KSTRIDE    - The field grid definition (WE, NS).
C
C     KAREA      - The field area definition (N, W, S, E) provided by
C                  the calling routine.
C
C     KNWE       - The number of points in the West-East direction in
C                  the field. This will be zero for an output field.
C
C     KNNS       - The number of points in the North-South direction
C                  in the field. This will be zero for an output field.
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
C     KNWE       - The number of points in the West-East direction in
C                  the field.
C
C     KNNS       - The number of points in the North-South direction
C                  in the field.
C
C     KLN        - The line number of the Northern latitude. This
C                  value is used during land sea mask processing to
C                  evaluate offsets in the "standard" files.
C
C     OWEGLOBE   - This variable will be true if the array of
C                  longitudes spans the globe.
C
C     ONPOLE     - This variable will be true if the North polar line
C                  of latitude is included in the grid.
C
C     OSPOLE     - This variable will be true if the South polar line
C                  of latitude is included in the grid.
C
C     Return value
C     ____________
C
C     The error indicator (INTEGER).
C
C     Error and Warning Return Values
C     _______________________________
C
C     -1    The West-East range does not match the stride and number
C           of points.
C     -2    The North-South range does not match the stride and
C           number of points.
C
C     The two problems above could be caused by known deficiencies
C     in the GRIB representation of ranges and strides. They will be
C     treated as WARNING values.
C
C     25101 A global field is requested with an inappropriate
C           West-East stride.
C     25102 A global field is requested with an inappropriate
C           North-South stride.
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
C     If all elements of array KAREA are zero then limits for the
C     appropriate global grid are generated.
C
C     Otherwise it is assumed that all the limits in KAREA have been
C     validated and the sizes of the arrays required are calculated.
C
C     REFERENCE
C     _________
C
C     None
C
C     COMMENTS
C     ________
C
C     Program contains sections 0 to 4 and 9
C
C     AUTHOR
C     ______
C
C     K. Fielding      *ECMWF*      Oct 1993
C
C     MODIFICATIONS
C     _____________
C
C     J.D.Chambers   ECMWF   March 1999
C     Use tolerance when testing E-W and N-S global span.
C
C----<
C     _______________________________________________________
C
C     Section 0. Definition of variables.
C     _______________________________________________________
C
      IMPLICIT NONE
C
#include "parim.h"
C
C     Function arguments
C
      LOGICAL OWEGLOBE, ONPOLE, OSPOLE
      INTEGER KNWE, KNNS, KLN, KPR, KERR
      INTEGER KSTRIDE(2), KAREA(4)
C
C     Parameters
C
      INTEGER JPROUTINE, JPEPSI
      PARAMETER (JPROUTINE = 25100)
      PARAMETER (JPEPSI = 50)
C                          `---> represents 0.5 millidegree to allow for
C                                grid step being off by upto 0.5 millidgrees
C
C
C     Local variables
C
      INTEGER IEAST, IWEST, IREACH, INORTH, ISOUTH
C
C     _______________________________________________________
C
C     Section 1. Initialisation
C     _______________________________________________________
C
  100 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGLSIZE: Section 1.',JPQUIET)
C
      IGLSIZE = 0
C
      IF( KPR.GE.1 ) THEN
        CALL INTLOG(JP_DEBUG,'IGLSIZE: Input parameters.',JPQUIET)
         IF( KNWE.NE.0 ) CALL INTLOG(JP_DEBUG,
     X     'IGLSIZE: No.long pts = ',KNWE)
         IF( KNNS.NE.0 ) CALL INTLOG(JP_DEBUG,
     X     'IGLSIZE: No.lat. pts = ',KNNS)
        CALL INTLOG(JP_DEBUG,'IGLSIZE: Grid WE stride = ',KSTRIDE(1))
        CALL INTLOG(JP_DEBUG,'IGLSIZE: Grid NS stride = ',KSTRIDE(2))
        CALL INTLOG(JP_DEBUG,'IGLSIZE: Area North = ', KAREA(1))
        CALL INTLOG(JP_DEBUG,'IGLSIZE: Area West  = ', KAREA(2))
        CALL INTLOG(JP_DEBUG,'IGLSIZE: Area South = ', KAREA(3))
        CALL INTLOG(JP_DEBUG,'IGLSIZE: Area East  = ', KAREA(4))
      ENDIF
C
C     _______________________________________________________
C
C     Section 2. Generate global limits if no limits set
C     _______________________________________________________
C
  200 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGLSIZE: Section 2.',JPQUIET)
C
      IF( (KAREA(JPEAST).EQ.0 )  .AND.
     X    (KAREA(JPWEST).EQ.0 )  .AND.
     X    (KAREA(JPNORTH).EQ.0 ) .AND.
     X    (KAREA(JPSOUTH).EQ.0 ) ) THEN
C
        IF( MOD(JP360,KSTRIDE(JPWESTEP)).NE.0 ) THEN
          IGLSIZE = JPROUTINE + 1
          IF( KERR.GE.0 ) THEN
            CALL INTLOGR(JP_ERROR,
     X        'IGLSIZE: West-East stride for global grid = ',
     X        REAL(KSTRIDE(JPWESTEP))/PPMULT)
            CALL INTLOGR(JP_ERROR,
     X        'IGLSIZE: This is not a factor of ',
     X        REAL(JP360)/PPMULT)
          ENDIF
C
          IF( KERR.EQ.0 ) CALL INTLOG(JP_FATAL,
     X      'IGLSIZE: Interpolation failed.',IGLSIZE)
C
          GO TO 900
        ENDIF
C
        IF( MOD(JP180,KSTRIDE(JPNSSTEP)).NE.0 ) THEN
          IGLSIZE = JPROUTINE + 2
          IF( KERR.GE.0 ) THEN
            CALL INTLOGR(JP_ERROR,
     X        'IGLSIZE: North-South stride for global grid = ',
     X        REAL(KSTRIDE(JPNSSTEP))/PPMULT)
            CALL INTLOGR(JP_ERROR,
     X        'IGLSIZE: This is not a factor of ',
     X        REAL(JP180)/PPMULT)
          ENDIF
C
          IF( KERR.EQ.0 ) CALL INTLOG(JP_FATAL,
     X      'IGLSIZE: Interpolation failed.',IGLSIZE)
C
          GO TO 900
        ENDIF
C
        KAREA(JPNORTH) = JP90
        KAREA(JPWEST) = 0
        KAREA(JPSOUTH) = - JP90
        KAREA(JPEAST) = JP360 - KSTRIDE(JPWESTEP)
C
      ENDIF
C
C     _______________________________________________________
C
C     Section 3. Standardise West-East limits and check for
C                West-East global fields
C     _______________________________________________________
C
  300 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGLSIZE: Section 3.',JPQUIET)
C
C     Force both KAREA(JPWEST) and KAREA(JPEAST) into standard
C     range (0 to JP360)
C
      IF( KAREA(JPWEST).LT.0 ) KAREA(JPWEST) = KAREA(JPWEST) + JP360
C
      IF( KAREA(JPEAST).LT.0 ) KAREA(JPEAST) = KAREA(JPEAST) + JP360
C
C     Standardise range so that EAST > WEST
C
      IEAST = KAREA(JPEAST)
      IWEST = KAREA(JPWEST)
C
      IF( IEAST.LT.IWEST ) THEN
C
        IF( IEAST.LT.0 ) THEN
          IEAST = IEAST + JP360
          KAREA(JPEAST) = IEAST
        ELSE
          IWEST = IWEST - JP360
          KAREA(JPWEST) = IWEST
        ENDIF
C
      ENDIF
C
      IF( KNWE.EQ.0 ) THEN
C
C       Is it a global grid ?
C
        IREACH = IEAST - IWEST + KSTRIDE(JPWESTEP)
C
        OWEGLOBE = IREACH.GE.JP360
C
C       Number of points along line is
C
        KNWE = NINT( FLOAT(IREACH) / FLOAT(KSTRIDE(JPWESTEP)) )
C
      ELSE
C
C       Number of points is set (input field)
C
        IF( ABS(IWEST+(KNWE-1)*KSTRIDE(JPWESTEP)-IEAST).GT.
     X      ((KNWE-1)*JPEPSI) ) THEN
C
C         WARNING of possible mismatch
C
          IGLSIZE = - 1
          CALL INTLOGR(JP_WARN,
     X      'IGLSIZE: West-East range = ',REAL(KAREA(JPWEST))/PPMULT)
          CALL INTLOGR(JP_WARN,
     X      'IGLSIZE: to = ',REAL(KAREA(JPEAST))/PPMULT)
          CALL INTLOG(JP_WARN,
     X      'IGLSIZE: Does not match no. of pts = ',KNWE)
          CALL INTLOGR(JP_WARN,
     X      'IGLSIZE: and stride = ',REAL(KSTRIDE(JPWESTEP))/PPMULT)
C
        ENDIF
C
        OWEGLOBE = ABS((KNWE * KSTRIDE(JPWESTEP))-JP360).LT.
     X             (KNWE*JPEPSI)
C
      ENDIF
C
C     _______________________________________________________
C
C     Section 4. Generate points along a line of meridian
C     _______________________________________________________
C
  400 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGLSIZE: Section 4.',JPQUIET)
C
      INORTH = KAREA(JPNORTH)
      ISOUTH = KAREA(JPSOUTH)
      IF( INORTH.LT.ISOUTH ) THEN
        INORTH = KAREA(JPSOUTH)
        ISOUTH = KAREA(JPNORTH)
      ENDIF
C
      IF( KNNS.EQ.0 ) THEN
C
C     Number of lines not set (output field)
C
        IREACH = INORTH - ISOUTH + KSTRIDE(JPNSSTEP)
C
        KNNS = NINT( FLOAT(IREACH) / FLOAT(KSTRIDE(JPNSSTEP)) )
C
      ELSE
C
C       Number of lines is set (input field)
C
        IF(
     X    ABS(INORTH-(KNNS-1)*KSTRIDE(JPNSSTEP)-ISOUTH)
     X    .GT.((KNNS-1)*JPEPSI) ) THEN
C
C     WARNING of possible mismatch
C
          IGLSIZE = - 2
          CALL INTLOGR(JP_WARN,
     X      'IGLSIZE: North-South range = ',REAL(INORTH)/PPMULT)
          CALL INTLOGR(JP_WARN,
     X      'IGLSIZE: to = ',REAL(ISOUTH)/PPMULT)
          CALL INTLOG(JP_WARN,
     X      'IGLSIZE: Does not match no. of pts = ',KNNS)
          CALL INTLOGR(JP_WARN,
     X      'IGLSIZE: and stride = ',REAL(KSTRIDE(JPNSSTEP))/PPMULT)
C
         ENDIF
C
      ENDIF
C
      KLN = (JP90 - INORTH ) / KSTRIDE(JPNSSTEP) + 1
C
C     Check if North and South poles within grid
C
      ONPOLE = INORTH.EQ.JP90
      OSPOLE = ISOUTH.EQ.(- JP90)
C
      IF( KPR.GE.1 ) THEN
        CALL INTLOG(JP_DEBUG,'IGLSIZE: Output parameters.',JPQUIET)
        IF( KNWE.NE.0 ) CALL INTLOG(JP_DEBUG,
     X    'IGLSIZE: No.long pts = ',KNWE)
        IF( KNNS.NE.0 ) CALL INTLOG(JP_DEBUG,
     X    'IGLSIZE: No.lat. pts = ',KNNS)
        CALL INTLOG(JP_DEBUG,'IGLSIZE: Northern line no. =',KLN)
C
        IF( OWEGLOBE ) THEN
          CALL INTLOG(JP_DEBUG,'IGLSIZE: W-E fld is global',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IGLSIZE: W-E fld is NOT global',JPQUIET)
        ENDIF
C
        IF( ONPOLE ) THEN
          CALL INTLOG(JP_DEBUG,'IGLSIZE: N pole is in fld.',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IGLSIZE: N pole is NOT in fld.',JPQUIET)
        ENDIF
C
        IF( OSPOLE ) THEN
          CALL INTLOG(JP_DEBUG,'IGLSIZE: S pole is in fld.',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IGLSIZE: S pole is NOT in fld.',JPQUIET)
        ENDIF
C
      ENDIF
C
C     _______________________________________________________
C
C     Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
  900 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGLSIZE: Section 9.',JPQUIET)
C
      RETURN
      END
