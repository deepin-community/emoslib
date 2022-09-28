C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IRDIWE(KLLEN,KINS,KNSIND,KONS,KILONG,KIWEMAX,
     1  KOLONG,KOWE,KWEIND,KWEDIST,IAREA,OAREA,KPR,KERR)
C
C---->
C**** *IRDIWE*
C
C     PURPOSE
C     _______
C
C     Calculate the distances between points in an output longitude
C     array and its West and East neighbours in the input longitude
C     array for a quasi regular input field and regular output field.
C
C     INTERFACE
C     _________
C
C     IERR = IRDIWE(KLLEN,KINS,KNSIND,KONS,KILONG,KIWEMAX,
C    1  KOLONG,KOWE,KWEIND,KWEDIST,IAREA,OAREA,KPR,KERR)
C
C     Input parameters
C     ________________
C
C     KLLEN      - This array contains the quasi regular Gaussian
C                  line length definition.
C
C     KINS       - The number of points in the North-South direction
C                  in the input field.
C
C     KNSIND     - This array will contain the array offsets of the
C                  North and South points in the input array required
C                  for interpolation.
C
C     KONS       - The number of points in the North-South direction
C                  in the output field.
C
C     KILONG     - The array of input longitudes.
C
C     KIWEMAX    - The maximum number of points in the West-East
C                  direction for this quasi regular Gaussian grid.
C
C     KOLONG     - The array of output longitudes.
C
C     KOWE       - The number of points in the West-East direction in
C                  the output field.
C
C     IAREA      - Input field area definition (N/W/S/E).
C
C     OAREA      - output field area definition (N/W/S/E).
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
C     KWEIND     - This array will contain the array offsets of
C                  the West and East points in the input array
C                  required for interpolation for all lines of
C                  latitude.
C
C     KWEDIST    - This array holds the distances to longitude values
C                  in the input field from the associated longitude
C                  values in the output field. This array is
C                  dimensioned (2, KOWE, 2, KONS).
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
C     IGDIWE     - Calculate the distances between points in an
C                  output longitude array and its West and East
C                  neighbours in the input longitude array for one
C                  line of latitude.
C     INTLOG(R)  - Logs messages.
C
C     METHOD
C     ______
C
C     This routine will create the arrays of offsets and distance as
C     two arrays of length (2, Number of output longitudes, 2,
C     Number of output latitudes).
C
C     The array of offsets is copied from line to line provided the
C     number of points in the quasi regular lines of latitude does
C     not change. When the definition changes IGDIWE is used to
C     generate the offsets and distances for the new line definition.
C
C----<

C     ------------------------------------------------------------------
C*    Section 0. Definition of variables
C     ------------------------------------------------------------------
      IMPLICIT NONE

#include "parim.h"

C     Arguments
      INTEGER IAREA(4)
      INTEGER OAREA(4)
      INTEGER KINS
      INTEGER KONS
      INTEGER KIWEMAX
      INTEGER KOWE
      INTEGER KPR
      INTEGER KERR
      INTEGER KLLEN (KINS)
      INTEGER KNSIND (2,KONS)
      INTEGER KILONG (KIWEMAX+1)
      INTEGER KOLONG (KOWE)
      INTEGER KWEIND (2,KOWE,2,KONS)
      INTEGER KWEDIST(2,KOWE,2,KONS)

C     Local variables
      LOGICAL GWEGLOBE
      INTEGER KIWE
      INTEGER ISTRIDE
      INTEGER IERR
      INTEGER IBLON
      INTEGER IBNS
      INTEGER IBLAT
      INTEGER JLAT
      INTEGER JLON
      INTEGER JNS
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 23900)

C     External functions
      INTEGER IGDIWE
      EXTERNAL IGDIWE

C     ------------------------------------------------------------------
C*    Section 1. Initialisation
C     ------------------------------------------------------------------
  100 CONTINUE

      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IRDIWE: Section 1.',JPQUIET)

      IRDIWE = 0

      IF (KPR .GE. 1) THEN
        CALL INTLOG(JP_DEBUG,'IRDIWE: Input parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IRDIWE: Max no.input longs  = ',KIWEMAX)
        CALL INTLOG(JP_DEBUG,'IRDIWE: No.input lats       = ',KINS)
        CALL INTLOG(JP_DEBUG,'IRDIWE: No.output longs     = ',KOWE)
        CALL INTLOG(JP_DEBUG,'IRDIWE: No.output lats      = ',KONS)
        CALL INTLOG(JP_DEBUG,'IRDIWE: Output field spread = ',KOLONG(1))
        CALL INTLOG(JP_DEBUG,'IRDIWE: to ',KOLONG(KOWE))
      ENDIF

      GWEGLOBE = .TRUE.
      IBLON = 0
      IERR = 0

C     ------------------------------------------------------------------
C*    Section 2. Generate distances and points for all lines of latitude
C     ------------------------------------------------------------------
  200 CONTINUE

      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IRDIWE: Section 2.',JPQUIET)

      IBLAT = 1
      IBNS  = 1

      DO JLAT = 1, KONS
        DO JNS = 1, 2

          KIWE = KLLEN(KNSIND(JNS,JLAT))

          IF (KIWE .EQ. IBLON) THEN

C          Same line definition as last one calculated
           KWEIND (1:2,1:KOWE,JNS,JLAT) = KWEIND (1:2,1:KOWE,IBNS,IBLAT)
           KWEDIST(1:2,1:KOWE,JNS,JLAT) = KWEDIST(1:2,1:KOWE,IBNS,IBLAT)

          ELSE

            IBLON = KIWE
            IBNS  = JNS
            IBLAT = JLAT

C           Generate longitude values for this line            ! FIXME: remove
C           (wrap around input grid to simplify IGDIWE)        ! ...
            ISTRIDE = JP360 / KIWE                             ! ...
            DO JLON = 1, KIWE                                  ! ...
              KILONG (JLON) = (JLON - 1) * ISTRIDE             ! ...
            ENDDO                                              ! ...
            KILONG(KIWE + 1) = KILONG (1)                      ! ...

            IRDIWE = IGDIWE( KIWE, KOWE,
     1        KWEIND  (1,1,JNS,JLAT),
     2        KWEDIST (1,1,JNS,JLAT),
     3        IAREA, OAREA,
     4        KILONG, KOLONG, GWEGLOBE )   ! FIXME: remove
            IF (IRDIWE.NE.0) GO TO 900

          ENDIF

        ENDDO
      ENDDO

C     ------------------------------------------------------------------
C*    Section 9. Return to calling routine. Format statements
C     ------------------------------------------------------------------
  900 CONTINUE

      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IRDIWE: Section 9.',JPQUIET)
      END

