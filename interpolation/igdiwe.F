C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGDIWE(KIWE,KOWE,KWEIND,KWEDIST,IAREA,OAREA,
     .  KILONG,KOLONG,OWEGLOBE)  !FIXME: remove
C
C---->
C**** *IGDIWE*
C
C     PURPOSE
C     _______
C
C     Calculate the distances between points in an output longitude
C     array and its West and East neighbours in the input longitude
C     array.
C
C     INTERFACE
C     _________
C
C     IERR = IGDIWE(KIWE,KOWE,KWEIND,KWEDIST,IAREA,OAREA)
C
C     Input parameters
C     ________________
C
C     KIWE       - The number of points in the West-East direction in
C                  the input field.
C
C     KOWE       - The number of points in the West-East direction in
C                  the output field.
C
C     IAREA      - Input field area definition (N/W/S/E).
C
C     OAREA      - Input field area definition (N/W/S/E).
C
CTEMP KILONG     - The array of input longitudes.
CTEMP
CTEMP KOLONG     - The array of output longitudes.
CTEMP
CTEMP OWEGLOBE   - This variable will be true if the input array of
CTEMP              longitudes spans the globe.
C
C     Output parameters
C     ________________
C
C     KWEIND     - This array will contain the array offsets of the
C                  West and East points in the input array required
C                  for interpolation.
C
C     KWEDIST    - This array holds the distances to longitude values
C                  in the input field from the associated longitude
C                  values in the output field.
C
C     Return value
C     ____________
C
C     The error indicator (INTEGER).
C
C----<

      IMPLICIT NONE
#include "parim.h"
#include "nofld.common"

C     Arguments
      INTEGER KIWE
      INTEGER KOWE
      INTEGER IAREA(4)
      INTEGER OAREA(4)
      INTEGER KWEIND (2,KOWE)
      INTEGER KWEDIST(2,KOWE)

C     Local variables
      INTEGER I, J, IA, IB
      LOGICAL LSTAGGL, LSUBAR, LSUBARO  ! (to avoid globals)

C     Arguments & local variables (FIXME: remove)
      INTEGER KILONG(KIWE+1), KOLONG(KOWE)
      LOGICAL OWEGLOBE
      INTEGER IBASE, ILOW, IUP
      REAL LO, LO1, LO2, IDELTA, ODELTA

C     NOTE: subarea/staggered definition copied from areachk.F:218
C     testing shows that:
C       LGLOBL   = .NOT. LSUBARO
C       OWEGLOBE = .NOT. LSUBAR
      LSUBARO = (OAREA(1).NE.0) .AND. (OAREA(2).NE.0) .AND.
     .          (OAREA(3).NE.0) .AND. (OAREA(4).NE.0)
      LSUBAR  = (IAREA(1).NE.0) .AND. (IAREA(2).NE.0) .AND.
     .          (IAREA(3).NE.0) .AND. (IAREA(4).NE.0)
      LSTAGGL = (LSUBAR) .AND.
     .          (IABS(JP360 - IAREA(4) - IAREA(2)).LT.1000)


      IF( LSTAGGL .OR. LSUBAR ) THEN

!       CALL INTLOG(JP_DEBUG,
!    .    'IGDIWE: Staggered grids not supported', JPQUIET )

        CALL INTLOG(JP_DEBUG,'IGDIWE: algorithm: old :-(',JPQUIET)

C       OLD METHOD:
C       This sets the arrays of offsets and distance (of length
C       (2, Number of output longitudes)), used for all latitude rows when
C       both input & and output grids are regular. It is used to provide
C       the values for one line of latitude for quasi regular Gaussian
C       input fields.


        IGDIWE = 0
        IF (OWEGLOBE) THEN

          DO I = 1, KIWE

C           NOTE entry #1 is repeated as point KIWE + 1 to simplify the wrap around
            ILOW = KILONG (I)
            IUP  = KILONG (I + 1)
            IF (IUP .GT. ILOW) THEN

              DO J = 1, KOWE

                IF (KOLONG(J).GE.ILOW .AND.  KOLONG(J).LT.IUP) THEN

C                 Normal case IUP > ILOW: longitude point found
                  KWEIND (JP_I_W, J) = I
                  IF (I .LT. KIWE) THEN
                    KWEIND (JP_I_E, J) = I + 1
                  ELSE
                    KWEIND (JP_I_E, J) = 1
                  ENDIF
                  KWEDIST (JP_I_W, J) = KOLONG (J) - ILOW
                  KWEDIST (JP_I_E, J) = IUP - KOLONG (J)

                ENDIF

              ENDDO

            ELSE

              DO J = 1, KOWE

                IF (KOLONG (J) .GE. ILOW) THEN

C                 Wrap around case: longitude point found between ILOW and 3600000
                  KWEIND (JP_I_W, J) = I
                  IF (I .LT. KIWE) THEN
                    KWEIND (JP_I_E, J) = I + 1
                  ELSE
                    KWEIND (JP_I_E, J) = 1
                  ENDIF
                  KWEDIST (JP_I_W, J) = KOLONG (J) - ILOW
                  KWEDIST (JP_I_E, J) = IUP + JP360 - KOLONG (J)

                ENDIF

                IF (KOLONG (J) .LE. IUP) THEN

C                 Wrap around case: longitude point found between 0 and IUP
                  KWEIND (JP_I_W, J) = I
                  IF (I .LT. KIWE) THEN
                    KWEIND (JP_I_E, J) = I + 1
                  ELSE
                    KWEIND (JP_I_E, J) = 1
                  ENDIF
                  KWEDIST (JP_I_W, J) = KOLONG (J) + JP360 - ILOW
                  KWEDIST (JP_I_E, J) = IUP - KOLONG (J)

                ENDIF

              ENDDO

            ENDIF

          ENDDO

        ELSE

C         last point found for non-global input grids
          IBASE = 1

          DO J = 1, KOWE

            DO I = IBASE, KIWE - 1

              ILOW = KILONG (I)
              IUP = KILONG (I + 1)

              IF (KOLONG(J).GE.ILOW .AND. KOLONG(J).LE.IUP) THEN

C               Normal case IUP > ILOW: longitude point found
                KWEIND (JP_I_W, J) = I
                KWEIND (JP_I_E, J) = I + 1
                IBASE = I
                KWEDIST (JP_I_W, J) = KOLONG (J) - ILOW
                KWEDIST (JP_I_E, J) = IUP - KOLONG (J)
                GOTO 250

              ELSE IF (IUP .LT. ILOW) THEN

                IF (KOLONG (J) .GE. ILOW) THEN

C                 Wrap around case: longitude point found between ILOW and 3600000
                  KWEIND (JP_I_W, J) = I
                  KWEIND (JP_I_E, J) = I + 1
                  IBASE = I
                  KWEDIST (JP_I_W, J) = KOLONG (J) - ILOW
                  KWEDIST (JP_I_E, J) = IUP + JP360 - KOLONG (J)
                  GOTO 250

                ENDIF

                IF (KOLONG (J) .LE. IUP) THEN

C                 Wrap around case: longitude point found between 0 and IUP
                  KWEIND (JP_I_W, J) = I
                  KWEIND (JP_I_E, J) = I + 1
                  IBASE = I
                  KWEDIST (JP_I_W, J) = KOLONG (J) + JP360 - ILOW
                  KWEDIST (JP_I_E, J) = IUP - KOLONG (J)
                  GOTO 250

                ENDIF

              ENDIF

            ENDDO

C           This statement will be reached if the output grid does not
C           fit within the input grid.
            IGDIWE = 6
            LO  = REAL(KOLONG(J))/PPMULT
            LO1 = REAL(KILONG(1))/PPMULT
            LO2 = REAL(KILONG(KIWE))/PPMULT
      CALL INTLOG( JP_ERROR,'IGDIWE: Output long. value number = ',J)
      CALL INTLOGR(JP_ERROR,'IGDIWE: Output long. value ',LO)
      CALL INTLOGR(JP_ERROR,'IGDIWE: outside input range = ',LO1)
      CALL INTLOGR(JP_ERROR,'IGDIWE: to ',LO2)
      CALL INTLOG(JP_FATAL,'IGDIWE: Interpolation failing.',JPQUIET)
            GOTO 900

  250     CONTINUE
          ENDDO

        ENDIF
  900   CONTINUE

      ELSE

        CALL INTLOG(JP_DEBUG,'IGDIWE: algorithm: new :-)',JPQUIET)

        IF( LGLOBL ) THEN

          DO J = 1, KOWE

            I = FLOOR( REAL((J-1)*KIWE)/REAL(KOWE) ) + 1
            KWEIND (JP_I_W,J) = I
            KWEIND (JP_I_E,J) = MOD(I,KIWE)+1

!C           original:
!            IKIWE = JP360 / KIWE
!            IKOWE = JP360 / KOWE
!            IA = (J-1)*IKOWE - (I-1)*IKIWE
!            IB = ( I )*IKIWE - (J-1)*IKOWE  ! (same as: IKIWE - A)

            IA = (J-1)*KIWE - (I-1)*KOWE
            IB = ( I )*KOWE - (J-1)*KIWE

!C           adimensionalize from KIWE*KOWE space (optional):
!            IA = INT( REAL(IA*JP360)*REAL(KIWE*KOWE) )
!            IB = INT( REAL(IB*JP360)*REAL(KIWE*KOWE) )

            KWEDIST(JP_I_W,J) = IA
            KWEDIST(JP_I_E,J) = IB

          ENDDO

        ELSE

C         (this also works globally, but it isn't precision-independent)
C         set grid increments
          IDELTA = REAL(360)/REAL(KIWE)
          ODELTA = REAL(360)/REAL(KOWE)
          IF( .NOT.LGLOBL .AND. KOWE.GT.1 ) THEN
            ODELTA = REAL(OAREA(4)-OAREA(2))/REAL(JPMULT*(KOWE-1))
          ENDIF

          LO1 = REAL(OAREA(2))/REAL(JPMULT)
          DO J = 1, KOWE

            LO = REAL(J-1)*ODELTA + LO1
            DO WHILE (LO.LT.0.)
              LO = LO + 360.
            ENDDO
            DO WHILE (LO.GE.360.)
              LO = LO - 360.
            ENDDO

            I = MOD(FLOOR( LO/IDELTA ),KIWE) + 1
            KWEIND (JP_I_W,J) = I
            KWEIND (JP_I_E,J) = MOD(I,KIWE)+1

            IA = INT(REAL(JPMULT)*(LO-REAL(I-1)*IDELTA   ))
           !IB = INT(REAL(JPMULT)*(   REAL( I )*IDELTA-LO)) (same as below)
            IB = INT(REAL(JPMULT)*IDELTA) - IA
            DO WHILE (IA.LT.0 .OR. IB.LT.0)
              IA = IA + INT(REAL(JPMULT)*IDELTA)
              IB = IB + INT(REAL(JPMULT)*IDELTA)
            ENDDO

            KWEDIST(JP_I_W,J) = IA
            KWEDIST(JP_I_E,J) = IB

          ENDDO

        ENDIF
        IGDIWE = 0

      ENDIF
      END
