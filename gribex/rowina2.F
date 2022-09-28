C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ROWINA2( P, KO, KI, PW, KCODE, PMSVAL, KRET)
C
C---->
C**** ROWINA2 - Interpolation of row of values.
C
C     Purpose.
C     --------
C
C     Interpolate a row of values.
C
C
C**   Interface.
C     ----------
C
C     CALL ROWINA2( P, KO, KI, PW, KCODE, PMSVAL, KRET)
C
C
C     Input Parameters.
C     -----------------
C
C     P      - Row of values to be interpolated.
C              Dimension must be at least KO.
C
C     KO     - Number of values required.
C
C     KI     - Number of values in P on input.
C
C     PW     - Working array.
C              Dimension must be at least (0:KO+2,3).
C
C     KCODE  - Interpolation required.
C              1 , linear.
C              3 , cubic.
C
C     PMSVAL - Value used for missing data indicator.
C
C
C     Output Parameters.
C     ------------------
C
C     P     - Now contains KO values.
C     KRET  - Return code
C             0, OK
C             Non-zero, error
C
C
C     Method.
C     -------
C
C     Linear or cubic interpolation performed as required.
C
C
C     Externals.
C     ----------
C
C     SCM0
C
C
C     Reference.
C     ----------
C
C     None.
C
C
C     Comments.
C     ---------
C
C     This is a version of ROWINA which allows for missing data
C     values and hence for bitmapped fields.
C
C
C     Author.
C     -------
C
C     J.D.Chambers    ECMWF     22.07.94
C
C
C     Modifications.
C     --------------
C
C     J.D.Chambers    ECMWF     13.09.94
C     Add return code KRET and remove calls to ABORT.
C
C
C     -----------------------------------------------------------------
C----<
C
C*******************************************************************
C     Section 0.  Declarations.
C*******************************************************************
C
      IMPLICIT NONE
C
C     Subroutine arguments
      REAL P, PW, PMSVAL
      INTEGER KO,KI,KCODE, KRET
      DIMENSION P(KO),PW(0:KO+2,3)
C
C     Local variables
      INTEGER JL, IP
      REAL ZRDI, ZDO, ZPOS, ZWT, ZWT1
C
C*******************************************************************
C     Section 1.  Linear interpolation ..
C*******************************************************************
C
  100 CONTINUE
C
      KRET = 0
C
      IF( KCODE.EQ.1 ) THEN
C
C        Move input values to work array
         DO 110 JL = 1, KI
            PW(JL,1) = P(JL)
  110    CONTINUE
C
C        Arrange wrap-around value in work array
         PW(KI+1,1) = P(1)
C
C        Set up constants to be used to figure out weighting for
C        values in interpolation.
         ZRDI = FLOAT(KI)
         ZDO  = 1.0 / FLOAT(KO)
C
C        Loop through the output points
         DO 120 JL = 1, KO
C
C           Calculate weight from the start of row
            ZPOS = (JL-1) * ZDO
            ZWT  = ZPOS * ZRDI
C
C           Get the current array position(minus 1) from the weight -
C           note the implicit truncation.
            IP   = ZWT
C
C           If the left value is missing, use the right value
            IF ( PW(IP+1,1) .EQ. PMSVAL ) THEN
               P(JL) = PW(IP+2,1)
C
C           If the right value is missing, use the left value
            ELSE IF ( PW(IP+2,1) .EQ. PMSVAL ) THEN
               P(JL) = PW(IP+1,1)
C
C           If neither missing, interpolate ...
            ELSE
C
C             Adjust the weight to range (0.0 to 1.0)
              ZWT  = ZWT - IP
C
C             Interpolate using the weighted values on either side
C             of the output point position
              P(JL) = (1.0-ZWT) * PW(IP+1,1) + ZWT * PW(IP+2,1)
            ENDIF
C
  120    CONTINUE
C
C*******************************************************************
C     Section 2.  Cubic interpolation ..
C*******************************************************************
C
  200 CONTINUE
C
      ELSEIF(KCODE.EQ.3) THEN
         DO 210 JL = 1,KI
            IF ( P(JL) .EQ. PMSVAL ) THEN
              WRITE(*,*) ' ROWINA2: Cubic interpolation not supported'
              WRITE(*,*) ' ROWINA2: for fields containing missing data.'
              WRITE(*,*) ' Sorry!'
              KRET = 1
              GOTO 900
            ENDIF
            PW(JL,1) = P(JL)
  210    CONTINUE
         PW(0,1) = P(KI)
         PW(KI+1,1) = P(1)
         PW(KI+2,1) = P(2)
         DO 220 JL = 1,KI
            PW(JL,2) =  - PW(JL-1,1)/3.0 - 0.5*PW(JL,1)
     1             + PW(JL+1,1)    - PW(JL+2,1)/6.0
            PW(JL+1,3) =    PW(JL-1,1)/6.0 - PW(JL,1)
     1             + 0.5*PW(JL+1,1) + PW(JL+2,1)/3.0
  220    CONTINUE
         CALL SCM0(PW(1,2),PW(2,3),PW(1,1),PW(2,1),KI)
         ZRDI = FLOAT(KI)
         ZDO = 1.0/FLOAT(KO)
         DO 230 JL = 1,KO
            ZPOS = (JL-1)*ZDO
            ZWT = ZPOS*ZRDI
            IP = ZWT+1
            ZWT = ZWT+1.0-IP
            ZWT1  =  1.0 - ZWT
            P(JL) = ((3.0-2.0*ZWT1)*PW(IP,1) + ZWT*PW(IP,2))*ZWT1*ZWT1
     1       + ((3.0-2.0*ZWT) *PW(IP+1,1) - ZWT1*PW(IP+1,3))*ZWT*ZWT
  230    CONTINUE
C
      ELSE
C
C*******************************************************************
C     Section 3.  Invalid interpolation code ..
C*******************************************************************
C
  300   CONTINUE
C
        WRITE (*,9001) KCODE
        KRET = 2
      ENDIF
C
C*******************************************************************
C     Section 9.  Closedown.
C*******************************************************************
C
900   CONTINUE
      RETURN
C
 9001 FORMAT (1H ,'ROWINA2 : Invalid interpolation code = ',I4)
C
      END
