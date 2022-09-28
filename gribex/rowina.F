C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C


      SUBROUTINE ROWINA (P,KO,KI,PW,KCODE)
C
C---->
C**** ROWINA - Interpolation of row of values.
C
C     Purpose.
C     --------
C
C           Interpolate a row of values.
C
C**   Interface.
C     ----------
C
C           CALL ROWINA (P,KO,KI,PW,KCODE)
C
C           Integer    K.
C           Real       P.
C           Logical    O.
C           Character  H.
C
C               Input Parameters.
C               -----------------
C
C               P     - Row of values to be interpolated.
C                       Dimension must be at least KO.
C
C               KO    - Number of values required.
C
C               KI    - Number of values in P on input.
C
C               PW    - Working array.
C                       Dimension must be at least (0:KO+2,3).
C
C               KCODE - Interpolation required.
C                       1 , linear.
C                       3 , cubic.
C
C               Output Parameters.
C               ------------------
C
C               P     - Now contains KO values.
C
C     Method.
C     -------
C
C           Linear or cubic interpolation performed as required.
C
C     Externals.
C     ----------
C
C           SCM0
C
C     Reference.
C     ----------
C
C           None.
C
C     Comments.
C     ---------
C
C           This is a version of ROWINT which conforms to Ansi
C           standards, achieved by passing the work array as a
C           parameter and changing lower case letters to upper case.
C
C     Author.
C     -------
C
C           J. Hennessy     ECMWF     09.10.91
C
C     Modifications.
C     --------------
C
C           J. Hennessy     ECMWF     07.01.92
C           Call to ABORT changed to ABORTX.
C
C     -----------------------------------------------------------------
C----<
C
      DIMENSION P(KO),PW(0:KO+2,3)
C
      IF(KCODE.EQ.1) THEN
         DO 102 JL=1,KI
            PW(JL,1)=P(JL)
  102    CONTINUE
         PW(KI+1,1)=P(1)
         ZRDI=REAL(KI)
         ZDO=1./REAL(KO)
C
         DO 105 JL=1,KO
            ZPOS=(JL-1)*ZDO
            ZWT=ZPOS*ZRDI
            IP=ZWT
            ZWT=ZWT-IP
            P(JL)=(1.-ZWT)*PW(IP+1,1)+ZWT*PW(IP+2,1)
  105    CONTINUE
C
      ELSEIF(KCODE.EQ.3) THEN
         DO 302 JL=1,KI
            PW(JL,1)=P(JL)
  302    CONTINUE
         PW(0,1)=P(KI)
         PW(KI+1,1)=P(1)
         PW(KI+2,1)=P(2)
         DO 305 JL=1,KI
            PW(JL,2)= - PW(JL-1,1)/3. - 0.5*PW(JL,1)
     1             + PW(JL+1,1)    - PW(JL+2,1)/6.
            PW(JL+1,3)=   PW(JL-1,1)/6. - PW(JL,1)
     1             + 0.5*PW(JL+1,1) + PW(JL+2,1)/3.
  305    CONTINUE
         CALL SCM0(PW(1,2),PW(2,3),PW(1,1),PW(2,1),KI)
         ZRDI=REAL(KI)
         ZDO=1./REAL(KO)
         DO 310 JL=1,KO
            ZPOS=(JL-1)*ZDO
            ZWT=ZPOS*ZRDI
            IP=ZWT+1
            ZWT=ZWT+1.-IP
            ZWT1 = 1. - ZWT
            P(JL)=((3.-2.*ZWT1)*PW(IP,1) + ZWT*PW(IP,2))*ZWT1*ZWT1
     1       + ((3.-2.*ZWT) *PW(IP+1,1) - ZWT1*PW(IP+1,3))*ZWT*ZWT
  310    CONTINUE
      ELSE
         WRITE (*,9001) KCODE
         CALL ABORTX ('ROWINA')
      ENDIF
C
      RETURN
C
 9001 FORMAT (1H ,'ROWINA : Invalid interpolation code = ',I4)
C
      END
