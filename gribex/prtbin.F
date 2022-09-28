C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C


      SUBROUTINE PRTBIN (KIN,KNBIT,KOUT,KERR)
C
C---->
C**** PRTBIN - Binary to decimal conversion.
C
C     Purpose.
C     --------
C
C           Produces a decimal number with ones and zeroes
C           corresponding to the ones and zeroes of the input
C           binary number.
C           eg input number 1011 binary, output number 1011 decimal.
C
C**   Interface.
C     ----------
C
C           CALL PRTBIN (KIN,KNBIT,KOUT,KERR)
C
C           Integer    K.
C           Real       P.
C           Logical    O.
C           Character  H.
C
C               Input Parameters.
C               -----------------
C
C               KIN   - Integer variable containing binary number.
C
C               KNBIT - Number of bits in binary number.
C
C               Output Parameters.
C               -----------------
C
C               KOUT  - Integer variable containing decimal value
C                       with ones and zeroes corresponding to those of
C                       the input binary number.
C
C               KERR  - 0, If no error.
C                       1, Number of bits in binary number exceeds
C                          maximum allowed or is less than 1.
C
C     Method.
C     -------
C
C           Odd numbers have a binary representation ending in 1, even
C           numbers end in 0.
C
C     Externals.
C     ----------
C
C           None.
C
C     Reference.
C     ----------
C
C           None.
C
C     Comments.
C     ---------
C
C           Replaces earlier routine of the same name, which
C           contained non-ANSI code.
C           Routine contains sections 0, 1 and section 9.
C
C     Author.
C     -------
C
C           John Hennessy     ECMWF    01.10.91
C
C     Modifications.
C     --------------
C
C           None.
C
C     ---------------------------------------------------------------
C----<
C
C
C
C
C
C
C
C
C
C
C*    Section 0. Definition of variables. Check on parameters.
C     -----------------------------------------------------------------
C
C*    Prefix conventions for variable names.
C
C     Logical      L (but not LP), global or common.
C                  O, dummy arguments.
C                  G, local variable.
C                  LP, parameter.
C     Character    C, Global or common.
C                  H, dummy arguments.
C                  Y (but not YP), local variables.
C                  YP, parameter.
C     Integer      M and N, global or common.
C                  K, dummy arguments.
C                  I, local variables.
C                  J (but not JP), loop control.
C                  JP, parameter.
C     Real         A to F and Q to X, global or common.
C                  P (but not PP), dummy arguments.
C                  Z, local variables.
C                  PP, parameter.
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
      INTEGER IDEC
      INTEGER IK
      INTEGER ITEMP
C
      INTEGER J102
C
      INTEGER KERR
      INTEGER KIN
      INTEGER KNBIT
      INTEGER KOUT
C
C     Check length of binary number to ensure decimal number
C     generated will fit in the computer word - in this case will
C     it fit in a Cray 48 bit integer?
C
      IF (KNBIT.LT.1.OR.KNBIT.GT.14)
     C   THEN
             KERR = 1
             WRITE(GRPRSM,9000) KNBIT
             GO TO 900
         ELSE
             KERR = 0
         ENDIF
C
C     -----------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C
C*    Section 1. Generate required number.
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
      KOUT = 0
      IK   = KIN
      IDEC = 1
C
      DO 102 J102=1,KNBIT
         ITEMP = IK - ( (IK/2)*2 )
         KOUT  = KOUT + ITEMP * IDEC
         IK    = IK / 2
         IDEC  = IDEC * 10
  102 CONTINUE
C
C     -----------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C
C*    Section 9. Format statements. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
 9000 FORMAT (1H ,'PRTBIN : Error in binary number length - ',I3,
     C            ' bits.')
C
      RETURN
      END
