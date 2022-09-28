C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE REPCHR (HCHAR,HOLD,HNEW)
C
C---->
C**** REPCHR - Replace one character by another.
C
C     Purpose.
C     --------
C
C           Replace all occurrences of a given character in a
C           character string by another.
C
C**   Interface.
C     ----------
C
C           CALL REPCHR (HCHAR,HOLD,HNEW)
C
C           Integer    K.
C           Real       P.
C           Logical    O.
C           Character  H.
C
C               Input parameters.
C               -----------------
C
C               HCHAR      - Character string to be modified.
C               HOLD       - Character to be replaced.
C               HNEW       - Replacement character.
C
C               Output parameters.
C               ------------------
C
C               HCHAR      - Modified character string.
C
C     Method.
C     -------
C
C           Characters examined in sequence and changed if necessary.
C           Only the first character of the strings HOLD and HNEW is
C           used, if these are longer than 1 character.
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
C           Program contains sections 0 to 1 and section 9.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      16.01:92.
C
C     Modifications
C     --------------
C
C           None.
C
C     ----------------------------------------------------------------
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
C*    Section 0 . Definition of variables.
C     ----------------------------------------------------------------
C
      IMPLICIT NONE
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
      INTEGER      ILEN
C
      INTEGER      J150
C
      CHARACTER*(*) HCHAR
      CHARACTER*(*) HOLD
      CHARACTER*(*) HNEW
C
C     ----------------------------------------------------------------
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
C*    Section 1 . Replace required characters.
C     ----------------------------------------------------------------
C
  100 CONTINUE
C
      ILEN = LEN (HCHAR)
C
      DO 150 J150=1,ILEN
         IF (HCHAR(J150:J150).EQ.HOLD(1:1)) HCHAR(J150:J150) = HNEW(1:1)
  150 CONTINUE
C
C     ----------------------------------------------------------------
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
C*    Section 9 . Return to calling routine.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
C
      END
