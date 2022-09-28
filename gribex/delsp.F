C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE DELSP (HCHAR)
C
C---->
C**** DELSP - Delete spaces around valid MARS directive separators.
C
C     Purpose.
C     --------
C
C           Removes any spaces before and after any characters
C           encountered in input string, which are valid separators
C           in a MARS command line. These are / , = " .
C           Any spaces between minus sign and number are also removed.
C
C**   Interface.
C     ----------
C
C           CALL DELSP (HCHAR)
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
C
C               Output parameters.
C               ------------------
C
C               HCHAR      - Modified character string.
C
C     Method.
C     -------
C
C           Uses index function to locate any spaces to be
C           removed.
C
C     Externals.
C     ----------
C
C           RTB
C
C     Reference.
C     ----------
C
C           None.
C
C     Comments.
C     ---------
C
C           Program contains sections 0 to 2 and section 9.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      01.09:86.
C
C     Modifications
C     --------------
C
C           J. Hennessy      ECMWF      30.03.87
C           Prevent looping when string ends with a separator.
C
C           J. Hennessy      ECMWF      18.07.91
C           Other separators added.
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
      IMPLICIT  NONE
C
      CHARACTER*(*)      HCHAR
C
      INTEGER            I
      INTEGER            IEND
      INTEGER            RTB
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
C*    Section 1 . Remove any spaces before separators.
C     ----------------------------------------------------------------
C
c--      WRITE (*,9000)
c--      WRITE (*,9002) HCHAR
C
  100 CONTINUE
C
      I = INDEX (HCHAR(1:),' /')
C
      IF (I.NE.0)
     C   THEN
             HCHAR(I:) = HCHAR(I+1:)
             GO TO 100
         ENDIF
C
  110 CONTINUE
C
      I = INDEX (HCHAR(1:),' ,')
C
      IF (I.NE.0)
     C   THEN
             HCHAR(I:) = HCHAR(I+1:)
             GO TO 110
         ENDIF
C
  120 CONTINUE
C
      I = INDEX (HCHAR(1:),' =')
C
      IF (I.NE.0)
     C   THEN
             HCHAR(I:) = HCHAR(I+1:)
             GO TO 120
         ENDIF
C
  130 CONTINUE
C
      I = INDEX (HCHAR(1:),' "')
C
      IF (I.NE.0)
     C   THEN
             HCHAR(I:) = HCHAR(I+1:)
             GO TO 130
         ENDIF
C
  140 CONTINUE
C
      I = INDEX (HCHAR(1:),' .')
C
      IF (I.NE.0)
     C   THEN
             HCHAR(I:) = HCHAR(I+1:)
             GO TO 140
         ENDIF
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
C*    Section 2 . Remove any spaces after separators.
C     ----------------------------------------------------------------
C
      iend = rtb (hchar)
  200 CONTINUE
C
      I = INDEX (HCHAR(1:),'/ ')
C
C*    Go to section 9 if string ends '/ '.
C
c++      IEND = RTB (HCHAR)
      IF (IEND.EQ.I) GO TO 210
C
      IF (I.NE.0)
     C   THEN
             HCHAR(I+1:) = HCHAR(I+2:)
             iend = iend - 1
             GO TO 200
         ENDIF
C
  210 CONTINUE
C
      I = INDEX (HCHAR(1:),', ')
C
C*    Go to section 9 if string ends ', '.
C
      IEND = RTB (HCHAR)
      IF (IEND.EQ.I) GO TO 220
C
      IF (I.NE.0)
     C   THEN
             HCHAR(I+1:) = HCHAR(I+2:)
             GO TO 210
         ENDIF
C
  220 CONTINUE
C
      I = INDEX (HCHAR(1:),'= ')
C
C*    Go to section 9 if string ends '= '.
C
      IEND = RTB (HCHAR)
      IF (IEND.EQ.I) GO TO 230
C
      IF (I.NE.0)
     C   THEN
             HCHAR(I+1:) = HCHAR(I+2:)
             GO TO 220
         ENDIF
C
  230 CONTINUE
C
      I = INDEX (HCHAR(1:),'" ')
C
C*    Go to section 9 if string ends '" '.
C
      IEND = RTB (HCHAR)
      IF (IEND.EQ.I) GO TO 240
C
      IF (I.NE.0)
     C   THEN
             HCHAR(I+1:) = HCHAR(I+2:)
             GO TO 230
         ENDIF
C
  240 CONTINUE
C
      I = INDEX (HCHAR(1:),'. ')
C
C*    Go to section 9 if string ends '. '.
C
      IEND = RTB (HCHAR)
      IF (IEND.EQ.I) GO TO 250
C
      IF (I.NE.0)
     C   THEN
             HCHAR(I+1:) = HCHAR(I+2:)
             GO TO 240
         ENDIF
C
  250 CONTINUE
C
      I = INDEX (HCHAR(1:),'- ')
C
C*    Go to section 9 if string ends '- '.
C
      IEND = RTB (HCHAR)
      IF (IEND.EQ.I) GO TO 260
C
      IF (I.NE.0)
     C   THEN
             HCHAR(I+1:) = HCHAR(I+2:)
             GO TO 250
         ENDIF
C
  260 CONTINUE
C
c--      WRITE (*,9001)
c--      WRITE (*,9002) HCHAR
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
 9000 FORMAT (1H ,'DELSP : Input string :-')
C
 9001 FORMAT (1H ,'DELSP : Output string :-')
C
 9002 FORMAT (1H ,A)
C
      END
