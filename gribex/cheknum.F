C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE CHEKNUM (VALU,LVAL,NUM)
C
C---->
C*
C*    NAME      : CHEKNUM
C*
C*    FUNCTION  : CHECKS THAT A CHARACTER STRING CORRECTLY
C*                REPRESENTS A NUMBER.
C*
C*    INPUT     : VALU = CHARACTER STRING TO BE CHECKED
C*                LVAL = NUMBER OF CHARACTERS IN THIS STRING
C*
C*    OUTPUT    : NUM  = 0, IF STRING REPRESENTS A NUMBER.
C*                       1, IF INVALID CHARACTER FOUND.
C*
C*       JOHN HENNESSY, ECMWF, 16TH APRIL 1985
C
C----<
C     ---------------------------------------------------------------
C
      IMPLICIT INTEGER (A-Z)
C
      CHARACTER*(*) VALU
      NUM = 0
C
C     CHECK ONE CHARACTER STRING FOR VALID DIGIT.
C
      IF (VALU(1:1).LT.'0'.OR.VALU(1:1).GT.'9') NUM=1
      IF (LVAL.EQ.1)  RETURN
C
C     CHECK FOR SIGN OR DECIMAL POINT AS FIRST CHARACTER IN STRING.
C
      IF (VALU(1:1).EQ.'+') NUM = 0
      IF (VALU(1:1).EQ.'-') NUM = 0
      IF (VALU(1:1).EQ.'.') NUM = 0
      IF (NUM.EQ.1) RETURN
C
      TOTAL = 1
      POINT = 0
      IF (VALU(1:1).EQ.'.') POINT = 1
C
      DO 100 I=2,LVAL
         IF (VALU(I:I).EQ.'.') POINT = POINT + 1
         IF (VALU(I:I).GE.'0'.AND.VALU(I:I).LE.'9')
     C      TOTAL = TOTAL + 1
  100 CONTINUE
C
      TOTAL = TOTAL + POINT
      IF (VALU(1:1).EQ.'.') TOTAL = TOTAL - 1
      IF (TOTAL.NE.LVAL) NUM = 1
      IF (POINT.GT.1) NUM = 1
C
      RETURN
      END
