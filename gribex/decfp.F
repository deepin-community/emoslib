C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE DECFP (FVAL,IEXP,IMANT)
C---->
C
C*
C*    NAME      : DECFP
C*
C*    FUNCTION  : DECODE GRIB CODE REPRESENTATION TO
C*                FLOATING POINT NUMBER.
C*
C*    INPUT     : IEXP  - 8 BIT SIGNED EXPONENT
C*                IMANT - 24 BIT MANTISSA
C*
C*    OUTPUT    : FVAL  - FLOATING POINT NUMBER
C*                IEXP  - UNCHANGED
C*                IMANT - UNCHANGED
C*
C*    JOHN HENNESSY , ECMWF , 15TH APRIL 1985
C*
C     ---------------------------------------------------------------
C----<
C
C
      IF (IEXP.EQ.128.AND.IMANT.EQ.0)
     C   THEN
             FVAL = 0.0
             RETURN
         ENDIF
C
C
C
C     SIGN OF VALUE
C
      JEXP  = IEXP
      ISIGN = 1
C
      IF (JEXP.GE.128)
     C     THEN
               JEXP  = JEXP - 128
               ISIGN = -1
           END IF
C
C     DECODE VALUE.
C
      FVAL = ISIGN*2.**(-24)*IMANT*16.**(JEXP-64)
C
C
      RETURN
      END
