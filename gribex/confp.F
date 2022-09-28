C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE CONFP (FVAL,IEXP,IMANT)
C
C
C
C
C
C---->
C********************************************************************
C*
C*    NAME      : CONFP
C*
C*    FUNCTION  : CONVERT FLOATING POINT NUMBER FROM MACHINE
C*                REPRESENTATION TO GRIB REPRESENTATION.
C*
C*    INPUT     : FVAL  - FLOATING POINT NUMBER TO BE CONVERTED.
C*
C*    OUTPUT    : IEXP  - 8 BIT SIGNED EXPONENT
C*                IMANT - 24 BIT MANTISSA
C*                FVAL  - UNCHANGED.
C*
C*    JOHN HENNESSY , ECMWF , 15TH APRIL 1985
C*
C********************************************************************
C----<
C
C
      IF (FVAL.EQ.0.0)
     C   THEN
             IEXP = 128
             IMANT = 0
             RETURN
         ENDIF
C
C
      EPS = 1.0E-12
      REF = FVAL
C
C     SIGN OF VALUE
C
      ISIGN = 0
      IF (REF.LE.0.)
     C   THEN
             ISIGN = 128
             REF = - REF
         ENDIF
C
C
C
C
C
C     EXPONENT
C
      IF (REF.EQ.0.0)
     C   THEN
            IEXP = 0
         ELSE
            IEXP = INT(ALOG(REF)*(1.0/ALOG(16.0))+64.0+1.0+EPS)
         ENDIF
C
      IF (IEXP.LT.0  ) IEXP = 0
      IF (IEXP.GT.127) IEXP = 127
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
C     MANTISSA
C
      IMANT = NINT (REF/16.0**(IEXP-70))
C
C     ADD SIGN BIT TO EXPONENT.
C
      IEXP = IEXP + ISIGN
C
C
C
C
      RETURN
C
      END
