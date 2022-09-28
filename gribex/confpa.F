C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CONFPA(FVAL,KLENFP,KOFF,KPACKED,KWORD,KLENPS,KBIT)
C
C---->
C********************************************************************
C*
C*    NAME      : CONFPA
C*
C*    FUNCTION  : CONVERT FLOATING POINT NUMBER FROM MACHINE
C*                REPRESENTATION TO GRIB REPRESENTATION.
C*
C*    INPUT     : FVAL  - FLOATING POINT NUMBERS TO BE CONVERTED.
C*                KLENFP  : LENGTH OF FVAL
C*              : KOFF  - OFFSET IN BITS TO FIRST PACKED FIELD IN KPACKED.
C*                KBIT    : NUMBER OF BITS IN OUTPUT WORD
C*                KLENPS  : LENGTH OF KPACKED
C*
C*    OUTPUT    : KPACKED : PACKED OUTPUT FIELD
C*
C*    JOHN HENNESSY , ECMWF , 15TH APRIL 1985
C*    DAVID DENT    , ECMWF , 18TH NOVEMBER 1997
C*
C*    LIMITATION of this VECTOR version:
C*    assumes KOFF=0 and INTEGER word length is 32
C*
C********************************************************************
C----<
C
      REAL FVAL(KLENFP)
      INTEGER KPACKED(KLENPS)
C
      EPS = 1.0E-12
      IOUT=1
C
C        convert array of real numbers
C
      DO 210 JL=1,KLENFP
        IF (FVAL(JL).EQ.0.0) THEN
          JEXP  = 128
          JMANT = 0
        ELSE
C
          REF = FVAL(JL)
C
C     SIGN OF VALUE
C
          ISIGN = 0
          IF (REF.LE.0.) THEN
            ISIGN = 128
            REF = - REF
          ENDIF
C
C     EXPONENT
C
          IF (REF.EQ.0.0) THEN
            JEXP = 0
          ELSE
            JEXP = INT(ALOG(REF)*(1.0/ALOG(16.0))+64.0+1.0+EPS)
          ENDIF
C
          IF (JEXP.LT.0  ) JEXP = 0
          IF (JEXP.GT.127) JEXP = 127
C
C     MANTISSA
C
          JMANT = NINT (REF/16.0**(JEXP-70))
C
C     ADD SIGN BIT TO EXPONENT.
C
          JEXP = JEXP + ISIGN
C
        ENDIF
C
        KPACKED(IOUT)=IOR(ISHFT(JEXP,24),JMANT)

#ifdef LITTLE_ENDIAN
        CALL SWAP4(KPACKED(IOUT))
#endif 
        
        IOUT=IOUT+1
  210 CONTINUE
C
      KWORD=KWORD+(KLENFP*32/KBIT)
C
      RETURN
C
      END
