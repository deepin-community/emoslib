C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE CONFP2 (PVAL,KEXP,KMANT,KBITS)
C
C---->
C**** CONFP2 - Convert floating point number to GRIb representation.
C
C     Purpose.
C     --------
C
C           Convert floating point number from machine
C           representation to GRIB representation.
C
C**   Interface.
C     ----------
C
C           CALL CONFP2 (PVAL,KEXP,KMANT,KBITS)
C
C           INTEGER    K.
C           REAL       P.
C           LOGICAL    O.
C           CHARACTER  H.
C
C               Input Parameters.
C               -----------------
C
C               PVAL    - Floating point number to be converted.
C
C               KBITS   - Number of bits in computer word.
C
C               Output Parameters.
C               -----------------
C
C               KEXP    - 8 Bit signed exponent.
C
C               KMANT   - 24 Bit mantissa.
C
C     Method.
C     -------
C
C           Floating point number represented as 8 bit exponent
C           and 24 bit mantissa in integer values.
C
C     Externals.
C     ----------
C
C           None.
C
C     Reference.
C     ----------
C
C           WMO Manual on Codes re GRIB representation.
C
C     Comments.
C     ---------
C
C           Rewritten from CONFP, to include number of bits per
C           computer word, to unite slightly different versions of
C           CONFP.
C
C           Routine contains sections 0 to 2 and section 9.
C
C     Author.
C     -------
C
C           John Hennessy   ECMWF   19.03.91
C
C     Modifications.
C     --------------
C
C           None.
C
C     -----------------------------------------------------------------
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
C     -----------------------------------------------------------------
C
      IMPLICIT LOGICAL     ( L , O , G )
      IMPLICIT CHARACTER*8 ( C , H , Y )
      IMPLICIT INTEGER     ( I , J , K , M , N )
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
C*    Section 1 . Convert value of 0.0.
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
      IF (PVAL.EQ.0.0)
     C   THEN
             KEXP  = 0
             KMANT = 0
             GO TO 900
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
C*    Section 2 . Convert other values.
C     -----------------------------------------------------------------
C
  200 CONTINUE
C
      ZEPS = 1.0E-12
      IF (KBITS.EQ.32) ZEPS = 1.0E-8
      ZREF = PVAL
C
C     Sign of value.
C
      ISIGN = 0
      IF (ZREF.LT.0.)
     C   THEN
             ISIGN = 128
             ZREF  = - ZREF
         ENDIF
C
C     Exponent.
C
      KEXP = INT(ALOG(ZREF)*(1.0/ALOG(16.0))+64.0+1.0+ZEPS)
C
      IF (KEXP.LT.0  ) KEXP = 0
      IF (KEXP.GT.127) KEXP = 127
C
C     Mantissa.
C
      KMANT = INT (ZREF/16.0**(KEXP-70))
C
C     Add sign bit to exponent.
C
      KEXP = KEXP + ISIGN
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
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
C
      END
