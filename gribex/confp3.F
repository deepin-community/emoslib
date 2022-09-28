C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CONFP3(PVAL,KEXP,KMANT,KBITS,KROUND)
C
C---->
C**** CONFP3 - Convert floating point number to GRIB representation.
C
C     Purpose.
C     --------
C
C     Convert floating point number from machine
C     representation to GRIB representation.
C
C**   Interface.
C     ----------
C
C     CALL CONFP3(PVAL,KEXP,KMANT,KBITS,KROUND)
C
C
C     Input Parameters.
C     -----------------
C
C     PVAL    - Floating point number to be converted.
C     KBITS   - Number of bits in computer word.
C     KROUND  - Conversion type.
C               0 , Closest number in GRIB format less than
C                   original number.
C               1 , Closest number in GRIB format to the
C                   original number (equal to, greater than or
C                   less than original number).
C               10 , as for 0 but with debug printout.
C               11 , as for 1 but with debug printout.
C
C     Output Parameters.
C     -----------------
C
C     KEXP    - 8 Bit signed exponent.
C     KMANT   - 24 Bit mantissa.
C
C     Method.
C     -------
C
C     Floating point number represented as 8 bit signed
C     exponent and 24 bit mantissa in integer values.
C
C     Externals.
C     ----------
C
C     DECFP2
C     ABORTX
C
C     Reference.
C     ----------
C
C     WMO Manual on Codes re GRIB representation.
C
C     Comments.
C     ---------
C
C     Routine aborts if an invalid conversion type parameter
C     is used or if a 24 bit mantissa is not produced.
C
C
C     Author.
C     -------
C
C     John Hennessy   ECMWF   18.06.91
C
C     Modifications.
C     --------------
C
C     John Hennessy   ECMWF   24.09.91
C     Corrections made to descriptions of input parameter KROUND.
C     Changes to comments and format statements.
C
C     J.D.Chambers    ECMWF    13.09.94
C     Inhibit abort if NOABORT = 1, use grbcom.h
C
C
C     J.D.Chambers    ECMWF    21.11.94
C     Use JNINT for VAX
C
C----<
C     -----------------------------------------------------------------
C*    Section 0 . Definition of variables. Set initial values.
C     -----------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
#include "grbcom.h"
C
C     Subroutine arguments
C
      REAL PVAL
      INTEGER KEXP,KMANT,KBITS,KROUND
C
C     Local variables
C
      INTEGER IEXP, IPR, IROUND, ISIGN
      REAL ZEPS, ZREF, ZVAL
C
C     -----------------------------------------------------------------
C*    Section 1 . Initialise
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
C     Debug print switch.
C
      IF( KROUND.GE.10) THEN
        IPR    = 1
        IROUND = KROUND - 10
      ELSE
        IPR    = 0
        IROUND = KROUND
      ENDIF
C
C     Check conversion type parameter.
C
      IF( IROUND.NE.0.AND.IROUND.NE.1) THEN
        WRITE(GRPRSM,*) 'CONFP3: Invalid conversion type = ', KROUND
C
        IF( NOABORT.EQ.0 ) CALL ABORTX('CONFP3')
C
C       If not aborting, arbitrarily set rounding to 'up'.
C
        IROUND = 1
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 2 . Convert value of zero.
C     -----------------------------------------------------------------
C
  200 CONTINUE
C
      IF( PVAL.EQ.0.0 ) THEN
        KEXP  = 0
        KMANT = 0
        IEXP  = 0
        ISIGN = 0
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 3 . Convert other values.
C     -----------------------------------------------------------------
C
  300 CONTINUE
C
      ZEPS = 1.0E-12
      IF( KBITS.EQ.32) ZEPS = 1.0E-8
      ZREF = PVAL
C
C     Sign of value.
C
      ISIGN = 0
      IF( ZREF.LT.0.) THEN
        ISIGN = 128
        ZREF  = - ZREF
      ENDIF
C
C     Exponent.
C
#ifdef CRAY
      IEXP = INT(ALOG(ZREF)*(1.0/ALOG(16.0))+64.0+1.0+ZEPS)
#else
      IEXP = INT(LOG(ZREF)*(1.0/LOG(16.0))+64.0+1.0+ZEPS)
#endif
C
      IF( IEXP.LT.0  ) IEXP = 0
      IF( IEXP.GT.127) IEXP = 127
C
C     Mantissa.
C
      IF( IROUND.EQ.0 ) THEN
C
C       Closest number in GRIB format less than original number.
C       Truncate for positive numbers.
C       Round up for negative numbers.
C
        IF( ISIGN.EQ.0 ) THEN
          KMANT = INT(ZREF/16.0**(IEXP-70))
        ELSE
#ifdef VAX
          KMANT = JNINT(ZREF/16.0**(IEXP-70)+0.5)
#else
          KMANT = NINT(ZREF/16.0**(IEXP-70)+0.5)
#endif
        ENDIF
C
      ELSE
C
C       Closest number in GRIB format to the original number
C       (equal to, greater than or less than original number).
C
#ifdef VAX
        KMANT = JNINT(ZREF/16.0**(IEXP-70))
#else
        KMANT = NINT(ZREF/16.0**(IEXP-70))
#endif
      ENDIF
C
C     Check that mantissa value does not exceed 24 bits.
C     If it does, adjust the exponent upwards and recalculate
C     the mantissa.
C     (Nb. 16777215 = 2**24 - 1)
C
      IF( KMANT.GT.16777215) THEN
C
  350   CONTINUE
C
        IEXP = IEXP + 1
C
C       Check for exponent overflow during adjustment
C
        IF( IEXP.GT.127 ) THEN
          WRITE(GRPRSM,*) 'CONFP3: Exponent overflow'
          WRITE(GRPRSM,9002) PVAL
          WRITE(GRPRSM,9003) ISIGN, IEXP, KMANT
C
          IF( NOABORT.EQ.0 ) CALL ABORTX('CONFP3')
C
C         If not aborting, arbitrarily set value to zero
C
          WRITE(GRPRSM,*) 'CONFP3: Value arbitrarily set to zero.'
          KEXP  = 0
          KMANT = 0
          IEXP  = 0
          ISIGN = 0
          GOTO 900
C
        ENDIF
C
        IF( IROUND.EQ.0) THEN
C
C         Closest number in GRIB format less than original number.
C         Truncate for positive numbers.
C         Round up for negative numbers.
C
          IF( ISIGN.EQ.0 ) THEN
            KMANT = INT(ZREF/16.0**(IEXP-70))
          ELSE
#ifdef VAX
            KMANT = JNINT(ZREF/16.0**(IEXP-70)+0.5)
#else
            KMANT = NINT(ZREF/16.0**(IEXP-70)+0.5)
#endif
          ENDIF
C
        ELSE
C
C         Closest number in GRIB format to the original number
C         (equal to, greater or less than original number).
C
#ifdef VAX
          KMANT = JNINT(ZREF/16.0**(IEXP-70))
#else
          KMANT = NINT(ZREF/16.0**(IEXP-70))
#endif
        ENDIF
C
C       Repeat calculation (with modified exponent) if still have
C       mantissa overflow.
C
        IF( KMANT.GT.16777215) GOTO 350
C
      ENDIF
C
C     Add sign bit to exponent.
C
      KEXP = IEXP + ISIGN
C
C     -----------------------------------------------------------------
C*    Section 9. Return
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
C     Print generated values if required.
C
      IF( IPR.EQ.1 ) THEN
        WRITE(GRPRSM,9006) KROUND
        WRITE(GRPRSM,9002) PVAL
        CALL DECFP2(ZVAL,KEXP,KMANT)
        WRITE(GRPRSM,9007) ZVAL
        WRITE(GRPRSM,9003) ISIGN, IEXP, KMANT
      ENDIF
C
      RETURN
C
C
 9002 FORMAT(1H ,'CONFP3: Original number = ',F30.20)
 9003 FORMAT(1H ,'CONFP3: Sign = ',I3,', Exponent = ',I3,
     X            ', Mantissa = ',I12)
 9006 FORMAT(1H ,'CONFP3: Conversion type parameter = ',I4)
 9007 FORMAT(1H ,'CONFP3: Converted to      ',F30.20)
C
      END
