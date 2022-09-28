C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE DECFP2 (PVAL,KEXP,KMANT)
C
C---->
C**** DECFP2 - GRIB representation to floating point representation.
C
C     Purpose.
C     --------
C
C     Convert GRIB representation of a floating point
C     number to machine representation.
C
C**   Interface.
C     ----------
C
C     CALL DECFP2 (PVAL,KEXP,KMANT)
C
C
C     Input Parameters.
C     -----------------
C
C     KEXP    - 8 Bit signed exponent.
C
C     KMANT   - 24 Bit mantissa.
C
C     Output Parameters.
C     -----------------
C
C     PVAL    - Floating point number represented
C                         by KEXP and KMANT.
C
C
C     Method.
C     -------
C
C     Floating point number represented as 8 bit exponent
C     and 24 bit mantissa in integer values converted to
C     machine floating point format.
C
C
C     Externals.
C     ----------
C
C     None.
C
C
C     Reference.
C     ----------
C
C     WMO Manual on Codes re GRIB representation.
C
C
C     Comments.
C     ---------
C
C     Rewritten from DECFP, to conform to programming standards.
C     Sign bit on 0 value now ignored, if present.
C     If using 32 bit reals, check power of 16 is not so small as to
C     cause overflows (underflows!); this causes warning to be given
C     on Fujitsus.
C
C
C     Author.
C     -------
C
C     John Hennessy   ECMWF   18.06.91
C
C
C     Modifications.
C     --------------
C
C     J.D.Chambers    ECMWF   1 Aug 1996
C     Add check power that power of 16 is not so small as to cause
C     overflows.
C
C
C----<
C     -----------------------------------------------------------------
C*    Section 0 . Definition of variables.
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
      REAL    PVAL
      INTEGER KEXP, KMANT
C
C     Local variables.
C
      INTEGER IEXP, ISIGN
      LOGICAL LDEBUG
C
C     -----------------------------------------------------------------
C*    Section 1 . Convert value of 0.0. Ignore sign bit.
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
      LDEBUG = (NDBG.GT.1)
C
      IF( LDEBUG )
     X  WRITE(GRPRSM,*) 'DECFP2: KEXP = ',KEXP,'. KMANT = ',KMANT
C
Cjdc  IF ( (KEXP.EQ.128.OR.KEXP.EQ.0).AND.KMANT.EQ.0) THEN
      IF( ((KEXP.EQ.128).OR.
     X     (KEXP.EQ.0)  .OR.
     X     (KEXP.EQ.255)    )) THEN
        PVAL = 0.0
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 2 . Convert other values.
C     -----------------------------------------------------------------
C
  200 CONTINUE
C
C     Sign of value.
C
      IEXP  = KEXP
      ISIGN = 1
C
C     Set sign of exponent
C
      IF (IEXP.GE.128) THEN
        IEXP  = IEXP - 128
        ISIGN = -1
      END IF
C
C     Decode value.
C
#if (defined REAL_8) || (defined CRAY)
      PVAL = ISIGN*2.**(-24)*KMANT*16.**(IEXP-64)
C                  ---------
C                      `---> this matchs/compensates for (IEXP-70)
C                            in confp3.F (instead of (IEXP-64))
#else
C     If using 32 bit reals, check power of 16 is not so small as to
C     cause overflows (underflows!); this causes warning to be given
C     on Fujitsus.
C
      IF(IEXP.GT.32) THEN
        PVAL = ISIGN*2.**(-24)*KMANT*16.**(IEXP-64)
      ELSE
        PVAL = 0.0
      ENDIF
#endif
      IF( LDEBUG ) THEN
        WRITE(GRPRSM,*) 'DECFP2: Values used for final calculation'
        WRITE(GRPRSM,*)
     X    'DECFP2: IEXP=',IEXP,'.ISIGN=',ISIGN,'.KMANT=',KMANT
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      IF( LDEBUG ) WRITE(GRPRSM,*) 'DECFP2: Returned value = ', PVAL
C
      RETURN
      END
