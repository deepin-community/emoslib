C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JSPLEG1( PLEG, PLAT, KTRUNC)
      IMPLICIT NONE
C
C---->
C**** JSPLEG1 - Routine to calculate legendre functions
C
C     Purpose
C     --------
C
C     This routine calculates the legendre functions for one latitude.
C     (but not their derivatives)
C
C
C     Interface
C     ----------
C
C     CALL JSPLEG1( PLEG, PLAT, KTRUNC)
C
C
C     Input parameters
C     ----------------
C
C     PLAT      - Latitude in radians
C     KTRUNC    - Spectral truncation
C
C
C     Output parameters
C     -----------------
C
C     PLEG      - Array of legendre functions for one latitude.
C                 The array must be at least (KTRUNC+1)*(KTRUNC+4)/2 
C                 words long.
C
C
C     Common block usage
C     ------------------
C
C     None
C
C
C     Method
C     ------
C
C     Recurrence relation with explicit relations for P(m,m) and 
C     P(m,m+1)
C
C
C     Externals
C     ---------
C
C     None
C
C
C     Reference
C     ---------
C
C     None
C
C
C     Comments
C     --------
C
C     Rewritten from SPLEG1 to avoid using common blocks.
C     Work arrays ZHLPx currently dimensioned for T213 using 
C     parameter JPTRP1.
C
C
C     AUTHOR
C     ------
C
C     J.D.Chambers         ECMWF        9 November 1993
C
C
C     Modifications
C     -------------
C
C     None
C
C----<
C     _______________________________________________________
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
C*    Prefix conventions for variable names
C
C     Logical      L (but not LP), global or common.
C                  O, dummy argument
C                  G, local variable
C                  LP, parameter.
C     Character    C, global or common.
C                  H, dummy argument
C                  Y (but not YP), local variable
C                  YP, parameter.
C     Integer      M and N, global or common.
C                  K, dummy argument
C                  I, local variable
C                  J (but not JP), loop control
C                  JP, parameter.
C     Real         A to F and Q to X, global or common.
C                  P (but not PP), dummy argument
C                  Z, local variable
C                  PP, parameter.
#include "jparams.h"
C
C     Subroutine arguments
C
      REAL PLEG, PLAT
      DIMENSION PLEG(*)
      INTEGER KTRUNC
C
C     Local variables
C
      REAL ZHLP1, ZHLP2, ZHLP3
      DIMENSION ZHLP1(JPTRP1), ZHLP2(JPTRP1), ZHLP3(JPTRP1)
      INTEGER ITOUT1, I1M, ILM, JM, JCN, IM2
      REAL ZSIN, ZCOS, ZF1M, ZRE1, ZF2M, ZM, ZN, ZE1, ZE2
C
C     _______________________________________________________
C
C
C*    Section 1. Initialization.
C     _______________________________________________________
C
  100 CONTINUE
C
      ITOUT1 = KTRUNC+1
      ZSIN   = SIN(PLAT)
      ZCOS   = SQRT(1.-ZSIN*ZSIN)
C
C     Step 1.        M = 0, N = 0 and N = 1
C
      ILM     = 2
      PLEG(1) = 1.0
      ZF1M    = SQRT(3.0)
      PLEG(2) = ZF1M*ZSIN
C     _______________________________________________________
C
C
C     Step 2.       Sum for M = 0 to T (T = truncation)
C     _______________________________________________________
C
 200  CONTINUE
C
      DO 205 JM = 2,ITOUT1
        ZM        = JM-1
        ZHLP1(JM) = SQRT(2.*ZM+3.)
        ZHLP2(JM) = 1./SQRT(2.*ZM)
 205  CONTINUE
      ZHLP1(1) = SQRT(3.)
C
      DO 570 JM = 1,ITOUT1
C
        I1M  = JM-1
        ZM   = I1M
        ZRE1 = ZHLP1(JM)
        ZE1  = 1./ZRE1
C     _______________________________________________________
C
C
C     Step 3.       M > 0 only
C     _______________________________________________________
C
 300    CONTINUE
C
        IF (I1M.NE.0) THEN
          ZF2M = ZF1M*ZCOS*ZHLP2(JM)
          ZF1M = ZF2M*ZRE1
C     _______________________________________________________
C
C
C     Step 4.       N = M and N = M+1
C     _______________________________________________________
C
 400    CONTINUE
C
          ILM       = ILM+1
          PLEG(ILM) = ZF2M
          ILM       = ILM+1
          PLEG(ILM) = ZF1M*ZSIN
C
C         When output truncation is reached, return to calling program
          IF ( JM .EQ. ITOUT1 ) GOTO 570
C
        ENDIF
C     _______________________________________________________
C
C
C     Step 5.       Sum for N = M+2 to T+1
C     _______________________________________________________
C
 500    CONTINUE
C
        IM2 = I1M+2
C
        DO 520 JCN = IM2,ITOUT1
          ZN         = JCN
          ZHLP3(JCN) = SQRT((4.*ZN*ZN-1.)/(ZN*ZN-ZM*ZM))
 520    CONTINUE
C
        DO 560 JCN = IM2,ITOUT1
C
          ZE2       = ZHLP3(JCN)
          ILM       = ILM+1
          PLEG(ILM) = ZE2*(ZSIN*PLEG(ILM-1)-ZE1*PLEG(ILM-2))
          ZE2       = 1./ZE2
          ZE1       = ZE2
C
 560    CONTINUE
C
 570  CONTINUE
C
C     _______________________________________________________
C
C
C*    Section 9. Return to calling routine.
C     _______________________________________________________
C
  900 CONTINUE
C
      RETURN
      END
