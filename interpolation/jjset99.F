C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JJSET99(TRIGS, IFAX, N, KRET)
C
C---->
C**** JJSET99
C
C     PURPOSE
C     _______
C
C     Computes factors of N & sines and cosines required by FFT99 
C     and FFT991.
C
C     INTERFACE
C     _________
C
C     CALL JJSET99(TRIGS, IFAX, N, KRET))
C
C     Input parameters
C     ________________
C
C     N      - Number to factorise.
C
C     Output parameters
C     ________________
C
C     TRIGS  - Sines and cosines for angles 0 to pi in N steps
C              (sin,cos,sin,cos,...)
C     IFAX   - Allowed factors of N  from 8,6,5,4,3,2.
C              (Only one 8 allowed).
C              IFAX(1)  = count of factors.
C              IFAX(2-9)= factor (or zero).
C              IFAX(10) = N.
C     KRET   - 0 if factorised OK.
C
C     Method
C     ______
C
C     Look for sixes first, store factors in order: 8,6,5,4,3,2;
C     then reverses the order for output.
C
C
C     Externals
C     _________
C
C     INTLOG - Log error messages.
C
C
C     Reference
C     _________
C
C     None.
C
C
C     Comments
C     ________
C
C     This is a tidy up, based on set99.F
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers      *ECMWF*      Nov 1996
C
C
C     MODIFICATIONS
C     _____________
C
C     None.
C
C----<
C     _______________________________________________________
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
C
      IMPLICIT NONE
#include "jparams.h"
#include "parim.h"
C
C     Parameters
      INTEGER JPROUTINE
      PARAMETER ( JPROUTINE = 31500 )
C
C     Subroutine arguments
      REAL TRIGS
      INTEGER IFAX, N, KRET
      DIMENSION TRIGS(N), IFAX(10)
C
C     Local variables
      INTEGER JFAX, LFAX, LOOP, NIL, NHL, NU, I, K, L, IFAC, NFAX
      DIMENSION LFAX(7), JFAX(10)
      DATA LFAX/6,8,5,4,3,2,1/
      REAL DEL, ANGLE
C
C     _______________________________________________________
C
C     Section 1.    Initialization.
C     _______________________________________________________
C
  100 CONTINUE
C
      DO LOOP = 1, 10
        JFAX(LOOP) = 0
      ENDDO
C
      KRET = 0
C
C     Generate sines and cosines for angles 0 to pi in N steps
C
      DEL = (2.0*PPI) / FLOAT(N)
      NIL = 0
      NHL = (N/2)-1
C
      DO K = NIL,NHL
        ANGLE        = FLOAT(K)*DEL
        TRIGS(2*K+1) = COS(ANGLE)
        TRIGS(2*K+2) = SIN(ANGLE)
      ENDDO
C
C     _______________________________________________________
C
C     Section 2.    Find allowed factors of N
C                   (8,6,5,4,3,2; only one 8 allowed)
C     _______________________________________________________
C
  200 CONTINUE
C
C     Look for sixes first, store factors in order: 8,6,5,4,3,2
C
      NU   = N
      IFAC = 6
      K    = 0
      L    = 1
C
C     Loop through potential factors.
C
  220 CONTINUE
C
      IF (MOD(NU,IFAC).EQ.0) THEN
C
C       Allowed factor found
        K       = K+1
C
        IF( K.GT.8) THEN
          CALL INTLOG(JP_ERROR,'Too many factors found factorising ',N)
          KRET = JPROUTINE + 1
          GOTO 910
        ENDIF
C
        JFAX(K) = IFAC
C
C       If factor is 8 ..
        IF (IFAC.EQ.8) THEN
C
C         Swap 8 into first array slot instead of 6 if 6 already found
          IF (K.NE.1) THEN
            JFAX(1) = 8
            JFAX(K) = 6
          ENDIF
        ENDIF
C
C       Factor found
        NU = NU/IFAC
C
C       Exit if all factors of N have been found
        IF (NU.EQ.1) GOTO 900
C
C       Only one 8 allowed as a factor
        IF (IFAC.NE.8) GOTO 220
      ENDIF
C
C     Pick up next allowed factor.
      L    = L+1
      IFAC = LFAX(L)
      IF (IFAC.GT.1) GOTO 220
C
C     Problem! All allowed factors tried but some factors still left.
C
      CALL INTLOG(JP_ERROR,'Illegal factors found factorising ',N)
      KRET = JPROUTINE + 2
      GOTO 910
C     _______________________________________________________
C
C     Section 9.    All factors found.
C     _______________________________________________________
C
  900 CONTINUE
C
C     Store the factors in the reverse order in the output array.
C
      NFAX    = K
      IFAX(1) = NFAX
      DO I = 1,NFAX
        IFAX(NFAX+2-I) = JFAX(I)
      ENDDO
C
      IFAX(10) = N
C
  910 CONTINUE
C
      RETURN
      END
