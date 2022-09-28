c Copyright 1981-2016 ECMWF.
c
c This software is licensed under the terms of the Apache Licence 
c Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
c
c In applying this licence, ECMWF does not waive the privileges and immunities 
c granted to it by virtue of its status as an intergovernmental organisation 
c nor does it submit to any jurisdiction.
c

      SUBROUTINE SYMINV(A,NDIM,N,COND,V,DEPS)
      DIMENSION A(NDIM,NDIM),V(NDIM)
C     INVERT IN PLACE THE LOWER TRIANGLE OF A (I.E. A(I,J) I.GE.J)
C     A IS A SYMMETRIC POSITIVE DEFINITE MATRIX
C     THE UPPER TRIANGLE OF A (IE. A(I,J) I.LT.J) IS NOT USED OR ALTERED
C
C     THIS VERSION IS OPTIMIZED FOR THE CDC FTN COMPILER.
C     IT REQUIRES THE FUNCTION 'NUMARG' FROM  'ECLIB'
C
      IF(N.LT.1)RETURN
        EPS=0.
c       IF(NUMARG().EQ.6) EPS=DEPS
      NDIA=NDIM+1
      IMX=ISAMAX(N,A(1,1),NDIA)
      ZMX=A(IMX,IMX)*FLOAT(N*N)
C     SECTION 1 & 2 COMBINED IN THIS VERSION
C**** 1. ROOT FREE CHOLESKY DECOMPOSITION  A =L D L(TRANSPOSE)
      J=1
      IF(A(1,1).LE.EPS)GOTO91
      V(1)=1./A(1,1)
      IF(N.EQ.1)GOTO20
      X=A(2,1)*V(1)
      A(2,2)=A(2,2)-X*A(2,1)
      A(2,1)=X
      IF(A(2,2).LE.EPS)GOTO91
      V(2)=1./A(2,2)
      IF(N.EQ.2)GOTO20
      DO 14 I=3,N
      DO 12 J=3,I
      S=A(I,J-1)
      DO 11 K=3,J
 11   S=S-A(I,K-2)*A(J-1,K-2)
      A(I,J-1)=S
 12   CONTINUE
      S=A(I,I)
      DO 13 J=2,I
      X=A(I,J-1)*V(J-1)
      S=S-X*A(I,J-1)
 13   A(I,J-1)=X
      A(I,I)=S
C         CHECK FOR POSITIVE-DEFINITENESS AND INVERT DIAGONAL MATRIX D.
      IF(A(I,I).LE.EPS)GOTO91
      V(I)=1./A(I,I)
 14   CONTINUE
C
C**** 2.  COPY INVERSE OF D WHICH HAS ALREADY BEEN CALCULATED.
 20   DO 21 J=1,N
 21   A(J,J)=V(J)
      IF(N.EQ.1) GO TO 50
C
C**** 3.  INVERSION OF L
 30   A(2,1)=-A(2,1)
      NM1=N-1
      IF(N.EQ.2)GOTO40
      DO 33 I=2,NM1
      DO 32 J=2,I
      S=A(I+1,J-1)
      DO 31 K=J,I
 31   S=S+A(I+1,K)*A(K,J-1)
 32   A(I+1,J-1)=-S
 33   A(I+1,I)=-A(I+1,I)
C
C**** 4.  INV A = INV L(TRANSPOSE) * INV D * INV L
 40   DO 44 J=2,N
      S=A(J-1,J-1)
      DO 41 I=J,N
      X=A(I,I)*A(I,J-1)
      S=S+A(I,J-1)*X
 41   A(I,J-1)=X
      A(J-1,J-1)=S
      IF(J.EQ.N) GO TO 50
      DO 43 I=J,NM1
      S=A(I,J-1)
      DO 42 K=I,NM1
 42   S=S+A(K+1,I)*A(K+1,J-1)
      A(I,J-1)=S
 43   CONTINUE
 44   CONTINUE
C
 50   IMX=ISAMAX(N,A(1,1),NDIA)
      COND=1./ABS(A(IMX,IMX)*ZMX)
      RETURN
C
 91   COND=-FLOAT(J)
      RETURN
      END
      INTEGER FUNCTION ISAMAX(N,A,M)
C
C     FIND THE LARGEST ABSOLUTE ELEMENT OF A , SPACED M WORDS APART
C
      DIMENSION A(*)
C
      LARGE=1
      IF(N.LE.1) GO TO 9
      INDEX=1+M
      DO 1 I=2,N
      IF(ABS(A(INDEX)).GE.ABS(A(LARGE))) LARGE=I
1     INDEX=INDEX+M
9     ISAMAX=LARGE
      RETURN
      END
