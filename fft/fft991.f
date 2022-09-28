c Copyright 1981-2016 ECMWF.
c
c This software is licensed under the terms of the Apache Licence 
c Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
c
c In applying this licence, ECMWF does not waive the privileges and immunities 
c granted to it by virtue of its status as an intergovernmental organisation 
c nor does it submit to any jurisdiction.
c

      SUBROUTINE FFT991(A,WORK,TRIGS,IFAX,INC,JUMP,N,ILOT,ISIGN)
      REAL A(*),WORK(*),
     *  TRIGS(*)
      INTEGER IFAX(*)
C
C     SUBROUTINE 'FFT991' - MULTIPLE FAST REAL PERIODIC TRANSFORM
C     SUPERSEDES PREVIOUS ROUTINE 'FFT991'
C
C     REAL TRANSFORM OF LENGTH N PERFORMED BY REMOVING REDUNDANT
C     OPERATIONS FROM COMPLEX TRANSFORM OF LENGTH N
C
C     A IS THE ARRAY CONTAINING INPUT & OUTPUT DATA
C     WORK IS AN AREA OF SIZE (N+1)*MIN(ILOT,JP_LOT)
C     TRIGS IS A PREVIOUSLY PREPARED LIST OF TRIG FUNCTION VALUES
C     IFAX IS A PREVIOUSLY PREPARED LIST OF FACTORS OF N
C     INC IS THE INCREMENT WITHIN EACH DATA 'VECTOR'
C         (E.G. INC=1 FOR CONSECUTIVELY STORED DATA)
C     JUMP IS THE INCREMENT BETWEEN THE START OF EACH DATA VECTOR
C     N IS THE LENGTH OF THE DATA VECTORS
C     ILOT IS THE NUMBER OF DATA VECTORS
C     ISIGN = +1 FOR TRANSFORM FROM SPECTRAL TO GRIDPOINT
C           = -1 FOR TRANSFORM FROM GRIDPOINT TO SPECTRAL
C
C     ORDERING OF COEFFICIENTS:
C         A(0),B(0),A(1),B(1),A(2),B(2),...,A(N/2),B(N/2)
C         WHERE B(0)=B(N/2)=0; (N+2) LOCATIONS REQUIRED
C
C     ORDERING OF DATA:
C         X(0),X(1),X(2),...,X(N-1), 0 , 0 ; (N+2) LOCATIONS REQUIRED
C
C     VECTORIZATION IS ACHIEVED ON CRAY BY DOING THE TRANSFORMS
C     IN PARALLEL
C
C     N MUST BE COMPOSED OF FACTORS 2,3 & 5 BUT DOES NOT HAVE TO BE EVEN
C
C     DEFINITION OF TRANSFORMS:
C     -------------------------
C
C     ISIGN=+1: X(J)=SUM(K=0,...,N-1)(C(K)*EXP(2*I*J*K*PI/N))
C         WHERE C(K)=A(K)+I*B(K) AND C(N-K)=A(K)-I*B(K)
C
C     ISIGN=-1: A(K)=(1/N)*SUM(J=0,...,N-1)(X(J)*COS(2*J*K*PI/N))
C               B(K)=-(1/N)*SUM(J=0,...,N-1)(X(J)*SIN(2*J*K*PI/N))
C
      IF(IFAX(10).NE.N) CALL SET99(TRIGS,IFAX,N)
      NFAX=IFAX(1)
      NX=N+1
      IF (MOD(N,2).EQ.1) NX=N
cfse  NBLOX=1+(ILOT-1)/64
cfse  NVEX=ILOT-(NBLOX-1)*64
      NBLOX=1+(ILOT-1)/512
      NVEX=ILOT-(NBLOX-1)*512
      IF (ISIGN.EQ.-1) GO TO 300
C
C     ISIGN=+1, SPECTRAL TO GRIDPOINT TRANSFORM
C     -----------------------------------------
  100 CONTINUE
      ISTART=1
      DO 220 NB=1,NBLOX
      IA=ISTART
      I=ISTART
*vocl loop,novrec
      DO 110 J=1,NVEX
      A(I+INC)=0.5*A(I)
      I=I+JUMP
  110 CONTINUE
      IF (MOD(N,2).EQ.1) GO TO 130
      I=ISTART+N*INC
*vocl loop,novrec
      DO 120 J=1,NVEX
      A(I)=0.5*A(I)
      I=I+JUMP
  120 CONTINUE
  130 CONTINUE
      IA=ISTART+INC
      ILA=1
      IGO=+1
C
      DO 160 K=1,NFAX
      IFAC=IFAX(K+1)
      IERR=-1
      IF (IGO.EQ.-1) GO TO 140
      CALL RPASSM(A(IA),A(IA+ILA*INC),WORK(1),WORK(IFAC*ILA+1),TRIGS,
     *    INC,1,JUMP,NX,NVEX,N,IFAC,ILA,IERR)
      GO TO 150
  140 CONTINUE
      CALL RPASSM(WORK(1),WORK(ILA+1),A(IA),A(IA+IFAC*ILA*INC),TRIGS,
     *    1,INC,NX,JUMP,NVEX,N,IFAC,ILA,IERR)
  150 CONTINUE
      IF (IERR.NE.0) GO TO 500
      ILA=IFAC*ILA
      IGO=-IGO
      IA=ISTART
  160 CONTINUE
C
C     IF NECESSARY, COPY RESULTS BACK TO A
C     ------------------------------------
      IF (MOD(NFAX,2).EQ.0) GO TO 190
      IBASE=1
      JBASE=IA
      DO 180 JJ=1,NVEX
      I=IBASE
      J=JBASE
      DO 170 II=1,N
      A(J)=WORK(I)
      I=I+1
      J=J+INC
  170 CONTINUE
      IBASE=IBASE+NX
      JBASE=JBASE+JUMP
  180 CONTINUE
  190 CONTINUE
C
C     FILL IN ZEROS AT END
C     --------------------
      IX=ISTART+N*INC
*vocl loop,novrec
      DO 210 J=1,NVEX
      A(IX)=0.0
      A(IX+INC)=0.0
      IX=IX+JUMP
  210 CONTINUE
C
      ISTART=ISTART+NVEX*JUMP
cfse  NVEX=64
      NVEX=512
  220 CONTINUE
      RETURN
C
C     ISIGN=-1, GRIDPOINT TO SPECTRAL TRANSFORM
C     -----------------------------------------
  300 CONTINUE
      ISTART=1
      DO 410 NB=1,NBLOX
      IA=ISTART
      ILA=N
      IGO=+1
C
      DO 340 K=1,NFAX
      IFAC=IFAX(NFAX+2-K)
      ILA=ILA/IFAC
      IERR=-1
      IF (IGO.EQ.-1) GO TO 320
      CALL QPASSM(A(IA),A(IA+IFAC*ILA*INC),WORK(1),WORK(ILA+1),TRIGS,
     *    INC,1,JUMP,NX,NVEX,N,IFAC,ILA,IERR)
      GO TO 330
  320 CONTINUE
      CALL QPASSM(WORK(1),WORK(IFAC*ILA+1),A(IA),A(IA+ILA*INC),TRIGS,
     *    1,INC,NX,JUMP,NVEX,N,IFAC,ILA,IERR)
  330 CONTINUE
      IF (IERR.NE.0) GO TO 500
      IGO=-IGO
      IA=ISTART+INC
  340 CONTINUE
C
C     IF NECESSARY, COPY RESULTS BACK TO A
C     ------------------------------------
      IF (MOD(NFAX,2).EQ.0) GO TO 370
      IBASE=1
      JBASE=IA
      DO 360 JJ=1,NVEX
      I=IBASE
      J=JBASE
      DO 350 II=1,N
      A(J)=WORK(I)
      I=I+1
      J=J+INC
  350 CONTINUE
      IBASE=IBASE+NX
      JBASE=JBASE+JUMP
  360 CONTINUE
  370 CONTINUE
C
C     SHIFT A(0) & FILL IN ZERO IMAG PARTS
C     ------------------------------------
      IX=ISTART
      DO 380 J=1,NVEX
      A(IX)=A(IX+INC)
      A(IX+INC)=0.0
      IX=IX+JUMP
  380 CONTINUE
      IF (MOD(N,2).EQ.1) GO TO 400
      IZ=ISTART+(N+1)*INC
      DO 390 J=1,NVEX
      A(IZ)=0.0
      IZ=IZ+JUMP
  390 CONTINUE
  400 CONTINUE
C
      ISTART=ISTART+NVEX*JUMP
cfse  NVEX=64
      NVEX=512
  410 CONTINUE
      RETURN
C
C     ERROR MESSAGES
C     --------------
  500 CONTINUE
      IF(IERR.EQ.1) THEN
  520 FORMAT('VECTOR LENGTH =',I4,', GREATER THAN 64')
      ELSEIF(IERR.EQ.2) THEN
      WRITE(6,540) IFAC
  540 FORMAT( 'FACTOR =',I3,', NOT CATERED FOR')
      ELSE
      WRITE(6,560) IFAC
  560 FORMAT('FACTOR =',I3,', ONLY CATERED FOR IF ILA*IFAC=N')
      ENDIF
      END
