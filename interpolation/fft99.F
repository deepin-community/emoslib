C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE FFT99(A,WORK,TRIGS,IFAX,INC,JUMP,N,LOT,ISIGN)
C
C---->
C**** FFT99
C
C     PURPOSE
C     _______
C
C     Multiple fast real periodic transform.
C
C
C     INTERFACE
C     _________
C
C     CALL FFT99(A,WORK,TRIGS,IFAX,INC,JUMP,N,LOT,ISIGN)
C
C
C     Input parameters
C     ________________
C
C     A      - the array containing input data
C     WORK   - an area of size (N+1)*MIN(LOT,64)
C     TRIGS  - a previously prepared list of trig function values
C     IFAX   - a previously prepared list of factors of N
C     INC    - the increment within each data 'vector'
C              (e.g. INC=1 for consecutively stored data)
C     JUMP   - the increment between the start of each data vector
C     N      - the length of the data vectors
C     LOT    - the number of data vectors
C     ISIGN  -  +1 for transform from spectral to gridpoint
C            -  -1 for transform from gridpoint to spectral
C
C     Output parameters
C     ________________
C
C     A      - the array containing output data
C
C
C     Method
C     ______
C     
C     Ordering of coefficients:
C         A(0),B(0),A(1),B(1),A(2),B(2),...,A(N/2),B(N/2)
C         where B(0) = B(N/2)=0; (N+2) locations required
C
C     Ordering of data:
C         X(N-1),X(0),X(1),X(2),...,X(N-1),X(0)
C         i.e. explicit cyclic continuity; (N+2) locations required
C
C     Vectorization is achieved by doing the transforms in parallel
C
C     N must be composed of factors 2,3 & 5 but does not have to be even
C
C     
C     Real transform of length N performed by removing redundant
C     operations from complex transform of length N
C     Definition of transforms:
C
C     ISIGN = +1:
C         X(J) = SUM(K=0,...,N-1)(C(K)*EXP(2*I*J*K*PI/N))
C         where C(K) = A(K)+I*B(K) and C(N-K) = A(K)-I*B(K)
C
C     ISIGN = -1:
C         A(K) = (1/N)*SUM(J=0,...,N-1)(X(J)*COS(2*J*K*PI/N))
C         B(K) = -(1/N)*SUM(J=0,...,N-1)(X(J)*SIN(2*J*K*PI/N))
C
C     Externals
C     _________
C
C     RPASSM  - Performs one pass through data as part of multiple real
C               FFT (fourier analysis) routine.
C     QPASSM  - Performs one pass through data as part of multiple real
C               FFT (fourier synthesis) routine.
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
C     Tidy up of code in older version of same routine.
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
      IMPLICIT NONE
C
C     Subroutine arguments
      REAL A, WORK, TRIGS
      INTEGER IFAX, INC, JUMP, N, LOT, ISIGN
      DIMENSION A(N),WORK(N),TRIGS(N),IFAX(10)
C
C     Local variables
      INTEGER NFAX, NX, NBLOX, NVEX, ISTART, NB, IA, I, J
      INTEGER LA, IGO, K, IFAC, IERR, IBASE, JBASE, JJ, II
      INTEGER IX, IZ
C
C ------------------------------------------------------------------
C     Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
C
C     Ensure factorization of N has been done.
      IF(IFAX(10).NE.N) CALL SET99(TRIGS,IFAX,N)
C
      NFAX = IFAX(1)
      IF (MOD(N,2).EQ.1) THEN
        NX = N
      ELSE
        NX = N + 1
      ENDIF
C
C     Calculate number of blocks of 64 vectors and number of vectors
C     'left over'. This remainder is transformed first.
C
      NBLOX = 1 + (LOT-1)/64
      NVEX  = LOT-(NBLOX-1)*64
C
      IF (ISIGN.EQ.1) THEN
C
C ------------------------------------------------------------------
C     Section 2.   Spectral to gridpoint transform.
C ------------------------------------------------------------------
C
  200   CONTINUE
C
C       Loop through the blocks of vectors
        ISTART = 1
        DO 270 NB = 1,NBLOX
          IA = ISTART
          I  = ISTART
          DO 210 J = 1,NVEX
            A(I+INC) = 0.5*A(I)
            I        = I + JUMP
  210     CONTINUE
C
          IF (MOD(N,2).NE.1) THEN
            I = ISTART + N*INC
            DO 220 J = 1,NVEX
              A(I) = 0.5*A(I)
              I    = I + JUMP
  220       CONTINUE
          ENDIF
C
          IA  = ISTART + INC
          LA  = 1
          IGO = 1
C
C         Work through the factors
          DO 230 K = 1,NFAX
            IFAC = IFAX(K+1)
            IERR = -1
            IF (IGO.NE.-1) THEN
              CALL RPASSM(A(IA),A(IA+LA*INC),WORK(1),WORK(IFAC*LA+1),
     X          TRIGS,INC,1,JUMP,NX,NVEX,N,IFAC,LA,IERR)
            ELSE
              CALL RPASSM(WORK(1),WORK(LA+1),A(IA),A(IA+IFAC*LA*INC),
     X          TRIGS,1,INC,NX,JUMP,NVEX,N,IFAC,LA,IERR)
            ENDIF
C
            IF (IERR.NE.0) GO TO 950
C
            LA  = IFAC*LA
            IGO = -IGO
            IA  = ISTART + INC
  230     CONTINUE
C
C         If necessary, copy results back to A
C         
          IF (MOD(NFAX,2).NE.0) THEN
            IBASE = 1
            JBASE = IA
            DO 250 JJ = 1,NVEX
              I = IBASE
              J = JBASE
              DO 240 II = 1,N
                A(J) = WORK(I)
                I    = I + 1
                J    = J + INC
  240         CONTINUE
              IBASE = IBASE + NX
              JBASE = JBASE + JUMP
  250       CONTINUE
          ENDIF
C
C         Fill in cyclic boundary values (ie repeat the data vector
C         end points at opposite end of the vector)
C         
          IX = ISTART
          IZ = ISTART + N*INC
CDIR$ IVDEP
          DO 260 J = 1,NVEX
            A(IX)     = A(IZ)
            A(IZ+INC) = A(IX+INC)
            IX        = IX + JUMP
            IZ        = IZ + JUMP
  260     CONTINUE
C
C         Adjust pointers for next block
          ISTART = ISTART + NVEX*JUMP
          NVEX   = 64
  270   CONTINUE
C
      ELSE
C
C ------------------------------------------------------------------
C     Section 3.   Gridpoint to spectral transform.
C ------------------------------------------------------------------
C
  300   CONTINUE
C
C       Loop through the blocks of vectors
        ISTART = 1
        DO 390 NB  =  1,NBLOX
          IA  = ISTART + INC
          LA  = N
          IGO = 1
C
          DO 310 K = 1,NFAX
            IFAC = IFAX(NFAX+2-K)
            LA   = LA/IFAC
            IERR = -1
            IF (IGO.NE.-1) THEN
              CALL QPASSM(A(IA),A(IA+IFAC*LA*INC),WORK(1),WORK(LA+1),
     X          TRIGS,INC,1,JUMP,NX,NVEX,N,IFAC,LA,IERR)
            ELSE
              CALL QPASSM(WORK(1),WORK(IFAC*LA+1),A(IA),A(IA+LA*INC),
     X          TRIGS,1,INC,NX,JUMP,NVEX,N,IFAC,LA,IERR)
            ENDIF
            IF (IERR.NE.0) GO TO 950
            IGO = -IGO
            IA  = ISTART + INC
  310     CONTINUE
C
C         If necessary, copy results back to A
C         
          IF (MOD(NFAX,2).NE.0) THEN
            IBASE = 1
            JBASE = IA
            DO 330 JJ = 1,NVEX
              I = IBASE
              J = JBASE
              DO 320 II = 1,N
                A(J) = WORK(I)
                I    = I + 1
                J    = J + INC
  320         CONTINUE
              IBASE = IBASE + NX
              JBASE = JBASE + JUMP
  330       CONTINUE
          ENDIF
C
C         Shift A(0) and fill in zero imaginary parts
C         
          IX = ISTART
          DO 340 J = 1,NVEX
            A(IX)     = A(IX+INC)
            A(IX+INC) = 0.0
            IX        = IX + JUMP
  340     CONTINUE
C
          IF (MOD(N,2).NE.1) THEN
            IZ = ISTART + (N+1)*INC
            DO 350 J = 1,NVEX
              A(IZ) = 0.0
              IZ    = IZ + JUMP
  350       CONTINUE
C
          ENDIF
C
C         Adjust pointers for next block
          ISTART = ISTART + NVEX*JUMP
          NVEX   = 64
  390   CONTINUE
C
      ENDIF
C
C ------------------------------------------------------------------
C     Section 9.   Closedown.
C ------------------------------------------------------------------
C
  900   CONTINUE
C
      RETURN
C
C     Error messages
C     
  950 CONTINUE
      GO TO (960,970,980) IERR
C
  960 CONTINUE
      WRITE(*,*) 'Vector length greater than 64, = ', NVEX
      GO TO 900
  970 CONTINUE
      WRITE(*,*) 'Factor not handled, =', IFAC
      GO TO 900
  980 CONTINUE
      WRITE(*,*) 'Factor only handled if LA*IFAC=N. Factor = ', IFAC
C
      END
