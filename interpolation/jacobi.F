C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      LOGICAL FUNCTION JACOBI(DATA,NM,W,SLAT)
C
C---->
C**** JACOBI
C
C     Purpose
C     -------
C
C     Rotates spectral fields by latitude.
C
C
C     Interface
C     ---------
C
C     IRET = JACOBI(DATA,NM,W,SLAT)
C
C     Input
C     -----
C
C     NM      - Triangular truncation number of the field.
C     DATA    - Linear array of REAL*8s of size (NM+1)*(NM+2)
C               holding the field.
C     W       - Work array of REAL*8s of size (NM+1)*(NM+2).
C     SLAT    - Rotation angle (degrees, REAL*8)
C              (degrees, negative => rotate counter-clockwise about the
C                                    new Z-axis).
C     
C
C     Output
C     ------
C
C     DATA    - The transformed field.
C
C     Function returns .FALSE. if data cannot be transformed.
C
C
C     Method
C     ------
C
C     See reference paper below.
C
C     A file of rotation coefficients is opened corresponding to
C     the triangular truncation NM and the rotation angle SLAT:
C
C           $PP_ROT_DIR/rot_coefs_Tttt_nnnnnnnnn
C     (or $SCRATCH/rot_coefs_Tttt_nnnnnnnnn,
C      or     $PWD/rot_coefs_Tttt_nnnnnnnnn)
C
C     where:
C            ttt =  triangular truncation
C      nnnnnnnnn =  SLAT*100000,
C     or
C           $PP_ROT_DIR/rot_coefs_Tttt_Mnnnnnnnn
C     (or $SCRATCH/rot_coefs_Tttt_Mnnnnnnnn,
C      or     $PWD/rot_coefs_Tttt_Mnnnnnnnn)
C
C     where:
C            ttt =  triangular truncation
C       nnnnnnnn =  -SLAT*100000 if SLAT is negative.
C
C     If the file does not exist it is created. During creation, the
C     filename is ROTpppppp_Tttt_nnnnnnnnn, where pppppp is the current
C     process ID.
C
C     The file of rotation coefficients is UNLINKed by the process after
C     being opened for reading. This causes the file to be removed when
C     the process terminates.
C
C
C     Externals
C     ---------
C
C     INTLOG  - Logs messages.
C     INTLOGR - Logs messages.
C     GETENV  - to get environment variable data.
C     GETPID  - to get the current process ID.
C     RENAME  - to change a filename.
C     UNLINK  - to remove a file.
C
C
C     Author
C     ------
C
C     R.McGrath and P.Lynch    HIRLAM
C
C
C     Reference.
C     ----------
C
C     "Spectral synthesis on rotated and regular grids"
C     by P.Lynch and R.McGrath (Irish Meteorological Service).
C     
C
C----<
C---------------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "jparams.h"
#include "parim.h"
C
C     Function arguments.
C
      REAL*8 DATA, W
      DIMENSION DATA(*), W(*)
      INTEGER NM
      REAL*8 SLAT
C
C     Parameters.
C
      REAL*8 EPS
      PARAMETER(EPS = 1.0E-10)
      INTEGER JPNM_MAX
      PARAMETER( JPNM_MAX = 640 )
      INTEGER NPBYTES
      PARAMETER( NPBYTES = 8 )
C
C     Local variables.
C
#ifndef REAL_8
      REAL SSLAT
#endif
      LOGICAL LFOR
      INTEGER I, ISKIP, J, N, MM, K, IEND, IPLUG, IPID
      INTEGER NN, M, NDEX
      REAL*8 SIMAG, S, TEMP, DSLAT
      REAL*8 RAD, TANB, SINB, COSB, Q, RNKN, BNKN, SQNN
      REAL*8 SQNN1, SQ2N, PKN, PK1N, RNKN1, SREAL ,RNK0, RNK1
      REAL*8 WISQR(JPNM_MAX)
      REAL*8 WIB(JPNM_MAX)
      REAL*8 WIR(JPNM_MAX)
      REAL*8 WIDAT(2*JPNM_MAX)
      CHARACTER*256 FILEDUM
      CHARACTER*256 FILENAME
      CHARACTER*24 COEFILE
      DATA COEFILE/'rot_coefs_Tnnn_nnnnnnnnn'/
      INTEGER FIRST, IUNIT, IRET, ILAT, OLDLAT, OLDTRUN, IOFFSET
      INTEGER IFSIZE
      INTEGER NMP1TM, MINUS1
      DATA FIRST/0/, OLDLAT/10000000/, OLDTRUN/0/
      SAVE FIRST, IUNIT, ILAT, OLDLAT, OLDTRUN, FILENAME, COEFILE
C
C     Externals
C
#ifndef gfortran
      INTEGER GETPID, UNLINK, RENAME
      EXTERNAL GETPID, UNLINK, RENAME
#endif
C
C     Statement function
C
      REAL*8 A, B
      LOGICAL ABSCHK
      ABSCHK(A,B) = (ABS(A-B) .LT. EPS*(ABS(A) + ABS(B)))
C
C     W array: 1 -> (NM+1)*(NM+2) for PMN/SQRT(2*N+1) values.
C
C     RNKM values in WIR (for M>0),
C                 in WIB (M<0).
C     Array of temporary SQRT values in WISQR, and
C     temporary transformed data in WIDAT
C
C ------------------------------------------------------------------
C*    Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
C
      IOFFSET = 0
      JACOBI = .FALSE.
C
      CALL INTLOG(JP_DEBUG,'JACOBI: truncation = ', NM)
#ifndef REAL_8
      SSLAT = SNGL(SLAT)
      CALL INTLOGR(JP_DEBUG,'JACOBI: rotation angle = ', SSLAT)
#else
      CALL INTLOGR(JP_DEBUG,'JACOBI: rotation angle = ', SLAT)
#endif
C
C     Exit immediately if rotation angle is zero.
C
      IF (ABS(SLAT).LT.EPS) THEN
        JACOBI = .TRUE.
        GOTO 999
      ENDIF
C
C     Change to radians.
C
      RAD   = 180.0/PPI
      DSLAT = SLAT/RAD
      TANB  = TAN(DSLAT/(2.0))
      SINB  = SIN(DSLAT)
      COSB  = COS(DSLAT)
      Q     = SQRT(2.0)/2.0
C
C ------------------------------------------------------------------
C*    Section 2.   Generate PMN/SQRT(2*N+1) values in W.
C ------------------------------------------------------------------
C
  200 CONTINUE
C
      W(1)  = 1.0
      W(2)  = COSB
C
      ISKIP = NM + 1
      DO 210 I = 1,ISKIP
        W(1+ISKIP*I)   = W(1+ISKIP*I-ISKIP)*SINB*
     X                   SQRT(DBLE(2*I-1)/DBLE(2*I))
        W(1+ISKIP*I+1) = COSB*DSQRT(DBLE(2*I+1))*W(1+ISKIP*I)
  210 CONTINUE
C
      DO 220 I = 2,ISKIP-1
C
        DO 215 J = 0,ISKIP-I+1
          W(I+J*ISKIP+1) = COSB*DSQRT(DBLE((2*I+2*J-1)*(2*I+2*J-1))/
     X                     DBLE((I+2*J)*I))*W(I+J*ISKIP)
     X                     -    DSQRT(DBLE((I-1)*(I+2*J-1))/
     X                     DBLE((I+2*J)*I))*W(I+J*ISKIP-1)
  215   CONTINUE
C
  220 CONTINUE
C
C ------------------------------------------------------------------
C*    Section 3.   Generate the rotation coefficients RNKM.
C ------------------------------------------------------------------
C
  300 CONTINUE
C
C     Generate the rotation coefficients RNKM (K = M dashed in note)
C     and store in W.  For K = 0 special case.
C
C     PMN = W(M*(NM+1) + (N-M+1))
C
C     Hold the input latitude and check if it is the same as the one
C     used the last time through
C
      ILAT = NINT(SLAT*100000)
      IF( (ILAT.EQ.OLDLAT).AND.(OLDTRUN.EQ.NM) ) THEN
C
C       Rewind the existing open coefficients file.
C
        CALL PBSEEK(IUNIT,0,0,IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_FATAL,'JACOBI: Rewind failed.',JPQUIET)
          CALL PBCLOSE(IUNIT,IRET)
          JACOBI = .FALSE.
          GOTO 999
        ENDIF
C
      ELSE
C
C       A different coefficients file is needed.
C
        IF( FIRST.NE.0 ) THEN
          CALL PBCLOSE(IUNIT,IRET)
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,'JACOBI: PBCLOSE failed.',JPQUIET)
            JACOBI = .FALSE.
          ENDIF
        ENDIF
        OLDLAT = ILAT
        OLDTRUN = NM
C
C       Setup the file name: rot_coefs_Tnnn_nnnnnnnnn
C       (Path is given by PP_ROT_DIR, SCRATCH, or PWD.)
C
        CALL GETENV('PP_ROT_DIR',FILENAME)
        IOFFSET = INDEX(FILENAME,' ')
        IF( IOFFSET.EQ.1) THEN
          CALL GETENV('SCRATCH',FILENAME)
          IOFFSET = INDEX(FILENAME,' ')
          IF( IOFFSET.EQ.1) THEN
            FILENAME = '.'
          ENDIF
        ENDIF
C
        IOFFSET = INDEX(FILENAME,' ')
        WRITE(COEFILE(12:14),'(I3.3)') NM
        IF( ILAT.GE.0) THEN
          WRITE(COEFILE(16:),'(I9.9)') ILAT
        ELSE
          COEFILE(16:) = 'M'
          WRITE(COEFILE(17:),'(I8.8)') -ILAT
        ENDIF
        FILENAME = FILENAME(1:IOFFSET-1) // '/' // COEFILE
C
        FILEDUM = FILENAME
        IPID = GETPID()
        IF( IPID.LT.0 ) THEN
          CALL INTLOG(JP_FATAL,'JACOBI: GETPID failed',JPQUIET)
          JACOBI = .FALSE.
          GOTO 999
        ENDIF
        IOFFSET = INDEX(FILENAME,' ')
        IPLUG = INDEX(FILENAME,'rot_coefs') + 3
        WRITE(FILEDUM(IPLUG:IPLUG+5),'(I6.6)') IPID
C
C       Try to open the file to see if it already exists
C
        CALL PBOPEN(IUNIT,FILENAME(1:IOFFSET-1),'r',IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_DEBUG,'JACOBI: PBOPEN for read failed',JPQUIET)
          CALL INTLOG(JP_DEBUG,'JACOBI: Non-existent file:',JPQUIET)
C
C         Set flag to show file does not exist
C
          FIRST = 0
C
        ELSE
C
C         File exists
C
          FIRST = 1
C
C         Unlink the file so it disappears on close
C
          FIRST = 1
          IRET = UNLINK(FILENAME(1:IOFFSET-1))
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_FATAL,'JACOBI: UNLINK failed',JPQUIET)
            JACOBI = .FALSE.
            GOTO 999
          ENDIF
C
C         Calculate the coefficients file size and check file is
C         the correct size (coefficients are REAL*8)
C
          IFSIZE = 8*(NM*(2*NM*NM + 9*NM +13)/3)
          CALL PBSEEK(IUNIT,0,2,IRET)
          IF( IRET.LT.0 ) THEN
            CALL INTLOG(JP_FATAL,'JACOBI: PBSEEK failed.',JPQUIET)
            CALL PBCLOSE(IUNIT,IRET)
            JACOBI = .FALSE.
            GOTO 999
          ENDIF
C
          IF( IFSIZE.NE.IRET) THEN
            CALL INTLOG(JP_FATAL,
     X        'JACOBI: Coefficients file size incorrect = ',IRET)
            CALL INTLOG(JP_FATAL,
     X        'JACOBI: Expected file size is            = ',IFSIZE)
            CALL PBCLOSE(IUNIT,IRET)
            JACOBI = .FALSE.
            GOTO 999
          ENDIF
C
C         Rewind the coefficients file.
C
          CALL PBSEEK(IUNIT,0,0,IRET)
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_FATAL,'JACOBI: Rewind failed.',JPQUIET)
            CALL PBCLOSE(IUNIT,IRET)
            JACOBI = .FALSE.
            GOTO 999
          ENDIF
        ENDIF
      ENDIF      
C
      DO 530 N = 1,NM
        RNKN  = (0.5*(1.0 + COSB))**N
        BNKN  = (0.5*(1.0 - COSB))**N
        SQNN  = DSQRT(DBLE((N+1)*N))
        SQNN1 = SQNN*SINB
        SQ2N  = DSQRT(2.0/DBLE(N))/SINB
C
C       Save SQRTS.
C
        DO 325 MM = 0,N
          WISQR(MM+1) = DSQRT(DBLE((N+MM)*(N-MM+1)))*SINB
  325   CONTINUE
C
        NN = 1
C
C       Generate RNKM, M = 0 to N.
C
        DO 490 K = N,0,-1
C
C         Generate the coefficients file if it doesn't exist
C
          IF( FIRST.EQ.0 ) THEN
            FIRST = -1
            CALL INTLOG(JP_DEBUG, 'JACOBI: PBOPEN for write',JPQUIET)
            IOFFSET = INDEX(FILEDUM,' ')
            CALL PBOPEN(IUNIT,FILEDUM(1:IOFFSET-1),'w',IRET)
            IF( IRET.NE.0 ) THEN
C
C             Try to use present working directory
C
              CALL INTLOG(JP_ERROR,
     X          'JACOBI: PBOPEN for write failed',JPQUIET)
              FILENAME = COEFILE
              FILEDUM  = COEFILE
              IOFFSET = INDEX(FILEDUM,' ')
              CALL PBOPEN(IUNIT,FILEDUM(1:IOFFSET-1),'w',IRET)
              IF( IRET.NE.0 ) THEN
                CALL INTLOG(JP_ERROR,
     X            'JACOBI: PBOPEN for write failed',JPQUIET)
                JACOBI = .FALSE.
                GOTO 999
              ENDIF
            ENDIF
          ENDIF
C
          IF( FIRST.LE.0) THEN
C
            PKN = W(K*(NM+1) + (N-K+1))
            IF (K.EQ.0) GOTO 400
C
C           For K > 0, go forward from zero.
C
            PK1N = W((K-1)*(NM+1) + (N-(K-1)+1))
C
C           Flip sign if necessary.
C
            IF (MOD(K,2).NE.0)THEN
              PKN = -PKN
            ELSE
              PK1N = -PK1N
            ENDIF
C
C           Work using forward recurrence as long as coefficient
C           calculated passes checks.
C
            LFOR = .TRUE.
C
            RNK0 = PKN
            RNK1 = (-DBLE(K)*TANB*PKN + (WISQR(K+1)/SINB)*PK1N)/SQNN
C
C           Recurrence starts at 2 for M = 0
C
            WIR(1) = RNK0
            WIR(2) = RNK1
C
            DO 330 MM = 1,N-1
              WIR(MM+2) = (2.0*WIR(MM+1)*(DBLE(MM)*COSB-DBLE(K))
     X                      - WISQR(MM+1)*WIR(MM)) /WISQR(MM+2)
C
C             Apply check to generated coefficient to see if its
C             absolute value is greater than 1.  If so, have to
C             switch to using backwards recurrences.
C
              IF (ABS(WIR(MM+2)).GT.1.0)THEN
                LFOR = .FALSE.
                IEND = MM + 3
                GOTO 335
              ENDIF
  330       CONTINUE
C
C           If forward recurrence appears OK so far,
C           test last element RNKN by comparing with WIR(N+1).
C
            IEND = N + 2
            IF (ABSCHK(RNKN,WIR(N+1))) GOTO 345
C
C           If test failed, try generating coefficients using
C           backwards recurrences.
C
  335       CONTINUE
C
C           Work backwards from the top.
C           Specify N; K is already set from the loop above.
C           Stop at M = 1 (NOT 0).
C
            RNKN1 = RNKN*SQ2N*(DBLE(N)*COSB-DBLE(K))
            WIR(N+1) = RNKN
C
C           Check whether difference is within prescribed tolerance.
C
            IF (LFOR .AND. ABSCHK(RNKN1,WIR(N))) GO  TO 345
            WIR(N) = RNKN1
C
C           For M = 0:
C
            DO 340 MM = N-1,1,-1
              S = (2.0*WIR(MM+1)*(DBLE(MM)*COSB-DBLE(K))
     X              - (WISQR(MM+2)*WIR(MM+2))) / WISQR(MM+1)
C
C             Accept these if forward recursion failed before
C             reaching this point.
C
              IF (MM+1.LT.IEND)THEN
                IF (ABSCHK(S,WIR(MM))) GOTO 345
              ENDIF
              WIR(MM) = S
C
C             If absolute value is greater than 1, give up gracefully
C
              IF (ABS(S).GT.1.0) GOTO 920
  340       CONTINUE
C
            GOTO 920
C
  345       CONTINUE
C
C           Now RNKM for M = 0,-N
C
C           RNKM M = -1,-N
C
C           Forward recurrence starts at 1 for M = 0, 2 for -1 etc.
C
            LFOR = .TRUE.
C
            WIB(1) = RNK0
            WIB(2) = -RNK1-RNK0*DBLE(2*K)/(SQNN1)
C
            DO 350 MM = 1,(N-1)
              WIB(MM+2) = (2.0*WIB(MM+1)*(DBLE(-MM)*COSB-DBLE(K))
     X              - WISQR(MM+1)*SINB*WIB(MM)) /WISQR(MM+2)
C
C             Apply check to generated coefficient to see if its
C             absolute value is greater than 1.  If so, have to
C             switch to using backwards recurrences.
C
              IF (ABS(WIB(MM+2)).GT.1.0)THEN
                LFOR = .FALSE.
                IEND = MM + 3
                GOTO 355
              ENDIF
  350       CONTINUE
C
C           If forward recurrence appears OK so far,
C           test last element BNKN by comparing with WIB(N+1).
C
            IEND = N + 1
            IF (ABSCHK(BNKN,WIB(N+1))) GOTO 400
C
C           If test failed, try generating coefficients using backwards
C           recurrences.
C
  355       CONTINUE
C
C           Get R values.
C           First RNKN, then next highest RNKN1.
C
            RNKN1 = -BNKN*SQ2N*(DBLE(N)*COSB + DBLE(K))
C
C           Work backwards from the top.
C           Specify N; K is already set from the loop above.
C
C           Stop at M = 1 (NOT 0).
C
            WIB(N+1) = BNKN
            IF (LFOR .AND. ABSCHK(RNKN1,WIB(N))) GOTO 400
            WIB(N) = RNKN1
C
            DO 360 MM = N-1,1,-1
              S = (2.0*WIB(MM+1)*(DBLE(-MM)*COSB-DBLE(K))
     X              - (WISQR(MM+2)*WIB(MM+2))) / WISQR(MM+1)
C
C             Accept backwards generated coefficients if forward
C             recursion failed before reaching this point.
C
              IF ( (MM.LT.IEND ) .AND. ABSCHK(S,WIB(MM)) ) GOTO 400
              WIB(MM) = S
C
C             If absolute value is greater than 1, give up.
C
              IF (ABS(S).GT.1.0) GOTO 910
C
  360       CONTINUE
C
C           End of backwards recurrences reached with no agreed
C           coefficients, give up.
C
            GOTO 910
C
C ------------------------------------------------------------------
C*    Section 4.   Got RNKM. Now transform spectral data.
C ------------------------------------------------------------------
C
  400       CONTINUE
C
C           Write coefficients to file
C
            CALL PBWRITE(IUNIT,WIR,(N+1)*NPBYTES,IRET)
            IF( IRET.NE.((N+1)*NPBYTES) ) THEN
              CALL INTLOG(JP_FATAL,'JACOBI: PBWRITE failed.',JPQUIET)
              CALL PBCLOSE(IUNIT,IRET)
              JACOBI = .FALSE.
              GOTO 999
            ENDIF
C
            CALL PBWRITE(IUNIT,WIB,(N+1)*NPBYTES,IRET)
            IF( IRET.NE.((N+1)*NPBYTES) ) THEN
              CALL INTLOG(JP_FATAL,'JACOBI: PBWRITE failed.',JPQUIET)
              CALL PBCLOSE(IUNIT,IRET)
              JACOBI = .FALSE.
              GOTO 999
            ENDIF
        
          ELSE
C
  415       CONTINUE
C
            PKN  = W(K*(NM+1) + (N-K+1))
            PK1N = W((K-1)*(NM+1) + (N-(K-1)+1))
C
C           Flip sign if necessary.
C
            IF (MOD(K,2).NE.0)THEN
              PKN  = -PKN
            ELSE
              PK1N = -PK1N
            ENDIF
C
C           Read coefficients from file
C
            CALL PBREAD(IUNIT,WIR,(N+1)*NPBYTES,IRET)
            IF( IRET.NE.((N+1)*NPBYTES) ) THEN
              CALL INTLOG(JP_FATAL,'JACOBI: PBREAD failed.',JPQUIET)
              CALL PBCLOSE(IUNIT,IRET)
              JACOBI = .FALSE.
              GOTO 999
            ENDIF
C
            CALL PBREAD(IUNIT,WIB,(N+1)*NPBYTES,IRET)
            IF( IRET.NE.((N+1)*NPBYTES) ) THEN
              CALL INTLOG(JP_FATAL,'JACOBI: PBREAD failed.',JPQUIET)
              CALL PBCLOSE(IUNIT,IRET)
              JACOBI = .FALSE.
              GOTO 999
            ENDIF
C
          ENDIF
C
C         Special case when K=0.
C
          IF( K.EQ.0) THEN
C
            SREAL = DATA(2*(N+1)-1)*PKN
            SIMAG = 0.0
            DO 420 M = 1,N
              NMP1TM = (NM+1)*M
              SREAL = SREAL
     X        + 2.0*DATA(2*(NMP1TM-((M-1)*M)/2+1+N-M)-1)
     X        * W(NMP1TM+(N-M+1))
  420       CONTINUE
C
            NDEX        = 1+N
            WIDAT(NN)   = SREAL
            WIDAT(NN+1) = SIMAG
            NN          = NN + 2
C
          ELSE
C
            SREAL  = DATA(2*(N+1)-1)*PKN
            SIMAG  = 0.0
            MINUS1 = -1
            DO 440 M = 1,N
              SREAL = SREAL +
     X              DATA(2*((NM+1)*M-((M-1)*M)/2+1+N-M)-1)
     X              * (WIR(M+1)+WIB(M+1)*MINUS1)
              SIMAG = SIMAG -
     X              DATA(2*((NM+1)*M -((M-1)*M)/2+1+N-M))
     X              * (-WIR(M+1)+WIB(M+1)*MINUS1)
              MINUS1 = -MINUS1
  440       CONTINUE
C
            NDEX        = (NM+1)*K - ((K-1)*K)/2+1+N-K
            WIDAT(NN)   = SREAL
            WIDAT(NN+1) = SIMAG
            NN          = NN + 2
          ENDIF
C
C         Update RNKN at both ends of the forward and backward
C         recurrences.
C
          TEMP = DSQRT(DBLE(N+K)/DBLE(N-K+1))
          RNKN = RNKN*TEMP*SINB/(1+COSB)
          BNKN = -BNKN*TEMP*(1+COSB)/SINB
  490   CONTINUE
C
C       End of inner loop.
C
C ------------------------------------------------------------------
C*    Section 5.   Now place the values of the rotated spectral
C                  coefficients in DATA.
C ------------------------------------------------------------------
C
  500 CONTINUE
C
        NN = 1
        DO 520 K = N,0,-1
          NDEX = (NM+1)*K - ((K-1)*K)/2+1+N-K
          DATA(2*NDEX-1) = WIDAT(NN)
          DATA(2*NDEX)   = WIDAT(NN+1)
          NN = NN + 2
C
  520   CONTINUE
C
  530 CONTINUE
C
C ------------------------------------------------------------------
C*    Section 9.   Return.
C ------------------------------------------------------------------
C
  900 CONTINUE
C
      JACOBI = .TRUE.
C
C     If coefficients file has just been generated:
C      - close it,
C      - rename it,
C      - re-open for reading
C      - unlink it, so it can disappear when closed,
C      - leave it open.
C
      IF( FIRST.LE.0 ) THEN
        FIRST = 1
        CALL PBCLOSE(IUNIT,IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'JACOBI: PBCLOSE failed.',JPQUIET)
          JACOBI = .FALSE.
          GOTO 999
        ENDIF
C
        FILEDUM(IOFFSET:IOFFSET) = CHAR(0)
        FILENAME(IOFFSET:IOFFSET) = CHAR(0)
        IRET = RENAME(FILEDUM(1:IOFFSET-1),FILENAME(1:IOFFSET-1))
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_FATAL,'JACOBI: RENAME of file failed',JPQUIET)
          JACOBI = .FALSE.
          GOTO 999
        ENDIF
C
        CALL INTLOG(JP_DEBUG, 'JACOBI: PBOPEN for read',JPQUIET)
        IOFFSET = INDEX(FILENAME,' ')
        CALL PBOPEN(IUNIT,FILENAME(1:IOFFSET-1),'r',IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_FATAL,'JACOBI: PBOPEN for read failed',JPQUIET)
          JACOBI = .FALSE.
          GOTO 999
        ENDIF
C
        IRET = UNLINK(FILENAME(1:IOFFSET-1))
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'JACOBI: UNLINK of file failed',JPQUIET)
          JACOBI = .FALSE.
          GOTO 999
        ENDIF
      ENDIF
C
  999 CONTINUE
      RETURN
C
C     Failure to converge with M < 0.
C
  910 CONTINUE
      CALL INTLOG(JP_FATAL,'JACOBI: Failure to converge M < 0', JPQUIET)
      JACOBI = .FALSE.
      GOTO 999
C
C     Failure to converge with M > 0.
C
  920 CONTINUE
      CALL INTLOG(JP_FATAL,'JACOBI: Failure to converge M > 0', JPQUIET)
      JACOBI = .FALSE.
      GOTO 999
      END
