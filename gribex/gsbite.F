C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

#if (defined CRAY) || (defined __uxp__ ) || (defined IBM_POWER4) || (defined rs6000) || (defined NECSX) || (defined POWERPC)
      SUBROUTINE GSBITE (KS,KD,KSKST,KSIZE,KSKBTW,K,KBPW,KMASK,HADIR)
C
C---->
C**** GSBITE - Extraction/insertion of bits from/to bitstream on Cray.
C
C     Purpose.
C     --------
C
C     Vectorising extraction/insertion of bits from/to bitstream.
C
C**   Interface.
C     ----------
C
C     CALL GSBITE (KS,KD,KSKST,KSIZE,KSKBTW,K,KBPW,KMASK,HADIR)
C
C
C     Input Parameters.
C     -----------------
C
C     KS      - If HADIR='D', input bit stream, else output bit stream.
C     KD      - If HADIR='D', output words, else input words.
C     KSKST   - Number of bits skipped at beginning of KS.
C     KSIZE   - Number of bits to be extracted to one word of KD.
C     KSKBTW  - Number of bits skipped between 2 words to be extracted.
C     K       - Number of words to be extracted into KD. If less than
C               or equal to 0 only calculate KBPW and KMASK.
C     KBPW    - Number of bits per word in KS,calculated if 0.
C     KMASK   - Masks for bit patterns, calculated if KMASK(2) is 0.
C               (KMASK must have dimension KBPW+1)
C     HADIR   - Direction of conversion: 'D' for decoding, ie
C               extract words KD(1...K) from bits KS(KSKST+1....)
C               If not 'D', encode, i.e. pack words KD(1....K) into
C               bits KS(KSKST+1.....KSKST+K*(KSIZE+KSKBTW))
C
C
C     Output Parameters.
C     ------------------
C
C     KS,KD   - See above.
C     KSKST   - Updated to number of bits used, i.e. to
C               KSKST+K*(KSIZE+KSKBTW)
C     KBPW    - If 0 on input, number of bits in each word of KS.
C     KMASK   - If KMASK(2) was 0 on input, bit pattern masks.
C               (KMASK must have dimension KBPW+1)
C
C
C     Method.
C     -------
C
C     Vector loop is over repeatedly occurring bit patterns.
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
C     None.
C
C
C     Comments.
C     ---------
C
C     This routine is for vector processors only.
C
C
C     Author.
C     -------
C
C     G.J.Cats 08 Dec 87
C
C
C     Modifications.
C     --------------
C
C     J. Hennessy     ECMWF     09.09.91
C     Introductory comments changed to conform to standard.
C
C     J.D.Chambers    ECMWF     05.01.95
C     Put in fixes suggested by G.Cats (Dec. 95) for:
C     i)    "Number of words to be extracted = 0"
C     ii)   Optimise test for encode/decode in loop by using logical.
C     iii)  Intialise ISTS before use.
C
C     J.D.Chambers    ECMWF     22.05.96
C     Defines to remove inline functions except on CRAY
C     (The Fujitsu doesn't need them).
C
C     J. Clochard, Meteo France, for ECMWF - January 1998.
C     Use intrinsic functions on CRAY.
C
C
C     ------------------------------------------------------------------
C----<
C
      DIMENSION KS(*),KD(*),KMASK(*)
      CHARACTER*1 HADIR
      LOGICAL     LODECD
C
C     1.  COMPLETE KBPW AND KMASK, RETURN IF 0 WORDS ARE TO BE EXTRACTED
C
      IF(KBPW.EQ.0) THEN
         IS    = KS(1)
         KS(1) = 1
 1101    CONTINUE
         IF(KS(1).NE.0) THEN
            KBPW  = KBPW + 1
            KS(1) = ISHFT(KS(1),1)
            GOTO 1101
         ENDIF
         KS(1) = IS
      ENDIF
      IF(KMASK(2).EQ.0) THEN
         KMASK(KBPW+1) = 0
         DO 1110 J = KBPW,1,-1
           KMASK(J) = IBSET(KMASK(J+1),KBPW-J)
 1110    CONTINUE
      ENDIF
      IF(K.LE.0)RETURN
C
C     2.  PRESET KD TO 0 IF KD IS OUTPUT I.E. WHEN DECODING
C
      LODECD = HADIR .EQ. 'D'
      IF (LODECD) THEN
         DO 2101 J = 1,K
           KD(J) = 0
 2101    CONTINUE
      ENDIF
      IF(KSIZE.LE.0) RETURN
C
C     3.  CALCULATE SEVERAL PARAMETERS FOR LOOPING (FOR EFFICIENCY, THE
C         CODE OF SECTIONS 3.3 AND 3.4 FOR K=1 IS SEPARATED INTO 3.2)
C
C     3.1 NUMBER OF BITS USED PER WORD, INITIAL NR OF SKIPPED BITS
C
      ISTEP = KSIZE + KSKBTW
      ISKWS = KSKST
C
C     3.2 VECTOR LOOP LENGTH AND STEP SIZE IN KD IF K=1;KS STEP IRRELVNT
C
      IF(K.EQ.1) THEN
         ILL  = 1
         IBDL = 2
         ISTD = 1
         ISTS = 1
      ELSE
C
C     3.3 STEP SIZES IN KS,KD: INVERSE OF LARGEST FACTOR OF ISTEP,KBPW
C
         ILCF = KBPW
         ISHF = ISTEP
 331     CONTINUE
         IF(ILCF.EQ.ISHF) GOTO 332
         IF(ILCF.EQ.1) GOTO 332
         IF(ILCF.GT.ISHF) THEN
            ILCF = ILCF - ISHF
         ELSE
            ISHF = ISHF - ILCF
         ENDIF
         GOTO 331
 332     CONTINUE
         ISTD = KBPW/ILCF
         ISTS = ISTEP/ILCF
C
C     3.4 VECTOR LOOP LENGTH AND SWITCH-OVER POINT FOR SMALLER LOOP
C
         ILL  = (K-1)/ISTD + 1
         IBDL = K - (ILL-1)*ISTD
      ENDIF
C
C     4.  LOOP OVER FIRST ISTD WORDS OF KD (TRAILS THE VECTOR LOOP)
C
      DO 790 JBD = 1,ISTD
C
C       4.1 LAST BIT IN KS TO BE TREATED
C
        IENBS = ISKWS + KSIZE
C
C       4.2 NR OF WORDS OF KS TO BE SKIPPED, NR OF BITS IN THOSE AND
C           THIS
C
        ISKW = ISKWS/KBPW
        ISTA = ISKW*KBPW
        ISKB = ISKWS - ISTA
C
C       4.3 MASK AND LEFT SHIFT FOR THE REMAINING BITS
C
        IMASK = KMASK(ISKB+1)
        ISH   = KSIZE + ISKB
C
C       4.4 POSITION OF CURRENT WORD OF KS
C
        IBS = ISKW + 1
C
C       5.  LOOP OVER WORDS OF KS CONTRIBUTING TO ONE WORD OF KD
C
 500    CONTINUE
C
C       5.1 UPDATE SHIFT AND LAST BIT IN CURRENT WORD
C
        ISH  = ISH - KBPW
        IEND = ISTA + KBPW
C
C       5.2 IS LAST BIT OF CURRENT WORD OUTSIDE RANGE TO BE EXTRACTED
C
        IF(IEND.GT.IENBS) THEN
          ISH   = IENBS - IEND
          IMASK = IAND(IMASK,NOT(KMASK(KBPW+ISH+1)))
        ENDIF
C
C       5.3 INITIAL OFFSETS FOR VECTOR ELEMENTS IN VECTOR LOOP
C
        IOS = 0
        IOD = 0
C
C     6.  VECTOR LOOP IS OVER REPEATEDLY OCCURRING BITPATTERNS/MASKS
C
        IF (LODECD) THEN
Cjdc#ifdef CRAY
CDIR$ IVDEP
!cdir nodep
          DO 611 JI = 1,ILL
            KD(JBD+IOD) =
     X        IOR(KD(JBD+IOD),ISHFT(IAND(IMASK,KS(IBS+IOS)),ISH))
            IOD = IOD + ISTD
            IOS = IOS + ISTS
 611      CONTINUE
Cjdc#else
!OCL NOVREC
Cjdc       CALL GSDECODE( KS(IBS), KD(JBD), ISTS, ISTD, ILL, IMASK, ISH )
Cjdc#endif
        ELSE
Cjdc#ifdef CRAY
CDIR$ IVDEP
!OCL NOVREC
!cdir nodep
          DO 612 JI = 1,ILL
            KS(IBS+IOS) = IOR(
     X      IAND(      KS(IBS+IOS),           NOT(IMASK)),
     X      IAND(ISHFT(KD(JBD+IOD),-ISH),    IMASK ))
            IOD = IOD + ISTD
            IOS = IOS + ISTS
 612      CONTINUE
Cjdc#else
Cjdc        CALL GSENCODE( KD(JBD), KS(IBS), ISTD, ISTS, ILL, IMASK, ISH )
Cjdc#endif
        ENDIF
C
C       7.  END LOOPS
C
C       7.1 PREPARE FOR END OF LOOP OVER WORDS OF KS WITIHN ONE KD WORD
C
        ISTA = ISTA + KBPW
C
C       7.2 NEXT WORD OF KD IF EXTRACTION NOT COMPLETED
C
        IF(ISTA.LT.IENBS) THEN
          IMASK = KMASK(1)
          IBS   = IBS + 1
          GOTO 500
        ENDIF
C
C       7.8 PREPARE FOR END OF LOOP OVER FIRST WORDS OF KD
C
        IF(JBD.EQ.IBDL)ILL = ILL - 1
        ISKWS = ISKWS + ISTEP
C
C       7.9 END LOOP OVER FIRST WORDS OF KD
C
 790  CONTINUE
C
C     8.  FINISHED: UPDATE KSKST AND RETURN
C
      KSKST = KSKST + K*ISTEP
C
      RETURN
      END
#endif
