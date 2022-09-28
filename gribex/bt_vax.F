C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE INXBIT (KGRIB,KLENG,KNSPT,KPARM,KNUM,KBIT,
     C                   KBLEN,HFUNC,KRET)
C
C---->
C**** INXBIT - Insert/extract bits consecutively in/from a given array
C
C     Purpose.
C     --------
C
C           Take rightmost KBLEN bits from KNUM words of KPARM
C           and insert them consecutively in KGRIB, starting at
C           bit after KNSPT or vice versa.
C
C**   Interface.
C     ----------
C
C           CALL INXBIT (KGRIB,KLENG,KNSPT,KPARM,KNUM,KBIT,
C    C                   KBLEN,KRET)
C
C           Integer    K.
C           Real       P.
C           Logical    O.
C           Character  H.
C
C               Input Parameters.
C               -----------------
C
C               KGRIB      - Array containing bitstream.
C               KLENG      - Length (words) of this array.
C               KNSPT      - Bit number after which insertion or
C                            extraction starts.
C               KPARM      - Array from which bits are taken for
C                            insertion in the bitstream or to which
C                            bits are extracted from the bitstream.
C               KBIT       - Number of bits in computer word.
C               KNUM       - Number of bit fields inserted/extracted.
C               KBLEN      - Number of bits per bit field.
C               HFUNC      - Requested function.
C                            'C' to insert bits in bitstream,
C                            'D' to extract bits from bitstream.
C
C               Output Parameters.
C               ------------------
C
C               KNSPT      - Bit number of last bit inserted/extracted.
C
C               KRET       - Return code.
C                            0 , No error encountered.
C                            1 , Insertion/extraction exceeded
C                                array boundary.
C
C     Method.
C     -------
C
C           Word and offset pointer calculated before calling
C           insertion/extraction routines.
C
C     Externals.
C     ----------
C
C           SBYTES
C           GBYTES
C
C     Reference.
C     ----------
C
C           ECLIB documentation on SBYTES and GBYTES.
C
C     Comments.
C     ---------
C
C           VAX version of routine.
C           Routine contains Sections 0 to 3 and Section 9.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      18.06.91
C
C     Modifications.
C     --------------
C
C           J. Hennessy      ECMWF      08.11.91
C           Parameter KMACH removed from list of input parameters.
C
C           J. Hennessy      ECMWF      12.10.92
C           Dimension of IMASK changed from 64 to 65.
C
C     ----------------------------------------------------------------
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
C*    Section 0 . Definition of variables. Data statements.
C     ----------------------------------------------------------------
C
C*    Prefix conventions for variable names.
C
C     Logical      L (but not LP), global or common.
C                  O, dummy arguments.
C                  G, local variable.
C                  LP, parameter.
C     Character    C, Global or common.
C                  H, dummy arguments.
C                  Y (but not YP), local variables.
C                  YP, parameter.
C     Integer      M and N, global or common.
C                  K, dummy arguments.
C                  I, local variables.
C                  J (but not JP), loop control.
C                  JP, parameter.
C     Real         A to F and Q to X, global or common.
C                  P (but not PP), dummy arguments.
C                  Z, local variables.
C                  PP, parameter.
C
      INTEGER IND
      INTEGER INUM
      INTEGER IOFF
      INTEGER IPR
      INTEGER IWORD
C
      INTEGER KBIT
      INTEGER KBLEN
      INTEGER KGRIB
      INTEGER KLENG
      INTEGER KNSPT
      INTEGER KNUM
      INTEGER KPARM
      INTEGER KRET
C
      INTEGER J901
C
      DIMENSION KGRIB(KLENG)
      DIMENSION KPARM(*)
C
      CHARACTER*1 HFUNC
C
C
C     Debug print switch.
C
      DATA IPR /0/
C
C     ----------------------------------------------------------------
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
C*    Section 1 . Set initial values.
C     ----------------------------------------------------------------
C
  100 CONTINUE
C
      IF (IPR.EQ.1)
     C   THEN
             WRITE (*,*) 'INXBIT : Section 1.'
             WRITE (*,*) '         Input values used -'
             WRITE (*,9009) KLENG
             WRITE (*,9002) KNSPT
             WRITE (*,9004) KBIT
             WRITE (*,9005) HFUNC
         ENDIF
C
      KRET = 0
C
C     ----------------------------------------------------------------
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
C*    Section 2 . Bit insertion/extraction.
C     ----------------------------------------------------------------
C
  200 CONTINUE
C
      IF (IPR.EQ.1) WRITE (*,*) 'INXBIT : Section 2.'
C
C*    Calculate word pointer and offset.
C
      IWORD = KNSPT / KBIT
      IOFF  = KNSPT - IWORD * KBIT
      IWORD = IWORD + 1
      IF (IPR.EQ.1) WRITE (*,9003) IWORD , IOFF
C
C     Insert/extract bits.
C
      IF (HFUNC.EQ.'C')
     C   THEN
             CALL SBYTES (KGRIB(IWORD),KPARM,IOFF,KBLEN,0,KNUM)
         ELSE
             CALL GBYTES (KGRIB(IWORD),KPARM,IOFF,KBLEN,0,KNUM)
         ENDIF
C
C     Update pointer.
C
      KNSPT = KNSPT + KBLEN * KNUM
C
C     ----------------------------------------------------------------
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
C*    Section 3 . Check out of range.
C    -----------------------------------------------------------------
C
  300 CONTINUE
C
      IF (IPR.EQ.1) WRITE (*,*) 'INXBIT : Section 3.'
C
      IND = KNSPT / KBIT
      IF (IND.GT.KLENG)
     C   THEN
             KRET = 1
             WRITE (*,9001) IND , KLENG
         ENDIF
C
C     ----------------------------------------------------------------
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
C*    Section 9 . Return to calling routine. Format statements.
C     ----------------------------------------------------------------
C
  900 CONTINUE
C
      IF (IPR.EQ.1)
     C   THEN
             INUM = KNUM
             IF (INUM.GT.360)
     C          THEN
                    INUM = 360
                    WRITE (*,9007) INUM
                ENDIF
             DO 901 J901=1,INUM
               IF (HFUNC.EQ.'C')
     C             THEN
                       WRITE (*,9006) KPARM(J901)
                   ELSE
                       WRITE (*,9008) KPARM(J901)
                   ENDIF
  901        CONTINUE
             WRITE (*,*) 'INXBIT : Section 9.'
             WRITE (*,*) '         Output values set -'
             WRITE (*,9002) KNSPT
         ENDIF
C
C
 9001 FORMAT (1H ,'INXBIT : Word ',I8,' is outside array bounds ',I8)
C
 9002 FORMAT (1H ,'         KNSPT  = ',I8)
C
 9003 FORMAT (1H ,'INXBIT : Word is',I8,', bit offset is ',I2)
C
 9004 FORMAT (1H ,'         KBIT   = ',I8)
C
 9005 FORMAT (1H ,'         HFUNC  = ',A)
C
 9006 FORMAT (1H ,'         Inserted value = ',I20)
C
 9007 FORMAT (1H ,'         First ',I9,' values.')
C
 9008 FORMAT (1H ,'         Extracted value = ',I20)
C
 9009 FORMAT (1H ,'         KLENG  = ',I20)
C
      RETURN
C
      END
      SUBROUTINE ABORTX (HNAME)
C
C**** ABORTX - Terminates execution of program.
C
C     Purpose.
C     --------
C
C           Terminates execution of program.
C
C**   Interface.
C     ----------
C
C           CALL ABORTX (HNAME)
C
C           Integer    K.
C           Real       P.
C           Logical    O.
C           Character  H.
C
C               Input Parameters.
C               -----------------
C
C               HNAME      - Name of calling routine.
C
C               Output Parameters.
C               ------------------
C
C               None.
C
C     Method.
C     -------
C
C           Prints message and terminates.
C
C     Externals.
C     ----------
C
C           EXIT
C
C     Reference.
C     ----------
C
C           None.
C
C     Comments.
C     ---------
C
C           VAX version of routine.
C           Routine contains Sections 0 to 1 and Section 9.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      13.11.91
C
C     Modifications.
C     --------------
C
C           None.
C
C     ------------------------------------------------------------------
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
C     ------------------------------------------------------------------
C
C*    Prefix conventions for variable names.
C
C     Logical      L (but not LP), global or common.
C                  O, dummy arguments.
C                  G, local variable.
C                  LP, parameter.
C     Character    C, Global or common.
C                  H, dummy arguments.
C                  Y (but not YP), local variables.
C                  YP, parameter.
C     Integer      M and N, global or common.
C                  K, dummy arguments.
C                  I, local variables.
C                  J (but not JP), loop control.
C                  JP, parameter.
C     Real         A to F and Q to X, global or common.
C                  P (but not PP), dummy arguments.
C                  Z, local variables.
C                  PP, parameter.
C
C
      CHARACTER*(*) HNAME
C
C     ------------------------------------------------------------------
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
C*    Section 1 . Print message and terminate.
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
      WRITE (*,9001) HNAME
C
      CALL EXIT (-1)
C
C     ------------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C*    Section 9 . Format statements.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
 9001 FORMAT (1H ,'ABORTX : Routine ',A,' has requested program',
     C               ' termination.')
C
      RETURN
C
      END
      SUBROUTINE SETPAR (KBIT,KNEG,KPR)
C
C**** SETPAR - Set number of bits in word. Set maximum negative integer.
C
C     Purpose.
C     --------
C
C           Set number of bits in word. Set maximum negative integer.
C
C**   Interface.
C     ----------
C
C           CALL SETPAR (KBIT,KNEG,KPR)
C
C           Integer    K.
C           Real       P.
C           Logical    O.
C           Character  H.
C
C               Input Parameters.
C               -----------------
C
C               KPR        - Debug print switch.
C                            1 , print out.
C                            0 , No print out.
C
C               Output Parameters.
C               ------------------
C
C               KBIT       - Number of bits in computer word.
C
C               KNEG       - Maximum negative integer.
C
C     Method.
C     -------
C
C           Values are assigned.
C
C     Externals.
C     ----------
C
C           None.
C
C     Reference.
C     ----------
C
C           None.
C
C     Comments.
C     ---------
C
C           VAX version of routine.
C           Routine contains Sections 0 to 3 and Section 9.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      28.10.91
C
C     Modifications.
C     --------------
C
C           None.
C
C     ------------------------------------------------------------------
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
C*    Section 0 . Definition of variables. Data statements.
C     ------------------------------------------------------------------
C
C*    Prefix conventions for variable names.
C
C     Logical      L (but not LP), global or common.
C                  O, dummy arguments.
C                  G, local variable.
C                  LP, parameter.
C     Character    C, Global or common.
C                  H, dummy arguments.
C                  Y (but not YP), local variables.
C                  YP, parameter.
C     Integer      M and N, global or common.
C                  K, dummy arguments.
C                  I, local variables.
C                  J (but not JP), loop control.
C                  JP, parameter.
C     Real         A to F and Q to X, global or common.
C                  P (but not PP), dummy arguments.
C                  Z, local variables.
C                  PP, parameter.
C
C
      INTEGER KBIT
      INTEGER KNEG
      INTEGER KPR
C
C     ------------------------------------------------------------------
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
C*    Section 1 . Assign values.
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
      IF (KPR.EQ.1)  WRITE (*,*) ' SETPAR : Section 1.'
C
      KBIT = 32
      KNEG = -2147483648
C
C     ------------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C*    Section 9 . Return to calling routine. Format statements.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
      IF (KPR.EQ.1)
     C   THEN
             WRITE (*,*) ' SETPAR : Section 9.'
             WRITE (*,*) '          Output values set -'
             WRITE (*,9001) KBIT
             WRITE (*,9002) KNEG
         ENDIF
C
 9001 FORMAT (1H ,'          KBIT = ',I3)
C
 9002 FORMAT (1H ,'          KNEG = ',I22)
C
      RETURN
C
      END
      SUBROUTINE GBITS(NPACK,ISAM,IBIT,NBITS,NSKIP,ITER)
C
C**** GBITS - VAX routine.
C
      DIMENSION NPACK(*),ISAM(*)
      PARAMETER (LENWRD=32)
C
C     ++++++++++
C     CALL GBITS(NPACK,ISAM,IBIT,NBITS,NSKIP,ITER)
C     ++++++++++
C
C     STARTS UNPACKING BITS AT BIT OFFSET IBIT IN ARRAY
C     NPACK.  IT TAKES NBITS AND STORES THEM IN THE LONGWORD
C     ARRAY STARTING AT ISAM.  THEN NSKIP BITS ARE SKIPPED IN
C     NPACK AND THE NEXT FIELD OF NBITS IS UNPACKED INTO THE
C     NEXT ISAM.  THIS IS DONE A TOTAL OF ITER TIMES.
C
C     THIS ROUTINE EXTRACTS BITS FROM VAX WORDS.
C     IT IS SIMILAR TO NCAR'S GBYTE BUT IT WORKS WITH
C     THE NATURAL UNDERLYING STRUCTURE OF THE VAX.
C     ----------
C
      ISW = 1
      GO TO 8
C
C
      ENTRY SBITS(NPACK,ISAM,IBIT,NBITS,NSKIP,ITER)
C
C     ++++++++++
C     CALL SBITS(NPACK,ISAM,IBIT,NBITS,NSKIP,ITER)
C     ++++++++++
C
C     THIS ROUTINE TAKES THE FIRST LONGWORD LOCATED AT
C     ISAM AND STORES IT INTO THE NBITS OF NPACK STARTING AT
C     BIT OFFSET IBIT.  THEN NSKIP BITS ARE SKIPPED IN
C     NPACK AND THE PROCESS IS REPEATED A TOTAL OF ITER TIMES.
C
C     THIS ROUTINE EXTRACTS BITS FROM VAX WORDS.
C     IT IS SIMILAR TO NCAR'S SBYTE BUT IT WORKS WITH
C     THE NATURAL UNDERLYING STRUCTURE OF THE VAX.
C     ----------
      ISW = 2
      GO TO 8
C
C     ++++++++++
      ENTRY G_BYTES(NPACK,ISAM,IBIT,NBITS,NSKIP,ITER)
C     ++++++++++
C
C     CALL G_BYTES(NPACK,ISAM,IBIT,NBITS,NSKIP,ITER)
C     STARTS UNPACKING BITS AT BIT OFFSET IBIT IN ARRAY
C     NPACK.  IT TAKES NBITS AND STORES THEM IN THE LONGWORD
C     ARRAY STARTING AT ISAM.  THEN NSKIP BITS ARE SKIPPED IN
C     NPACK AND THE NEXT FIELD OF NBITS IS UNPACKED INTO THE
C     NEXT ISAM.  THIS IS DONE A TOTAL OF ITER TIMES.
C
C     THIS ROUTINE DEALS WITH BITS AND BYTES IN THE ORDER
C     THEY ARE NUMBERED ON THE MESA (IE BIGENDIAN).
C     ----------
      ISW = 3
      GO TO 8
C
C     ++++++++++
      ENTRY S_BYTES(NPACK,ISAM,IBIT,NBITS,NSKIP,ITER)
C     ++++++++++
C
C     CALL S_BYTES(NPACK,ISAM,IBIT,NBITS,NSKIP,ITER)
C     THIS ROUTINE TAKES THE FIRST LONGWORD LOCATED AT
C     ISAM AND STORES IT INTO THE NBITS OF NPACK STARTING AT
C     BIT OFFSET IBIT.  THEN NSKIP BITS ARE SKIPPED IN
C     NPACK AND THE PROCESS IS REPEATED A TOTAL OF ITER TIMES.
C
C
C     THIS ROUTINE DEALS WITH BITS AND BYTES IN THE ORDER
C     THEY ARE NUMBERED ON THE MESA (IE BIGENDIAN).
C     ----------
      ISW = 4
      GOTO 8
C
C     ++++++++++
      ENTRY GBYTES(NPACK,ISAM,IBIT,NBITS,NSKIP,ITER)
C     ++++++++++
C
C        THIS ROUTINE IS JUST A REPEATED CALL TO G_BYTE
C        WHERE SUCCESSIVE CALLS SKIP NSKIP BITS
C        AND THE LOOP COUNT IS ITER
      ISW = 5
      GOTO 8
C
C     ++++++++++
      ENTRY SBYTES(NPACK,ISAM,IBIT,NBITS,NSKIP,ITER)
C     ++++++++++
C
C        THIS ROUTINE IS JUST A REPEATED CALL TO S_BYTE
C        WHERE SUCCESSIVE CALLS SKIP NSKIP BITS
C        AND THE LOOP COUNT IS ITER
      ISW = 6
      GO TO 8
C
C     ++++++++++
      ENTRY GBILS(NPACK,ISAM,IBIT,NBITS,NSKIP,ITER)
C     ++++++++++
C
C     GBIL(S)/SBIL(S) ENTRIES ALLOW DIRECT MAINTENANCE OF LEFT TO RIGHT
C     BIT STRINGS IN DEC ENVIRONMENT (VAX). THE MODIFICATION RETAINS FUL
C     COMPATIBILITY WITH ORIGINAL ENTRIES, HOWEVER, VERIFICATION BEYOND
C     GBIL(S)/SBIL(S) WAS VIA GBITS/SBITS ONLY (NOT GBYTES/SBYTES).
C
C     MODIFICATION PROGRAMMED BY A.D.STROUD, COMPASS SYSTEMS, INC.
C     NOVEMBER 1987 UNDER TOGA GRAND FUNDING DURING SOFTWARE DEVELOPMENT
C     FOR WMO PROPOSED STANDARD FM 94 BUFR ON THE VAX STATION II AT THE
C     OAG/NOS/NOAA, MONTEREY, CA.
C
C     FOR GBIL(S)/SBIL(S) THE FOLLOWING PARAMETER USAGE APPLIES
C
C     NPACK   - LEFT MOST(HIGHEST INDEXED) LONGWORD IN PACKED BIT STRING
C               ARRAY SPACE INFERRED BY IBIT(IN LIEU OF LOWEST INDEX FOR
C               NON-DEC, NOTE - SECOND 32 BIT SUBSTRING IN BIT STRING IS
C               NPACK(0), THIRD 32 BIT SUBSTRING IS NPACK(-1), ETC.,...)
C
C     ISAM    - ARRAY HOLDING/TO HOLD UNPACKED ELEMENT(S) (RJZF) FOR
C               SBIL(S)/GBIL(S), RESPECTIVELY.
C
C     IBIT    - INITIAL LEFT TO RIGHT BIT SKIP WITHIN THE NPACK BIT STRI
C               (INCLUDING LEFTMOST BIT OF NPACK, INTERPRET BIT SKIP THE
C                SAME AS IN NON-DEC ENVIRONMENT)
C
C     NBITS   - FIXED BIT WIDTH OF EACH ELEMENT TO BE UNPACKED.
C
C     NSKIP   - FIXED LEFT TO RIGHT BIT SKIP WIDTH BETWEEN ELEMENTS.
C
C     ITER    - NUMBER OF ELEMENTS TO UNPACK(IMPLIED MINIMUM DIMENSIONAL
C               OF ISAM ARRAY).
C
C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C     EXAMPLE
C
C  1) PARAMETER (N4=1000 , NB=4000, NL=1000)
C  2) INTEGER I4(N4),UNPACK(99)
C  3) BYTE IB(NB)
C  4) EQUIVALENCE (I4(1),IB(1))
C
C  5) READ(LUI,'(4000A1)') (IB(I),I=NB,1,-1)
C                FORMATTED READ OF ARBITRARY BITSTRING FROM NON-DEC
C                SITE, OR FROM DEC FILE AS WRITTEN AT 9) BELOW.
C
C  6) CALL GBILS(I4(NL),UNPK,67,22,47,95)
C                AFTER INITIALLY SKIPPING 67 BITS TO A POINT BEYOND THE
C                LEFTMOST THREE BITS OF I4(NL-2), GBILS ACQUIRES 95
C                SUBSTRING ELEMENTS OF THE WIDTH 22 BITS(EACH RIGHT JUST
C                WITH HIGH ORDER ZERO FILL) INTO THE RESPECTIVE FIRST
C                95 ARRAY ELEMENTS OF UNPK. SEPARATED BY 47 BITS IN SOUR
C                BITSTRING ARRAY I4, THE EXTRACTED DUPLICATE SUBSTRING
C                ELEMENTS ARE NOW EFFECTIVELY SEPARATED FROM EACH OTHER
C                BY 10 ZERO BITS IN OBJECT BITSTRING ARRAY UNPK.
C
C 7) UNPK(J)=... (J=1,95)
C
C 8) CALL SBILS(I4(NL),UNPK,67,22,47,95)
C                AFTER INITIALLY SKIPPING 67 BITS TO A POINT BEYOND THE
C                LEFTMOST THREE BITS OF I4(NL-2), SBILS STORES IN SUCCES
C                SIVE BITS THE FIRST OF 95 MODIFIED UNPK RIGHTMOST 22 BI
C                WIDTH SUBSTRING ELEMENTS. AFTER SKIPPING 47 BITS BEYOND
C                THE RIGHTMOST OF THE FIRST 22 BITS STORED, THE SECOND 2
C                BIT ELEMENT IS STORED, ETC. THE ORIGINAL SUBSTRING
C                ELEMENTS HAVE BEEN REPLACED BY MODIFIED SUBSRING ELEMEN
C                IN SITU WITHIN THE BITSTRING ARRAY I4 WITHOUT MODIFYING
C                ANY OTHER BITS.
C
C 9) WRITE(LUO,'(4000A1)') (IB(I),I=NB,1,-1)
C                FILE FOR NON-DEC OR FOR READ 5) ABOVE.
C
C
C
      ISW = 7
      GO TO 6
C
C     ++++++++++
      ENTRY SBILS(NPACK,ISAM,IBIT,NBITS,NSKIP,ITER)
C     ++++++++++
C
C
      ISW = 8
      GO TO 6
C
C     ++++++++++
      ENTRY GBIL(NPACK,ISAM,IBIT,NBITS)
C     ++++++++++
C
      ISW = 9
      GO TO 5
C
C     ++++++++++
      ENTRY SBIL(NPACK,ISAM,IBIT,NBITS)
C     ++++++++++
C
      ISW=10
 5    ITER1= 1
      NSKIP1=-NBITS
      GO TO 7
C
 6    NSKIP1=-NSKIP-NBITS
C
 7    JOFF=-IBIT-NBITS
      IF(ISW.LT.9) GO TO 9
      GO TO 10
C
 8    NSKIP1=NSKIP+NBITS
      JOFF=IBIT
C
 9    ITER1=ITER
C
C
 10   DO 30 I = 1 , ITER1
C
         IF(JOFF.GT.0) IBASE=JOFF/LENWRD+1
         IF(JOFF.LE.0) IBASE=(JOFF+1)/LENWRD+1
         IOFF=MOD(JOFF,LENWRD)
         IF(JOFF.LE.0) IOFF=MOD(IOFF+LENWRD,LENWRD)
C
         GO TO(11,12,13,14,15,16,11,12,11,12) ISW
C
11       CALL GBIT(NPACK(IBASE),ISAM(I),IOFF,NBITS)
         GO TO 20
12       CALL SBIT(NPACK(IBASE),ISAM(I),IOFF,NBITS)
         GO TO 20
13       CALL G_BYTe(NPACK(IBASE),ISAM(I),IOFF,NBITS)
         GO TO 20
14       CALL S_BYTE(NPACK(IBASE),ISAM(I),IOFF,NBITS)
         GO TO 20
15       CALL GBYTE(NPACK(IBASE),ISAM(I),IOFF,NBITS)
         GOTO 20
16       CALL SBYTE(NPACK(IBASE),ISAM(I),IOFF,NBITS)
         GOTO 20
20       JOFF = JOFF + NSKIP1
30    CONTINUE
      RETURN
      END
      SUBROUTINE GBYTE(NPACK,ISAM,IBIT,NBITS)
C
C**** GBYTE - VAX routine.
C
C     ++++++++++
C     CALL GBYTE(NPACK,ISAM,IBIT,NBITS)
C     STARTS UNPACKING BITS AT BIT OFFSET IBIT IN ARRAY
C     NPACK.  IT TAKES NBITS AND STORES THEM IN THE LONGWORD
C     ARRAY STARTING AT ISAM.
C
C     THIS ROUTINE DEALS WITH BITS AND BYTES IN THE ORDER
C     THEY ARE NUMBERED ON THE MESA (IE BIGENDIAN).
C     ----------
C
C     GBYTE AND SBYTE ARE SPECIAL ROUTINES TO DO THE SAME
C     THINGS AS THEIR COUNTERPARTS ON THE MESA.
C     THEY DIFFER FROM THE BITS ROUTINES IN THAT BITS
C     ARE COUNTED FROM THE TOP OF THE WORD RATHER THAN
C     THE BOTTOM.  THIS MEANS THAT THE BITS THEY SPECIFY JUMP AROUND
C     WHEN CROSSING BYTE BOUNDARIES.  THERE IS NO ASSUMPTION
C     MADE ABOUT THE WORDSIZE OF THE MACHINE THEY WERE WRITTEN
C     ON.  HOWEVER THE MAXIMUM NUMBER OF BITS EXTRACTED MUST BE
C     LESS THAN OR EQUAL TO 32.
C
      BYTE NPACK(8)
      INTEGER T
      EXTERNAL SS$_ABORT
      IDBIT(I) = 7 - MOD(I,8)
      ISW = 3
      ISAM = 0
      GO TO 10
      ENTRY SBYTE(NPACK,ISAM,IBIT,NBITS)
C     ++++++++++
C     CALL SBYTE(NPACK,ISAM,IBIT,NBITS)
C     THIS ROUTINE TAKES THE FIRST LONGWORD LOCATED AT
C     ISAM AND STORES IT INTO THE NBITS OF NPACK STARTING AT
C     BIT OFFSET IBIT.
C
C
C     THIS ROUTINE DEALS WITH BITS AND BYTES IN THE ORDER
C     THEY ARE NUMBERED ON THE MESA (IE BIGENDIAN).
C     ----------
      ISW = 4
10    IBASE = IBIT/8
C     IBASE POINTS TO THE FIRST BYTE INVOLVED
      IOFF = IBIT - 8*IBASE
C     IOFF IS THE BIT OFFSET WITHIN FIRST BYTE
      M = (IOFF+NBITS-1)/8 + 1
C     M IS THE NUMBER OF BYTES (INCLUDING PARTIALS) INVOLVED
C     IF(M.GT.5)THEN
      IF(NBITS.GT.32)THEN
         TYPE *,'ILLEGAL VALUE FOR NBITS IN S/GBYTE(S) CALL'
         CALL SYS$EXIT(SS$_ABORT)
      END IF
      NN = 0
C     NN IS THE NUMBER OF BITS TRANSFERRED
      DO 20 I = M , 1 , -1
C     LOOP ON BYTES
         IS = IDBIT(0)
C        CALC FIRST BIT WITHIN BYTE
         IE = IDBIT(7)
C        CALC LAST BIT WITHIN BYTE
         IF(I.EQ.1)IS = IDBIT(IOFF)
C        FIRST BIT VARIES FOR FIRST BYTE
         IF(I.EQ.M)IE = IDBIT(IOFF+NBITS-1)
C        LAST BIT VARIES FOR LAST BYTE
         N = IS - IE + 1
C        CALC NUMBER OF BITS WITHIN BYTE TO XFER
         IF(ISW.EQ.4)GO TO 12
         CALL GBIT(NPACK(IBASE+I),T,IE,N)
C        EXTRACT BITS FROM SOURCE
         CALL SBIT(ISAM,T,NN,N)
C        AND LOAD INTO DESTINATION
         GO TO 15
12       CALL GBIT(ISAM,T,NN,N)
C        EXTRACT BITS FROM SOURCE
         CALL SBIT(NPACK(IBASE+I),T,IE,N)
C        AND LOAD INTO DESTINATION
15       NN = NN + N
C        UPDATE BITS XFERRED
20    CONTINUE
      RETURN
      END
      SUBROUTINE G_BYTE(NPACK,ISAM,IBIT,NBITS)
C
C**** G_BYTE - VAX routine.
C
C     G_BYTE AND S_BYTE ARE SPECIAL ROUTINES TO DO THE SAME
C     THINGS AS THEIR COUNTERPARTS ON THE MESA.
C     THEY DIFFER FROM THE BITS ROUTINES IN THAT BITS
C     ARE COUNTED FROM THE TOP OF THE WORD RATHER THAN
C     THE BOTTOM.  THIS MEANS THAT THE BITS THEY SPECIFY JUMP AROUND
C     WHEN CROSSING BYTE BOUNDARIES.  THERE IS NO ASSUMPTION
C     MADE ABOUT THE WORDSIZE OF THE MACHINE THEY WERE WRITTEN
C     ON.  HOWEVER THE MAXIMUM NUMBER OF BITS EXTRACTED MUST BE
C     LESS THAN OR EQUAL TO 32.
C
      BYTE NPACK(8)
      INTEGER T
      EXTERNAL SS$_ABORT
      IDBIT(I) = 7 - MOD(I,8)
      ISW = 3
      ISAM = 0
      GO TO 10
      ENTRY S_BYTE(NPACK,ISAM,IBIT,NBITS)
      ISW = 4
10    IBASE = IBIT/8
C     IBASE POINTS TO THE FIRST BYTE INVOLVED
      IOFF = IBIT - 8*IBASE
C     IOFF IS THE BIT OFFSET WITHIN FIRST BYTE
      M = (IOFF+NBITS-1)/8 + 1
C     M IS THE NUMBER OF BYTES (INCLUDING PARTIALS) INVOLVED
C     IF(M.GT.5)THEN
      IF(NBITS.GT.32)THEN
         TYPE *,'ILLEGAL VALUE FOR NBITS IN S/G_BYTE(S) CALL'
         CALL SYS$EXIT(SS$_ABORT)
      END IF
      NN = 0
C     NN IS THE NUMBER OF BITS TRANSFERRED
      DO 20 I = M , 1 , -1
C     LOOP ON BYTES
         IBB = IBASE + I - 1
         IBC = IBB/4
         IBD = 4*IBC+3-MOD(IBB,4) + 1
C        CALCULATE UNSHIFTED BYTE
         IS = IDBIT(0)
C        CALC FIRST BIT WITHIN BYTE
         IE = IDBIT(7)
C        CALC LAST BIT WITHIN BYTE
         IF(I.EQ.1)IS = IDBIT(IOFF)
C        FIRST BIT VARIES FOR FIRST BYTE
         IF(I.EQ.M)IE = IDBIT(IOFF+NBITS-1)
C        LAST BIT VARIES FOR LAST BYTE
         N = IS - IE + 1
C        CALC NUMBER OF BITS WITHIN BYTE TO XFER
         IF(ISW.EQ.4)GO TO 12
C        CALL GBIT(NPACK(IBASE+I),T,IE,N)
         CALL GBIT(NPACK(IBD),T,IE,N)
C        EXTRACT BITS FROM SOURCE
         CALL SBIT(ISAM,T,NN,N)
C        AND LOAD INTO DESTINATION
         GO TO 15
12       CALL GBIT(ISAM,T,NN,N)
C        EXTRACT BITS FROM SOURCE
C        CALL SBIT(NPACK(IBASE+I),T,IE,N)
         CALL SBIT(NPACK(IBD),T,IE,N)
C        AND LOAD INTO DESTINATION
15       NN = NN + N
C        UPDATE BITS XFERRED
20    CONTINUE
      RETURN
      END
