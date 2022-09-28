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
C           IBM version of routine.
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
C           SYSABN
C
C     Reference.
C     ----------
C
C           None.
C
C     Comments.
C     ---------
C
C           IBM version of routine.
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
      CALL SYSABN (1)
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
C           IBM version of routine.
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
      KNEG = -2147483647
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
