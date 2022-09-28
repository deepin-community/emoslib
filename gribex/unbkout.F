C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C


      SUBROUTINE UNBKOUT (KUNIT,KARRAY,KINLEN,KOUTLEN,KEOF,KRET)
C
C---->
C**** UNBKOUT - Write a record to a pure binary file.
C
C     Purpose.
C     --------
C
C           Provides standard interface to write unblocked COS
C           files or UNICOS pure binary files.
C
C**   Interface.
C     ----------
C
C           CALL UNBKOUT (KUNIT,KARRAY,KINLEN,KOUTLEN,KEOF,KRET)
C
C           INTEGER    K.
C           REAL       P.
C           LOGICAL    O.
C           CHARACTER  H.
C
C               Input parameters.
C               -----------------
C
C               KUNIT      - Fortran unit number of file to be written.
C                            1 to 99.
C
C               KARRAY     - Array containing record to be written.
C
C               KINLEN     - Length of this array, in words.
C
C               KOUTLEN    - Length of record to be written (in words).
C                            If this is 0 length is assumed to be in
C                            the first word of the record.
C                            This is the case with MARS data in unpacked
C                            format.
C
C               KEOF       - 0, If not last write to this file.
C                            1, Last write to this file, so write
C                               record and flush output buffer.
C                            2, Last record already written, so
C                               flush output buffer.
C
C               KRET       - 0, Routine to abort if error encountered.
C                            1, Return to calling routine even if error.
C
C                            Adding 100 to KRET will add debug printout
C                            eg values of input and output parameters.
C
C                            Valid values for KRET are thus 0, 1, 100
C                            or 101.
C
C               Output parameters.
C               ------------------
C
C               KOUTLEN    - Length of record written, in words.
C
C               KRET       - Return code.
C                            0, No error encountered.
C                            1, Write error.
C                            2, Not used.
C                            3, Not used.
C                            4, Not used.
C                            5, No data transferred because user's array
C                               is smaller than the record length.
C                            6, Maximum number of files opened for write
C                               by user program exceeded.
C                            7, Error detected in parameters.
C
C     Method.
C     -------
C
C           Data is written in 512 word blocks. The length of each
C           record is taken from the first word (or input parameter)
C           and all full blocks are then written. Unless it is the last
C           write to the file, any remaining words are kept to use
C           as the start of the next block to be written.
C
C           An array of Fortran numbers in use is kept, with a
C           corresponding array of pointers to the next part of
C           data to be written to each one. When the last write is
C           made to a file, these entries are reset to 0.
C
C           If an error is encountered, an error message is printed.
C
C     Externals.
C     ----------
C
C           None.
C
C     Reference.
C     ----------
C
C           Cray CFT77 Manual.
C
C     Comments.
C     ---------
C
C           Maximum number of files which can be handled in one
C           program is 20
C
C           Routine contains Sections 0 to 3 and Section 9.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      24:04:90.
C
C     Modifications
C     --------------
C
C           J. Hennessy      ECMWF      31:10:90.
C           Do not write empty last block, when flushing buffer.
C
C           B. Raoult        ECMWF      29:11:90
C           Fix bug in zero-filling of short and last records.
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
C*    Section 0 . Definition of variables.
C     ----------------------------------------------------------------
C
      IMPLICIT LOGICAL     ( L, O, G )
      IMPLICIT CHARACTER*8 ( C, H, Y )
      IMPLICIT INTEGER     ( I, J, K, M, N )
C
      PARAMETER (JPBLOK=512)
      PARAMETER (JPFILS= 20)
C
      DIMENSION KARRAY(KINLEN)
      DIMENSION IOBUFF(JPFILS*JPBLOK)
      DIMENSION ISTART(JPFILS)
      DIMENSION IFTNOS(JPFILS)
C
      DATA ISTART /JPFILS*0/
      DATA IFTNOS /JPFILS*0/
C
      SAVE IOBUFF
      SAVE ISTART
      SAVE IFTNOS
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
C*    Section 1 . Check parameters. Set initial values.
C     ----------------------------------------------------------------
C
 1000 CONTINUE
C
C*    Set print switch, if required.
C
      IF (KRET.GE.100)
     C   THEN
             KPR = 1
         ELSE
             KPR = 0
         ENDIF
C
C*    Print input parameters, if required.
C
      IF (KPR.EQ.1)
     C   THEN
             WRITE (*,*) 'UNBKOUT : Section 1.'
             WRITE (*,*) '         Input values used -'
             WRITE (*,9904) KUNIT
             WRITE (*,9905) KINLEN
             WRITE (*,9907) KOUTLEN
             WRITE (*,9906) KRET
             WRITE (*,9908) KEOF
         ENDIF
C
C*    Check input parameters.
C
      IRET = 0
      IF (KRET.GE.100) KRET = KRET - 100
C
C*    Invalid value given for return code.
C
      IF (KRET.LT.0.OR.KRET.GT.1)
     C   THEN
             IRET = 7
             WRITE (*,9910) KRET
             GO TO 9000
         ENDIF
C
C*    Fortran unit numbers must be between 1 and 99 inclusive.
C
      IF (KUNIT.LT.1.OR.KUNIT.GT.99)
     C   THEN
             IRET = 7
             WRITE (*,9911) KUNIT
             GO TO 9000
         ENDIF
C
C*    Retain last write indicator.
C
      IF (KEOF.LT.0.OR.KEOF.GT.2)
     C   THEN
             IRET = 7
             WRITE (*,9916) KEOF
             GO TO 9000
         ENDIF
C
      IEOF = KEOF
      KEOF = 0
C
C*    Locate Fortran unit number in list of active numbers.
C
      DO 1010 J1010=1,JPFILS
         IF (IFTNOS(J1010).EQ.KUNIT)
     C      THEN
                IB = ISTART(J1010)
                IP = J1010
                GO TO 1030
            ENDIF
 1010 CONTINUE
C
C*    None found, so first time in. Set table entries.
C
      DO 1020 J1020=1,JPFILS
         IF (IFTNOS(J1020).EQ.0)
     C      THEN
                IB = 0
                IP = J1020
                IFTNOS(J1020) = KUNIT
                GO TO 1030
            ENDIF
 1020 CONTINUE
C
C*    No table entry free. Maximum number of files exceeded.
C
      IRET = 6
      WRITE (*,9912) JPFILS
      GO TO 9000
C
C*    Set pointer to array holding last part of data not written.
C
 1030 CONTINUE
C
      IPR = IP
      IP  =  (IP-1) * JPBLOK + 1
C
C     IOBUFF(IP) to IOBUFF(IP+B) are the remaining words after
C     last block written.
C
C*    Flush output buffer only requested.
C
      IF (IEOF.EQ.2) GO TO 3575
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
C*    Section 2 . Get length of next record.
C     ----------------------------------------------------------------
C
 2000 CONTINUE
C
      IF (KPR.EQ.1)
     C   THEN
             WRITE (*,*) 'UNBKOUT : Section 2.'
             WRITE (*,9909) IPR , IP , IB
         ENDIF
C
C     Get length of record.
C
      IF (KOUTLEN.EQ.0)
     C   THEN
             ILENW = KARRAY(1)
         ELSE
             ILENW = KOUTLEN
         ENDIF
C
C     Ensure that user's array is large enough.
C
      IF (KINLEN.LT.ILENW)
     C   THEN
             IRET = 5
             WRITE (*,9917) KINLEN , ILENW
             GO TO 9000
         ENDIF
C
      IF (KPR.EQ.1) WRITE (*,9902) ILENW
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
C*    Section 3 . Write complete blocks.
C     ----------------------------------------------------------------
C
 3000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'UNBKOUT : Section 3.'
C
C     If the record is 1 block or more in length write it to file.
C     Write partial last block into IOBUFF, preceding ones to file.
C
      IF ((IB+ILENW).GE.JPBLOK)
     C   THEN
C
C            Copy enough words from user array to complete first block.
C
             IF (IB.EQ.0) GO TO 3200
             DO 3050 J3050 = 1,JPBLOK - IB
                IOBUFF(IP+J3050+IB-1) = KARRAY(J3050)
 3050        CONTINUE
C
             IC = IB
             IB = JPBLOK - IB
C
C            Write first block.
C
             BUFFER OUT (KUNIT,0) (IOBUFF(IP),IOBUFF(IP+JPBLOK-1))
             IF (UNIT(KUNIT)) 3200,3700,3700
C
 3200        CONTINUE
C
C            Check if length to be written was exactly 1 block.
C
             IF ((ILENW + IC).EQ.JPBLOK)
     C          THEN
                    INTER = 0
                    GO TO 3555
                ENDIF
C
             ILENR = ILENW - IB
             IDIFF = MOD (ILENR,JPBLOK)
             IF (IDIFF.NE.0)
     C          THEN
                    ILENR = ILENR - IDIFF
                ELSE
                    ILENR = ILENR - JPBLOK
                ENDIF
             IE    = IB + ILENR
C
C            For a short record there may not be a full
C            second block.
C
             IF (IE.EQ.IB) GO TO 3400
C
C            If remaining part of record to be written is a multiple
C            of the block size, write complete record.
C
             IF ((ILENW-IE).EQ.JPBLOK) IE = IE + JPBLOK
C
             BUFFER OUT (KUNIT,0) (KARRAY(IB+1),KARRAY(IE))
             IF (UNIT(KUNIT)) 3400,3700,3700
C
C            Put end of field in IOBUFF.
C
 3400        CONTINUE
C
             INTER = ILENW - IE
             DO 3500 J3500=1,INTER
                IOBUFF(IP+J3500-1) = KARRAY(IE+J3500)
 3500        CONTINUE
C
         ELSE
C
C            Add short record to buffer.
C
             DO 3550 J3550 = 1,ILENW
                IOBUFF(IP+J3550+IB-1) = KARRAY(J3550)
 3550        CONTINUE
             INTER = IB + ILENW
C
         ENDIF
C
 3555 CONTINUE
C
      IB      = INTER
      KOUTLEN = ILENW
C
C     Fill unused part of buffer with zeroes.
C
      DO 3560 J3560 = IB+1,JPBLOK
         IOBUFF(J3560+IP-1) = 0
 3560 CONTINUE
C
C     Write last part of last record if end of file.
C     Do not write empty last block, when flushing buffer.
C
 3575 CONTINUE
C
      IF (IEOF.GT.0)
     C   THEN
             IF (IB.EQ.0) GO TO 3600
             DO 3580 J3580 = IB+1,JPBLOK
                IOBUFF(J3580+IP-1) = 0
 3580        CONTINUE
             BUFFER OUT (KUNIT,0) (IOBUFF(IP),IOBUFF(IP+JPBLOK-1))
             IF (UNIT(KUNIT)) 3600 , 3700 , 3700
         ENDIF
C
 3600 CONTINUE
C
C     Save pointer to start of next write area, unless last write.
C
      IF (IEOF.GT.0)
     C   THEN
             IFTNOS(IPR) = 0
             ISTART(IPR) = 0
         ELSE
             ISTART(IPR) = IB
         ENDIF
C
      GO TO 9000
C
 3700        CONTINUE
C
C*           Write error, go to section 9.
C
             IRET = 1
             WRITE (*,9913) KUNIT
C
C    ----------------------------------------------------------------
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
 9000 CONTINUE
C
      IF (KPR.EQ.1)
     C THEN
           WRITE (*,*) 'UNBKOUT : Section 9.'
           WRITE (*,*) '         Output values set -'
           WRITE (*,9907) KOUTLEN
           WRITE (*,9908) KEOF
           WRITE (*,9906) IRET
       ENDIF
C
C
      IF (IRET.NE.0.AND.IRET.NE.2)
     C   THEN
             WRITE (*,9903) IRET
             IF (KRET.EQ.0) CALL ABORT
         ENDIF
C
      KRET = IRET
C
 9902 FORMAT (1H ,'UNBKOUT : Record length (words) = ',I8)
C
 9903 FORMAT (1H ,'UNBKOUT : Return code = ',I6)
C
 9904 FORMAT (1H ,9X,'KUNIT   = ',I6)
C
 9905 FORMAT (1H ,9X,'KINLEN  = ',I6)
C
 9906 FORMAT (1H ,9X,'KRET    = ',I6)
C
 9907 FORMAT (1H ,9X,'KOUTLEN = ',I6)
C
 9908 FORMAT (1H ,9X,'KEOF    = ',I6)
C
 9909 FORMAT (1H ,'IPR = ',I4,'  IP = ',I8,'  IB = ',I8)
C
 9910 FORMAT (1H ,'UNBKOUT : KRET on input is ',I4,
     C         '. Valid values are 0,1,100 or 101.')
 9911 FORMAT (1H ,'UNBKOUT : KUNIT on input is ',I4,
     C         '. Valid values are 1 to 99.')
 9912 FORMAT (1H ,'UNBKOUT : Maximum number of files (',I2,
     C         ' exceeded.')
 9913 FORMAT (1H ,'UNBKOUT : Error in writing file number ',I2)
C
 9916 FORMAT (1H ,'UNBKOUT : KEOF on input is ',I4,
     C         '. Valid values are 0,1 or 2.')
 9917 FORMAT (1H ,'UNBKOUT : Array length is ',I6,' words, but record',
     C        ' length is ',I6,' words.')
C
      RETURN
C
      END
