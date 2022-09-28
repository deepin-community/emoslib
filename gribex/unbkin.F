C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE UNBKIN (KUNIT,KARRAY,KINLEN,KOUTLEN,KEOF,KRET)
C
C---->
C**** UNBKIN - Read a record from a pure binary file.
C
C     Purpose.
C     --------
C
C           Provides standard interface to read unblocked COS
C           files or UNICOS pure binary files.
C
C           Read next record from a pure binary file
C           and return in given array to calling routine.
C
C**   Interface.
C     ----------
C
C           CALL UNBKIN (KUNIT,KARRAY,KINLEN,KOUTLEN,KEOF,KRET)
C
C           INTEGER    K.
C           REAL       P.
C           LOGICAL    O.
C           CHARACTER  H.
C
C               Input parameters.
C               -----------------
C
C               KUNIT      - Fortran unit number of file to be read.
C                            1 to 99.
C
C               KARRAY     - Array to receive record.
C
C               KINLEN     - Length of this array, in words.
C
C               KOUTLEN    - Length of record to be read (in words)
C                            If this is 0 the length of the record is
C                            assumed to be in the first word of the
C                            record. This is the case for MARS data
C                            in unpacked format.
C
C               KEOF       - 0, If not last read from this file.
C                            1, Last read to be made from this file.
C
C                            Normally this will be 0, but if the
C                            same unit number is used later for
C                            another file, and end-of-file was not
C                            reached on the first one, a misread
C                            will occur, unless KEOF is set to 1 on
C                            the last read from the first file.
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
C               KOUTLEN    - Length of record read, in words.
C
C               KEOF       - 0, If no end of file detected.
C                            1, End of file detected.
C
C               KRET       - Return code.
C                            0, No error encountered.
C                            1, Read error.
C                            2, Not used.
C                            3, Not used.
C                            4, Not used.
C                            5, No data transferred because user's array
C                               is smaller than the record length.
C                            6, Maximum number of files opened for read
C                               by user program exceeded.
C                            7, Error detected in parameters.
C                            8, Not used.
C                            9, Not used.
C                           10, End of file encountered before end of
C                               record.
C
C     Method.
C     -------
C
C           Data is read in 512 word blocks. The length of each
C           record is given in the first word (or as input parameter)
C           and enough blocks then read to complete the record, which
C           is returned to the user. Any remaining words are kept to
C           use as the start of the next record on the next read.
C
C           An array of Fortran numbers in use is kept, with a
C           corresponding array of pointers to the last part of
C           data read from each one. A similar array keeps end of file
C           status for each file. When the last read has been
C           made from a file, these entries are reset to 0.
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
C           None.
C
C     Comments.
C     ---------
C
C           Maximum number of files which can be handled in one
C           program is 20.
C
C           Routine contains Sections 0 to 3 and Section 9.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      25:06:90.
C
C     Modifications
C     --------------
C
C           None.
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
      DIMENSION INBUFF(JPFILS*JPBLOK)
      DIMENSION ISTART(JPFILS)
      DIMENSION IFTNOS(JPFILS)
      DIMENSION IATEOF(JPFILS)
C
      DATA ISTART /JPFILS*0/
      DATA IFTNOS /JPFILS*0/
      DATA IATEOF /JPFILS*0/
C
      SAVE INBUFF
      SAVE ISTART
      SAVE IFTNOS
      SAVE IATEOF
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
C     Set print switch, if required.
C
      IF (KRET.GE.100)
     C   THEN
             KPR = 1
         ELSE
             KPR = 0
         ENDIF
C
C     Print input parameters, if required.
C
      IF (KPR.EQ.1)
     C   THEN
             WRITE (*,*) 'UNBKIN : Section 1.'
             WRITE (*,*) '         Input values used -'
             WRITE (*,9904) KUNIT
             WRITE (*,9905) KINLEN
             WRITE (*,9906) KRET
             WRITE (*,9908) KEOF
         ENDIF
C
C     Check input parameters.
C
      IRET = 0
      IF (KRET.GE.100) KRET = KRET - 100
C
C     Invalid value given for return code.
C
      IF (KRET.LT.0.OR.KRET.GT.1)
     C   THEN
             IRET = 7
             WRITE (*,9910) KRET
             GO TO 9000
         ENDIF
C
C     Fortran unit numbers must be between 1 and 99 inclusive.
C
      IF (KUNIT.LT.1.OR.KUNIT.GT.99)
     C   THEN
             IRET = 7
             WRITE (*,9911) KUNIT
             GO TO 9000
         ENDIF
C
C     Keep last read indicator.
C
      IEOF = KEOF
      KEOF = 0
C
C     Locate Fortran unit number in list of active numbers.
C
      DO 1010 J1010=1,JPFILS
         IF (IFTNOS(J1010).EQ.KUNIT)
     C      THEN
                IB = ISTART(J1010)
                IZ = IATEOF(J1010)
                IP = J1010
                GO TO 1030
            ENDIF
 1010 CONTINUE
C
C     None found, so first time in. Set table entries.
C
      DO 1020 J1020=1,JPFILS
         IF (IFTNOS(J1020).EQ.0)
     C      THEN
                IB = 0
                IZ = 0
                IP = J1020
                IFTNOS(J1020) = KUNIT
                GO TO 1030
            ENDIF
 1020 CONTINUE
C
C     No table entry free. Maximum number of files exceeded.
C
      IRET = 6
      WRITE (*,9912) JPFILS
      GO TO 9000
C
C     Set pointer to array holding last part of data read.
C
 1030 CONTINUE
C
      IPR = IP
      IP  =  (IP-1) * JPBLOK + 1
C
C     INBUFF(IP) to INBUFF(IP+IB) are the remaining words of the
C     last block read.
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
C*    Section 2 . Ensure enough data is read to include a full record.
C     ----------------------------------------------------------------
C
 2000 CONTINUE
C
      IF (KPR.EQ.1)
     C   THEN
             WRITE (*,*) 'UNBKIN : Section 2.'
             WRITE (*,9909) IPR , IP , IB
         ENDIF
C
C     Copy remainder of last block read to user array.
C
C     KARRAY   1      IB
C              !       !
C              =========
C
      DO 2025 J2025 = 1,IB
         KARRAY(J2025) = INBUFF(IP+J2025-1)
 2025 CONTINUE
C
C     Fill remainder of array with zeroes.
C
      DO 2050 J2050 = IB+1,KINLEN
         KARRAY(J2050) = 0
 2050 CONTINUE
C
C
C*    Get length of record to be read.   .
C
      IF (KOUTLEN.NE.0)
     C   THEN
             ILENW = KOUTLEN
         ELSE
             ILENW = KARRAY(1)
         ENDIF
C
C     Force a block to be read if no data remains from previous
C     read.
C
      IF (IB.EQ.0)            ILENW = JPBLOK + 1
C
C     Force a block to be read if data length is 0 to check for
C     end-of-file.
C
      IF (ILENW.EQ.0)         ILENW = JPBLOK + 1
C
C     Force a block to be read if the record Length is
C     greater than the length of data already read.
C
      IF (ILENW.GT.IB)        ILENW = JPBLOK + 1
C
C     No data is read if full record has already been read.
C
C     No data is read if an end-of-file has been
C     encountered on previous read. Even if end-of-file has been
C     reached there may be short record(s) remaining in the
C     last block read.
C
C     KARRAY   1      IB            IE
C              !       !             !
C              =======================
C
C     Read next block if necessary to get full record.
C
      IF (IZ.EQ.0.AND.ILENW.GT.JPBLOK)
     C   THEN
             IE = IB + JPBLOK
             BUFFER IN (KUNIT,0) (KARRAY(IB+1),KARRAY(IE))
             IF (UNIT(KUNIT)) 2300,2200,2100
         ELSE
             IE = IB
             GO TO 2300
         ENDIF
C
 2100 CONTINUE
C
C     Read error, go to section 9.
C
      IRET = 1
      WRITE (*,9913) KUNIT
      GO TO 9000
C
 2200 CONTINUE
C
C     End of file read, so set flag. Last block read may still
C     contain short records.
C
      IZ          = 1
      IATEOF(IPR) = IZ
C
 2300 CONTINUE
C
C*    Get length of record.
C
      IF (KOUTLEN.NE.0)
     C   THEN
             ILENW = KOUTLEN
C
C            If the record length has been set by user, and
C            end-of-file has been encountered, force end-of-file
C            processing if record length is greater than remaining
C            contents of the buffer.
C
             IF(IZ.EQ.1.AND.ILENW.GT.IB) ILENW = 0
C
         ELSE
             ILENW = KARRAY(1)
         ENDIF
C
C        If end-of-file has been encountered and record
C        length is 0 no further record remains in the buffer.
C
         IF (IZ.EQ.1.AND.ILENW.EQ.0)
     C      THEN
                KEOF = 1
                IFTNOS(IPR) = 0
                ISTART(IPR) = 0
                IATEOF(IPR) = 0
                GO TO 9000
            ENDIF
C
      IF (KPR.EQ.1) WRITE (*,9902) ILENW
C
C     Ensure that user's array is large enough.
C
      IF (KINLEN.LT.ILENW)
     C   THEN
             WRITE (*,9917) KINLEN , ILENW
             IRET = 5
             GO TO 9000
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
C*    Section 3 . Read enough blocks to complete current record.
C     ----------------------------------------------------------------
C
 3000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'UNBKIN : Section 3.'
C
C
C     If data already read does not contain a full record,
C     read enough blocks to include the next full record.
C     Read last block into INBUFF, preceding ones into user's array.
C
      IF (IE.LT.ILENW)
     C   THEN
C
C            KARRAY   1      IB            IE                ILENW
C                     !       !             !                    !
C                     ==============================================
C                                                                  !
C                                                                ILENR
             IB    = IE
             ILENR = ILENW - IE
C
C            If second block is last block, do not read into user array.
C
             IF (ILENR.LE.JPBLOK) GO TO 3010
C
             IDIFF = MOD (ILENR,JPBLOK)
             IF (IDIFF.NE.0)
     C          THEN
                    ILENR = ILENR - IDIFF
                ELSE
                    ILENR = ILENR - JPBLOK
                ENDIF
             IE    = IE + ILENR
             BUFFER IN (KUNIT,0) (KARRAY(IB+1),KARRAY(IE))
             IF (UNIT(KUNIT)) 3010,3700,2100
C
 3010 CONTINUE
C
             BUFFER IN (KUNIT,0) (INBUFF(IP),INBUFF(IP+JPBLOK-1))
             IF (UNIT(KUNIT)) 3100,3700,2100
C
C            Put end of field in user's array.
C
 3100        CONTINUE
C
             IEND = ILENW - IE
             DO 3200 J3200=1,IEND
                KARRAY(IE+J3200) = INBUFF(IP+J3200-1)
 3200        CONTINUE
C
C            Move remainder of block to front of buffer.
C
             INTER = JPBLOK - IEND
             DO 3300 J3300=1,INTER
                INBUFF(IP+J3300-1) = INBUFF(IP+J3300-1+IEND)
 3300        CONTINUE
C
C
         ELSE
C
C            Check for premature end-of-file ie record
C            longer than data already read.
C
C            KARRAY   1        IB          ILENW
C                     !         !    EOF    !
C                     ===========
C
C===         IF (IZ.EQ.1.AND.ILENW.GT.IB) GO TO 3700
C            RE-implement above line and remove next IF clause
C            when ECFILE clears its buffers before use.
C
             IF (IZ.EQ.1.AND.ILENW.GT.IB)
     C          THEN
                    KEOF = 1
                    IFTNOS(IPR) = 0
                    ISTART(IPR) = 0
                    IATEOF(IPR) = 0
                    KOUTLEN     = 0
                    GO TO 9000
                 ENDIF
C
C            KARRAY   1           ILENW    IE
C                     !              !      !
C                     =======================
C
             INTER = IE - ILENW
             DO 3400 J3400=1,INTER
                INBUFF(IP+J3400-1) = KARRAY(ILENW+J3400)
 3400        CONTINUE
         ENDIF
C
      KOUTLEN = ILENW
      IB      = INTER
C
 3600 CONTINUE
C
C     Save pointer to start of next read area, unless last read.
C
      IF (IEOF.EQ.1)
     C   THEN
             IFTNOS(IPR) = 0
             ISTART(IPR) = 0
             IATEOF(IPR) = 0
         ELSE
             ISTART(IPR) = IB
             IATEOF(IPR) = IZ
         ENDIF
C
      GO TO 9000
C
C     End-of-file before end of record.
C
 3700 CONTINUE
C
      IRET = 10
      WRITE (*,9918)
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
           WRITE (*,*) 'UNBKIN : Section 9.'
           WRITE (*,*) '         Output values set -'
           WRITE (*,9907) KOUTLEN
           WRITE (*,9908) KEOF
           WRITE (*,9906) IRET
       ENDIF
C
C     Error encountered. Clear table entries, if any exist.
C     Abort if requested.
C
      IF (IRET.NE.0.AND.IRET.NE.2)
     C   THEN
             IF (IRET.NE.6)
     C          THEN
                    IFTNOS(IPR) = 0
                    IATEOF(IPR) = 0
                    ISTART(IPR) = 0
                ENDIF
             WRITE (*,9903) IRET
             IF (KRET.EQ.0) CALL ABORT
         ENDIF
C
      KRET = IRET
C
      RETURN
C
C
 9902 FORMAT (1H ,'UNBKIN : Record length (words) = ',I8)
C
 9903 FORMAT (1H ,'UNBKIN : Return code = ',I6)
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
 9910 FORMAT (1H ,'UNBKIN : KRET on input is ',I4,
     C        '. Valid values are 0,1,10,11,100,110,101 or 111.')
 9911 FORMAT (1H ,'UNBKIN : KUNIT on input is ',I4,
     C        '. Valid values are 1 to 99.')
 9912 FORMAT (1H ,'UNBKIN : Maximum number of files (',I2,') exceeded.')
C
 9913 FORMAT (1H ,'UNBKIN : Error in reading file number ',I3)
C
 9917 FORMAT (1H ,'UNBKIN : Array length is ',I6, 'words, but record ',
     C        'length is ',I6,' words.')
 9918 FORMAT (1H ,'UNBKIN : End-of-file reached before end of record.')
C
C
      END
