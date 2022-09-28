C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

#ifdef CRAY
      SUBROUTINE BLCKCR (UN1,UN2,BUF,IRET)
C
C---->
C**********************************************************************
C*                                                                    *
C*    NAME      : BLCKCR
C*                                                                    *
C*    FUNCTION  : CONVERTS AN UNBLOCKED DATASET                       *
C*                INTO A BLOCKED DATASET                              *
C*                                                                    *
C*    INPUT     : UN1   = INPUT  LUN                                  *
C*                UN2   = OUTPUT LUN                                  *
C*                BUF   = WORK AREA
C*                                                                    *
C*    OUTPUT    : IRET  = -100   BUFFER IN  ERROR                     *
C*                      = -200   BUFFER OUT ERROR                     *
C*                      > 0      NO. OF RECORDS WRITTEN               *
C*                      < 0      ERRORS REPORTED BY SUBROUTINES       *
C*                                                                    *
C*    GENERAL   : BLCKCR  CALLS  RECLEN                               *
C*                               OFFSET                               *
C*                               MODVAL                               *
C*                                                                    *
C*    AUTHOR    : B.V. GURETZKY     V.E.  DAY                         *
C*                                                                    *
C*    MODIFIED  : J. HENNESSY       26.11.85                          *
C*                      "           24.06.86
C*                      "           19.08.86
C*                                                                    *
C**********************************************************************
C----<
C
      IMPLICIT INTEGER (B,U)
      DIMENSION BUF(*)
C
C     NO. OF BITS PER CRAY WORD
      DATA NBIT /64/
C
C     BLOCK LENGTH FOR CRAY FILES (ICB) = 512
      DATA ICB /512/
C
C
      IRET = 0
      IRES = 0
      INT1 = 0
      IOFF = 0
      IPR  = 1

C     READ FIRST BLOCK AND GET LENGTH OF FIRST DATA RECORD.
C
      BUFFER IN (UN1,0) (BUF(1),BUF(ICB))
      IF (UNIT(UN1)) 100,5500,10000
C
  100 CALL RECLEN (BUF(1),LEN,NBIT,ICODE,INDIC,LEVEL,KERR)
      IF (KERR.NE.0)
     C   THEN
             IRET = KERR
             RETURN
         ENDIF
C
C     PRINT RECORD LENGTH , IF REQUIRED.
C
      IF (IPR.EQ.1) WRITE (*,9111) LEN
 9111 FORMAT (1H ,'BLCKCR : RECORD LENGTH (BYTES) = ',I9)
C     MAKE LENGTH MULTIPLE OF ICB
C
      LENW = LEN / 8
      CALL MODVAL (LENW,IE,ICB)
      IB = ICB
C
      DO 5000 J = 1,10000
C
C        READ ENOUGH BLOCKS TO INCLUDE REMAINDER OF CURRENT
C        DATA RECORD.
C
         IF (IPR.EQ.1) WRITE (*,9112) IB , IE
 9112 FORMAT (1H ,'BLCKCR : IB = ',I8,'   IE = ',I8)
         IF (IB.NE.IE)
     C      THEN
                BUFFER IN (UN1,0) (BUF(IB+1),BUF(IE))
                IOSTAT = UNIT(UN1)
                IF (IPR.EQ.1) WRITE (*,7878) IOSTAT
 7878           FORMAT (1H ,'IOSTAT = ',I4)
                IF (UNIT(UN1)) 1000,5500,10000
            ENDIF
C
C        WRITE FIELD IN UNBLOCKED FORMAT
C
 1000    BUFFER OUT (UN2,0) (BUF(1),BUF(LENW))
         IOSTAT = UNIT(UN2)
         IF (IOSTAT.GT.0) GO TO 10100
         IRET = IRET + 1
C
C        MOVE REMAINDER OF BLOCK TO FRONT OF BUFFER.
C
         INTER = IE - LENW
         DO 2000 K = 1,INTER
            BUF(K) = BUF(LENW+K)
 2000    CONTINUE
         IB = INTER
C
C        READ NEXT BLOCK TO CHECK LENGTH OF NEXT RECORD
C        AND END-OF-FILE
C
         IF (INTER.LT.ICB)
     C      THEN
                BUFFER IN (UN1,0) (BUF(INTER+1),BUF(INTER+ICB))
                IF (UNIT(UN1)) 3000,2500,10000
 2500           CONTINUE
                BUFFER IN (UN1,0) (BUF(INTER+1),BUF(INTER+ICB))
                IF (UNIT(UN1)) 3000,5500,10000
 3000           CONTINUE
                IB = IB + ICB
            ENDIF
C
C        GET  LENGTH FOR NEXT RECORD.
C
         CALL RECLEN (BUF(1),LEN,NBIT,ICODE,INDIC,LEVEL,KERR)
         IF (KERR.NE.0)
     C      THEN
                IRET = KERR
                RETURN
            ENDIF
C
         LENW = LEN / 8
C
C        MAKE LENGTH MULTIPLE OF ICB
C
         INT = LENW - IB
         CALL MODVAL (INT,IE,ICB)
C
C        READ OF SHORT RECORDS MAY BE COMPLETE.
C
         IF (INT.LT.0) IE = 0
         IE = IB + IE

 5000 CONTINUE

 5500 CONTINUE

      RETURN
C
10000 CONTINUE
      IRET               = -100
      WRITE (*,*) ' BLCKCR : BUFFER IN ERROR'
      RETURN
C
10100 CONTINUE
      IRET               = -200
      WRITE (*,*) ' BLCKCR : BUFFER OUT ERROR'
      RETURN
C
      END
#endif
