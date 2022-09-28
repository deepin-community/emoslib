C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE SORTF (UN1,UN2,IRET)
C
C---->
C**********************************************************************
C*                                                                    *
C*    NAME      : SORTF                                               *
C*                                                                    *
C*    FUNCTION  : SORTS FIELDS IN A FILE IN ASCENDING ORDER           *
C*                                                                    *
C*    INPUT     : UN1   = INPUT  LUN                                  *
C*                UN2   = OUTPUT LUN                                  *
C*                                                                    *
C*    OUTPUT    : IRET  = -10    INSUFFICENT BUFFER LENGTH            *
C*                      = -100   BUFFER IN ERROR                      *
C*                      = -200   BUFFER OUT ERROR                     *
C*                      = -300   UNRECOGNISED LEVEL TYPE.
C*                      = -400   LEVEL SEQUENCE ERROR.
C*                      > 0      NO. OF FIELDS WRITTEN                *
C*                      < 0      ERROR CODES FROM SUBROUTINES.        *
C*                                                                    *
C*    GENERAL   : SORTF CALLS RECLEN                                  *
C*                            SORT                                    *
C*                                                                    *
C*    AUTHOR    : B.V. GURETZKY   16.07.85                            *
C*                                                                    *
C*    MOFIFIED  : J. HENNESSY     26.11.85                            *
C                 J. HENNESSY     23.03.87
C*                                                                    *
C**********************************************************************
C----<
C
      IMPLICIT INTEGER (B,R,U)
      PARAMETER (BLEN=500000)
      DIMENSION BUF(BLEN)
C
C     NO OF BITS  PER WORD
      DATA NBIT /64/
C
      DATA IPR /1/
C
C
      IRET               = 0
      IB                 = 0
      LEN                = 0
      NOF                = -1
      RECNO              = 0
      ICODE              = 0
      INTER              = 0



C      SKIP TO END OF FILE AND CHECK LAST FIELD.
C
      DO 2000 J = 1,10000
         BUFFER IN (UN1,0) (BUF(1),BUF(BLEN))
         IUNIT = UNIT(UN1)
         IF (UNIT(UN1)) 1000,2500,10000
 1000    CONTINUE
         RECNO = RECNO + 1
 2000 CONTINUE
C
C     IF LAST FIELD IS LN SURFACE PRESSURE OR BUDGET VALUE
C     RECORD, WRITE TO OUTPUT FILE.
C
 2500 CALL RECLEN (BUF(1),LEN,NBIT,ICODE,INDIC,LEVEL,JERR)
      IF (JERR.NE.0)
     C   THEN
             IRET = JERR
             RETURN
         ENDIF
C
C     IDENTIFY LEVEL TYPE FOR SEQUENCE CHECKING PURPOSES.
C
C     SURFACE DATA.
C
      IF (INDIC.EQ.1)
     C   THEN
             INTER = 0
             ITYPE = 0
             GO TO 2600
         ENDIF
C
C     PRESSURE LEVEL DATA.
C
      IF (INDIC.EQ.100)
     C   THEN
             INTER = 9999
             ITYPE = 1
             GO TO 2600
         ENDIF
C
C     MODEL LEVEL DATA.
C
      IF (INDIC.EQ.109)
     C   THEN
             INTER = 0
             ITYPE = 2
             GO TO 2600
         ENDIF
C
C     INVALID LEVEL TYPE.
C
      IRET = -300
      WRITE (*,9001) INDIC
 9001 FORMAT (1H ,'SORTF : INVALID LEVEL TYPE - ',I3)
      RETURN
C
 2600 CONTINUE
C
      IF (ICODE.EQ.152.OR.ICODE.EQ.128)
     C   THEN
             IRET        = IRET   +  1
             LEN         = LEN    /  8
             RECNO       = RECNO  -  1
             BUFFER OUT (UN2,0) (BUF(1),BUF(LEN))
             IUNIT       = UNIT(UN2)
             IF (IPR.EQ.1) WRITE (*,9101) LEVEL,ICODE
 9101        FORMAT (1H ,'LEVEL ',I5,' FIELD ',I5)
             IF (IUNIT.GT.0) GO TO 10500
         ENDIF
C
      REWIND UN1
C
C     START AGAIN AT BEGINNING OF FILE FOR OTHER FIELDS.
C
      DO 5000 J = 1,RECNO
C
C        READ AS MANY FIELDS AS POSSIBLE INTO MEMORY.
C
         LEVEL = INTER
         BUFFER IN (UN1,0) (BUF(IB+1),BUF(BLEN))
         IUNIT = UNIT(UN1)
         IF (UNIT(UN1)) 3000,5500,10000
 3000    CONTINUE
         NOF = NOF + 1
C
C        IF LEVEL CHANGES THEN SORT PREVIOUS FIELDS.
C
         CALL RECLEN (BUF(IB+1),LEN,NBIT,ICODE,INDIC,INTER,JERR)
            IF (JERR.NE.0)
     C         THEN
                   IRET = JERR
                   RETURN
               ENDIF
C
         LEN = LENGTH(UN1)
         IF ((INTER.NE.LEVEL).AND.(J.GT.1))
     C      THEN
C
C               FIRST CHECK THAT LEVEL SEQUENCE IS CORRECT.
C
                IF ( (ITYPE.EQ.1.AND.INTER.GT.LEVEL) .OR.
     C               (ITYPE.EQ.2.AND.INTER.LT.LEVEL) )
     C             THEN
                       IRET = - 400
                       WRITE (*,9002)
 9002                  FORMAT (1H ,'SORTF : LEVEL SEQUENCE ERROR.')
                       RETURN
                   ENDIF
C
C               SORT FIELDS
C
                CALL SORT (BUF(1),NOF,UN2,IRET,NBIT)
                IF (IRET.LE.0) RETURN
C
C               MOVE LAST FIELD TO BUFFER START
C
                NOF = 0
                DO 4000 K = 1,LEN
                   BUF(K) = BUF(IB+K)
 4000           CONTINUE
                IB = 0
C
            ENDIF
C
C        CHECK BUFFER LIMITS
C
         IB = IB + LEN
         IF (IB+LEN.GT.BLEN)
     C      THEN
                IRET = -10
                WRITE (*,9000) J
 9000           FORMAT (' SORTF : NOT ENOUGH MEMORY FOR FIELD :',I4.4)
                RETURN
            ENDIF
C
C
 5000 CONTINUE
C
C     SORT LAST SET OF FIELDS.
C
 5500 NOF = NOF + 1
      CALL SORT (BUF(1),NOF,UN2,IRET,NBIT)
C
      RETURN
C
10000 CONTINUE
      IRET = -100
      WRITE (*,*) ' SORTF : BUFFER IN ERROR'
      RETURN
C
10500 CONTINUE
      IRET = -200
      WRITE (*,*) ' SORTF : BUFFER OUT ERROR'
      RETURN
C
      END
