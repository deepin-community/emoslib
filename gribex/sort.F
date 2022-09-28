C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE SORT (BLOCK,NOF,UN,IRETR,NBIT)
C
C---->
C**********************************************************************
C*                                                                    *
C*    NAME      : SORT                                                *
C*                                                                    *
C*    FUNCTION  : SORTS AN ARRAY IN ASCENDING FIELD CODE-NUMBER       *
C*                AND WRITES THE FIELD TO GIVEN UNIT NUMBER.          *
C*                                                                    *
C*    INPUT     : BLOCK = INPUT ARRAY CONTAINING FIELDS.              *
C*                NOF   = NO OF FIELDS                                *
C*                UN    = OUTPUT FILE FOR SORTED FIELDS.              *
C*                NBIT  = NO OF BITS PER WORD                         *
C*                                                                    *
C*    OUTPUT    : IRETR  >  0     NO. OF RECORDS WRITTEN              *
C*                       <  0     ERRORS REPORTED BY SUBROUTINES      *
C*                       = -200   BUFFER OUT ERROR                    *
C*                BLOCK - RECORDS ARE ALTERED . FIELD-CODE OCTETS     *
C*                        ARE SET TO 0 !!!!!                          *
C*                                                                    *
C*    GENERAL   : SORT    CALLS RECLEN                                *
C*                              SBYTE                                 *
C*                                                                    *
C*    AUTHOR    : B.V. GURETZKY  V.E. DAY                             *
C*                                                                    *
C*    MODIFIED  : J. HENNESSY    26.11.85                             *
C*                                                                    *
C**********************************************************************
C----<
C
      IMPLICIT INTEGER (B,U)
      DIMENSION BLOCK(*)
C
      DATA IPR /1/
C
C     RETURN IF NUMBER OF FIELDS NOT MORE THAN 0.
C
      IF (NOF.LE.0) RETURN
C
C     ONLY 1 FIELD SUPPLIED
C
      IF (NOF.EQ.1)
     C  THEN
            CALL RECLEN (BLOCK(1),LEN,NBIT,ICODE,INDIC,LEVEL,JERR)
            IF (JERR.NE.0)
     C         THEN
                   IRETR = JERR
                   RETURN
               ENDIF
            LEN = LEN / 8
            BUFFER OUT (UN,0) (BLOCK(1),BLOCK(LEN))
            IUNIT = UNIT(UN)
            IF (IUNIT.GT.0) GO TO 10000
            IRETR = IRETR + 1
            RETURN
        ENDIF
C
C
C     LOOP THROUGH FIELDS SUPPLIED TO FIND THE ONE WITH
C     THE NEXT HIGHEST FIELD CODE.
C
      DO 2000 I = 1,NOF
C
         IB              = 0
         LENEXTR         = 0
         IOFFEXT         = 0
         ICODEXT         = 999
C
         DO 1000 J = 1,NOF
C
C           GET LENGTH OF FIELD RECORD.
C
            CALL RECLEN  (BLOCK(IB+1),LEN,NBIT,ICODE,INDIC,LEVEL,JERR)
            IF (JERR.NE.0)
     C         THEN
                   IRETR = JERR
                   RETURN
               ENDIF
            LEN = LEN / 8
C
            IF ((ICODE.LT.ICODEXT).AND.(ICODE.NE.0))
     C         THEN
                   LENEXTR   = LEN
                   IOFFEXT   = IB
                   ICODEXT   = ICODE
               ENDIF
            IB = IB + LEN
C
 1000    CONTINUE
C
C        WRITE FIELD
C
         LEN = IOFFEXT + LENEXTR
         BUFFER OUT(UN,0) (BLOCK(IOFFEXT+1),BLOCK(LEN))
         CALL RECLEN (BLOCK(IOFFEXT+1),LEN,NBIT,ICODE,INDIC,LEVEL,JERR)
            IF (JERR.NE.0)
     C         THEN
                   IRETR = JERR
                   RETURN
               ENDIF
         IUNIT = UNIT(UN)
         IF (IPR.EQ.1) WRITE (*,9101) LEVEL,ICODE
 9101    FORMAT (1H ,'LEVEL ',I5,' FIELD ',I5)
         IF (ICODE.LT.127.OR.ICODE.GT.255)
     C      THEN
                WRITE (*,*) 'SORT : INVALID FIELD CODE.'
                CALL ABORT
            ENDIF
C
         IF (IUNIT.GT.0) GO TO 10000
C
C        SET  CODE-TYPE TO ZERO TO INDICATE
C        THAT FIELD HAS ALREADY BEEN WRITTEN
C
         IZERO = 0
         CALL SBYTE (BLOCK(IOFFEXT+2),IZERO,32,8)
         IRETR = IRETR + 1

 2000 CONTINUE

      RETURN
10000 CONTINUE

      IRETR = -200
      WRITE (*,*) ' SORT : BUFFER OUT ERROR.'
C
      RETURN
C
      END
