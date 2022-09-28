C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE RECLEN  (BLOCK,LEN,NBIT,ICODE,INDIC,LEVEL,IERR)
C
C---->
C*                                                                    *
C*    NAME      : RECLEN
C*                                                                    *
C*    FUNCTION  : CALCULATES FROM THE GRIB (OR PSEUDO GRIB) HEADERS   *
C*                THE TOTAL LENGTH OF THE RECORD, AND ALSO RETURNS    *
C*                PARAMETER NUMBER, INDICATOR OF TYPE OF LEVEL AND    *
C*                LEVEL NUMBER.                                       *
C*                                                                    *
C*    INPUT     : BLOCK = INPUT ARRAY                                 *
C*                NBIT  = NO OF BITS PER COMPUTER WORD                *
C*                                                                    *
C*    OUTPUT    : LEN   = LENGTH IN OCTETS                            *
C*                ICODE = PARAMETER NUMBER                            *
C*                INDIC = LEVEL TYPE INDICATOR                        *
C*                LEVEL = LEVEL NUMBER                                *
C*                IERR  = -1 , UNDEFINED BLOCK 3 INCLUDED.            *
C*                      = -2 , ERROR DETECTED BY SUBROUTINE OFFSET.   *
C                       = -3 , Section length of 0 found.
C*                      =  0 , NO ERROR.                              *
C*                                                                    *
C*    GENERAL   : RECLEN CALLS GBYTES                                 *
C*                             OFFSET                                 *
C*                             MODVAL                                 *
C*                                                                    *
C*    AUTHOR    : B.V. GURETZKY  03.05.85                             *
C*                                                                    *
C*    MODIFIED  : J. HENNESSY    25.11.85                             *
C                 J. Hennessy    11.11.91
C                 Checks on section lengths of 0 added.
C*                                                                    *
C     ---------------------------------------------------------------
C----<
C
      IMPLICIT  INTEGER(B,U)
      DIMENSION BLOCK(*)
      DIMENSION IBLOCK(24)
C
C     SKIP PAST LEADING 4 ASCII CHARACTERS
C
      IWORD = 1
      IOFF  = 0
      IBYTE = 8
      NVAL  = 4
      ILENG = 200
      IERR  = 0
C
      CALL OFFSET (IOFF,NVAL,IWORD,IBYTE,NBIT,ILENG,IERR)
      IF (IERR.NE.0) RETURN
C
C     EXTRACT FIRST 4 OCTETS OF BLOCK 1.
C
      CALL GBYTES (BLOCK(IWORD),IBLOCK(1),IOFF,IBYTE,0,NVAL)
      CALL OFFSET (IOFF,NVAL,IWORD,IBYTE,NBIT,ILENG,IERR)
      IF (IERR.NE.0) RETURN
C
C     IF THESE OCTETS DO NOT CONTAIN 0, 0, 24 AND 0 RESPECTIVELY
C     THEN THE DATA IS IN THE OLD VERSION OF THE CODE.
C
      IF (IBLOCK(1).EQ.0.AND.IBLOCK(2).EQ.0.AND.IBLOCK(3).EQ.24.
     C    AND.IBLOCK(4).EQ.0)
     C      THEN
                ISNEW = 1
                NVAL = 20
            ELSE
                ISNEW = 0
                NVAL = 16
            ENDIF
C
C     EXTRACT NEXT 16 OR 20 OCTETS OF BLOCK 1 - NUMBER DEPENDS
C     ON VERSION OF CODE.
C
      CALL GBYTES (BLOCK(IWORD),IBLOCK(5),IOFF,IBYTE,0,NVAL)
      CALL OFFSET (IOFF,NVAL,IWORD,IBYTE,NBIT,ILENG,IERR)
      IF (IERR.NE.0) RETURN
C
C     LENGTH OF BLOCK 0 + BLOCK 1
      LEN = 24 + (ISNEW*4)
C
C
C     PARAMETER NUMBER.
C
      ICODE = IBLOCK(5+4*ISNEW)
C
C     LEVEL TYPE
C
      INDIC = IBLOCK(6+4*ISNEW)
C
C     LEVEL DESCRIPTION
C
      KTEMP = 32 + ISNEW*224
C
      IF (INDIC.EQ.100.OR.INDIC.EQ.103.OR.INDIC.EQ.105.
     C   .OR.INDIC.EQ.107.OR.INDIC.EQ.109)
     C     THEN
               LEVEL = IBLOCK(7+ISNEW*4) * KTEMP + IBLOCK(8+ISNEW*4)
           ELSE
               LEVEL = IBLOCK(7+ISNEW*4)
           ENDIF
C
C     CHECK FOR PRESENCE OF BLOCK 2.
C
      IFLAG = IBLOCK(4+4*ISNEW)
C
      INC = 0
      IF (ISNEW.EQ.0)
     C   THEN
             IF (IFLAG.EQ.1.OR.IFLAG.EQ.3) INC = 1
         ELSE
             IF (IFLAG.EQ.128.OR.IFLAG.EQ.192) INC = 1
         ENDIF
C
C     LENGTH OF GRID DEFINITION BLOCK
C
      IF (INC.EQ.1)
     C   THEN
             CALL GBYTE (BLOCK(IWORD),LBLK,IOFF,24)
             LEN = LEN + LBLK
             IF (LBLK.EQ.0)
     C          THEN
                    IERR = -3
                    LEN  = 0
                    WRITE (*,9001)
                    RETURN
                ENDIF
C
             CALL  OFFSET (IOFF,LBLK,IWORD,8,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
         ENDIF
C
C     CHECK FOR PRESENCE OF BLOCK 3
C
      INC = 0
      IF (ISNEW.EQ.0)
     C   THEN
             IF (IFLAG.EQ.2.OR.IFLAG.EQ.3) INC = 1
         ELSE
             IF (IFLAG.EQ.64.OR.IFLAG.EQ.192) INC = 1
         ENDIF
C
C
C------------------------------------------
C
C     NMC WASHINGTON SEA SURFACE TEMPERATURE ANALYSIS IS THE ONLY
C     GRIB CODED FIELD WITH BLOCK 3. IT ALSO VARIES IN LENGTH SO
C     THE LENGTH IS FIXED FOR ARCHIVAL/RETRIEVAL PURPOSES TO
C     34920 BYTES.
C
      IF (INC.EQ.1)
     C   THEN
             LEN = 34920
             RETURN
         ENDIF
C
C------------------------------------------
C
C
      IF (INC.EQ.1)
     C    THEN
              IERR = -1
              WRITE (*,*) ' RECLEN : BLOCK 3 OF GRIB CODE NOT DEFINED'
              RETURN
          ENDIF
C
C     LENGTH OF BINARY DATA BLOCK
C
      CALL GBYTE (BLOCK(IWORD),LBLK,IOFF,24)
      LEN = LEN + LBLK
C
             IF (LBLK.EQ.0)
     C          THEN
                    IERR = -3
                    LEN  = 0
                    WRITE (*,9002)
                    RETURN
                ENDIF
C
C     BLOCK 5 HAS FIXED LENGTH OF 4 OCTETS.

      LEN = LEN + 4
C
C     MAKE LENGTH MULTIPLE OF 120 OCTETS
C
      INTER = 120
      CALL MODVAL (LEN,LENB,INTER)
      LEN = LENB
C
 9001 format (1h ,'RECLEN : Length of section 2 is 0.')
C
 9002 format (1h ,'RECLEN : Length of section 4 is 0.')
      RETURN
      END
