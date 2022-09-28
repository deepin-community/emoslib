C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE DECOPS (NAME,CHDATA,ILENCH,FPDATA,ILENFP,INDATA,ILENIN,
     C                   NBYTE,LGDATA,ILENLG,IB1PAR,IPSCOD,ILENPS,
     C                   JLENCH,JLENFP,JLENIN,JLENLG,IRSERV,NBIT,IWORD,
     C                   IERR )
C
C---->
C*
C*     NAME     : DECOPS
C*
C*     FUNCTION : DECODE PSEUDO-GRIB CODED DATA.
C*
C*     INPUT    : NBIT     - NUMBER OF BITS IN COMPUTER WORD
C*
C*                IPSCOD   - INTEGER ARRAY OF DATA IN PSEUDO-GRIB CODE.
C*                ILENPS   - LENGTH OF ARRAY IPSCOD.
C*
C*                CHDATA   - CHARACTER VARIABLE TO RECEIVE DECODED
C*                           CHARACTER DATA.
C*                ILENCH   - LENGTH OF THIS VARIABLE
C*
C*                FPDATA   - REAL ARRAY TO RECEIVE DECODED F.P. VALUES.
C*                ILENFP   - LENGTH OF THIS ARRAY.
C*
C*                INDATA   - INTEGER ARRAY TO RECEIVE DECODED INTEGERS
C*                ILENIN   - LENGTH OF THIS ARRAY.
C*
C*                LGDATA   - LOGICAL ARRAY TO RECEIVE DECODED LOGICALS
C*                ILENLG   - LENGTH OF THIS ARRAY.
C*
C*                          IF ANY LENGTH (ILENCH,-FP,-IN,-LG) IS SET
C*                          TO 1, ONLY BLOCK 0 AND BLOCK 1 ARE DECODED.
C*
C*                IB1PAR  - INTEGER ARRAY (DIMENSION 18) TO RECEIVE
C*                          DATA DEFINITION INFORMATION.
C*
C*
C*     OUTPUT   : PARAMETER FROM BLOCK 0
C*                ----------------------
C*
C*                NAME    - CHARACTER VARIABLE, 4 LETTERS OF CODE NAME.
C*
C*
C*                PARAMETERS FROM BLOCK 1 , PRODUCT DEFINITION BLOCK.
C*                --------------------------------------------------
C*
C*                IB1PAR  - INTEGER ARRAY (DIMENSION 18) OF DATA
C*                          DEFINITION INFORMATION.
C*
C*                WORD      CONTENTS
C*                ----      --------
C*                  1       ORIGINATING CENTRE IDENTIFIER.
C*                  2       MODEL IDENTIFICATION.
C*                  3       GRID DEFINITION.
C*                  4       FLAG ( CODE TABLE 1)
C*                  5       PARAMETER IDENTIFIER ( CODE TABLE 2 ).
C*                  6       TYPE OF LEVEL (CODE TABLE 3).
C*                 7-8      VALUE(S) OF LEVELS (CODE TABLE 3).
C*                  9       YEAR OF DATA
C*                 10       MONTH OF DATA
C*                 11       DAY OF DATA
C*                 12       HOUR OF DATA
C*                 13       MINUTE OF DATA
C*                 14       TIME UNIT (CODE TABLE 4).
C*                 15       TIME RANGE ONE
C*                 16       TIME RANGE TWO
C*                 17       TIME RANGE FLAG (CODE TABLE 5).
C*                 18       NUMBER AVERAGED.
C*
C*                          VALUE(S) OF LEVEL CAN OCCUPY 2 WORDS..
C*                          FOR A LAYER THE FIRST WORD DEFINES THE TOP
C*                          AND THE SECOND THE BOTTOM OF THE LAYER.
C*                          FOR A SINGLE LEVEL, ONLY THE FIRST WORD IS
C*                          USED.
C*
C*
C*
C*                PARAMETERS FROM BLOCK 4 , BINARY DATA BLOCK.
C*                -------------------------------------------
C*
C*                CHDATA   - UNPACKED CHARACTER DATA
C*                JLENCH   - NUMBER OF CHARACTERS. NEGATIVE IF
C*                           MISSING DATA CODED.
C*
C*                FPDATA  - ARRAY OF FLOATING POINT VALUES.
C*                JLENFP  - NUMBER OF VALUES IN THIS ARRAY.
C*                          JLENFP IS NEGATIVE IF MISSING DATA CODED
C*
C*                INDATA  - ARRAY OF INTEGER VALUES.
C*                JLENIN  - NUMBER OF VALUES IN THIS ARRAY.
C*                          JLENIN IS NEGATIVE IF MISSING DATA CODED
C*
C*                LGDATA  - ARRAY OF LOGICAL VALUES.
C*                JLENLG  - NUMBER OF VALUES IN THIS ARRAY.
C*                          JLENLG IS NEGATIVE IF MISSING DATA CODED
C*
C*                NBYTE   - NUMBER OF BYTES FOR INTEGER VALUES.
C*
C*                IRSERV  - NUMBER OF BYTES IN DATA BLOCK RESERVED
C*                          FOR FUTURE USE.
C*
C*
C*                OTHER PARAMETERS.
C*                ----------------
C*
C*                IWORD   - NUMBER OF WORDS DECODED.
C*
C*                IERR    - ERROR INDICATOR.
C*
C*                          0, NO ERROR.
C*
C*                         -2, INPUT ARRAY HOLDS FEWER CODED VALUES
C*                             THAN EXPECTED.
C*                         -3, OUTPUT ARRAY/VARIABLE TOO SMALL TO HOLD
C*                             DECODED DATA VALUES.
C*                         -4, CURRENTLY UNDEFINED OPTION SELECTED.
C*
C*                          1, NO BLOCK 5 FOUND.
C*
C*    GENERAL  : DECOPS CALLS GBYTE
C*                            GBYTES
C*                            OFFSET
C*                            DECFP
C*
C*    AUTHOR   : J.HENNESSY  23.01.86
C*
C*    MODIFIED : J.HENNESSY  06.06.86
C*                    "      21.08.86
C*
C*               J.Hennessy  17.09.90
C*               Lower case letters as well as the characters
C*               _ - . , added.
C*
C     ---------------------------------------------------------------
C---->
C
C
      PARAMETER (NCHAR=67)
C
C
      LOGICAL     LGDATA
      CHARACTER*4 NAME
      CHARACTER*1 CHAR
      CHARACTER*(*) CHDATA
C
C
C
      DIMENSION FPDATA(*),INDATA(*),LGDATA(*)
      DIMENSION IPSCOD(ILENPS),IBLOCK(24),IB1PAR(18)
      DIMENSION IASCII(NCHAR)
      DIMENSION CHAR(NCHAR)
C
C
      DATA IASCII /65 ,66 ,67 ,68 ,69 ,70 ,71 ,72 ,73 ,74 ,75 ,76 ,77 ,
     C             78 ,79 ,80 ,81 ,82 ,83 ,84 ,85 ,86 ,87 ,88 ,89 ,90 ,
     C             48 ,49 ,50 ,51 ,52 ,53 ,54 ,55 ,56 ,57 ,32 ,
     C             97 ,98 ,99 ,100,101,102,103,104,105,106,107,108,109,
     C             110,111,112,113,114,115,116,117,118,119,120,121,122,
     C             95 ,46 ,44 , 45  /
C
      DATA CHAR   /'A','B','C','D','E','F','G','H','I','J','K','L','M',
     C             'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
     C             '0','1','2','3','4','5','6','7','8','9',' ',
     C             'a','b','c','d','e','f','g','h','i','j','k','l','m',
     C             'n','o','p','q','r','s','t','u','v','w','x','y','z',
     C              '_','.',',','-' /
C
C
C
C
C
C
      IERR = 0
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
C
C
C
C
C
C
C
C********************************************************************
C*
C*    BLOCK 0 - INDICATOR BLOCK.
C*
C********************************************************************
C
C
C     EXTRACT 4 OCTETS CONTAINING ASCII NAME.
C
      IWORD = 1
      IOFF  = 0
      IBYTE = 8
      NVAL  = 4
C
      CALL GBYTES (IPSCOD(1),IBLOCK(1),0,IBYTE,0,NVAL)
      CALL OFFSET (IOFF,NVAL,IWORD,IBYTE,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
      DO 100 I=1,36
         IF (IBLOCK(1).EQ.IASCII(I)) NAME(1:1) = CHAR(I)
         IF (IBLOCK(2).EQ.IASCII(I)) NAME(2:2) = CHAR(I)
         IF (IBLOCK(3).EQ.IASCII(I)) NAME(3:3) = CHAR(I)
         IF (IBLOCK(4).EQ.IASCII(I)) NAME(4:4) = CHAR(I)
  100 CONTINUE
C
C
C
C
C
C
C********************************************************************
C*
C*    BLOCK 1 - PRODUCT DEFINITION BLOCK.
C*
C********************************************************************
C
C
C     EXTRACT FIRST 4 OCTETS OF BLOCK 1.
C
      NVAL  = 4
C
      CALL GBYTES (IPSCOD(IWORD),IBLOCK(1),IOFF,IBYTE,0,NVAL)
      CALL OFFSET (IOFF,NVAL,IWORD,IBYTE,NBIT,ILENPS,IERR)
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
      CALL GBYTES (IPSCOD(IWORD),IBLOCK(5),IOFF,IBYTE,0,NVAL)
      CALL OFFSET (IOFF,NVAL,IWORD,IBYTE,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C     TRANSFER PRODUCT DEFINITION INFORMATION TO OUTPUT ARRAY.
C     EXCEPT FOR NUMBER AVERAGED.
C
      DO 200 I=1,17
         IB1PAR(I) = IBLOCK(I+4*ISNEW)
  200 CONTINUE
C
C
      WRITE(*,*) ' '
      WRITE(*,*) '     ******************************************'
      WRITE(*,*) '     ******************************************'
      WRITE(*,*) '     **                                      **'      
      WRITE(*,*) '     ** DECOPS is not Year-2000 compliant;   **'
      WRITE(*,*) '     ** dates are returned as 2 digits.      **'
      WRITE(*,*) '     **                                      **'
      WRITE(*,*) '     ** Please switch to using DECOPS2 which **'
      WRITE(*,*) '     ** returns dates as 4 digits in the     **'
      WRITE(*,*) '     ** range 1900 upwards.                  **'
      WRITE(*,*) '     **                                      **'
      WRITE(*,*) '     ** DECOPS will be set to abort after    **'
      WRITE(*,*) '     ** January 1999.                        **'
      WRITE(*,*) '     **                                      **'
      WRITE(*,*) '     ******************************************'
      WRITE(*,*) '     ******************************************'
      WRITE(*,*) ' '
C
C
C     NUMBER AVERAGED OCCUPIES 2 OCTETS.
C
      IB1PAR(18) = IBLOCK(18+4*ISNEW) * 256 + IBLOCK(19+4*ISNEW)
C
C
C
C
C
C***
C*    DESCRIPTION OF LEVEL OR LAYER ( CODE TABLE 3 ).
C***
C
C     CERTAIN LEVEL TYPES REQUIRE THAT THE DESCRIPTION OCCUPY
C     BOTH 8 BIT FIELDS. PUT SINGLE VALUE IN FIRST LEVEL WORD.
C
      KTEMP = 32 + (ISNEW*224)
      IF ((IB1PAR(6).EQ. 20).OR.
     X    (IB1PAR(6).EQ.100).OR.
     X    (IB1PAR(6).EQ.103).OR.
     X    (IB1PAR(6).EQ.105).OR.
     X    (IB1PAR(6).EQ.107).OR.
     X    (IB1PAR(6).EQ.109).OR.
     X    (IB1PAR(6).EQ.111).OR.
     X    (IB1PAR(6).EQ.113).OR.
     X    (IB1PAR(6).EQ.115).OR.
     X    (IB1PAR(6).EQ.117).OR.
     X    (IB1PAR(6).EQ.125).OR.
     X    (IB1PAR(6).EQ.160) ) THEN
              IB1PAR(7) = IB1PAR(7) * KTEMP + IB1PAR(8)
              IB1PAR(8) = 0
          ENDIF
C
C
C
C
C
C
C
C
C
C***
C*    TIME RANGE.
C***
C
C     ONE TIME RANGE CAN OCCUPY TWO OCTETS.
C
      IF (IB1PAR(17).EQ.10)
     C   THEN
             IB1PAR(15) = IB1PAR(15) * KTEMP + IB1PAR(16)
             IB1PAR(16) = 0
         ENDIF
C
C
C
C
C
C
C
C
C
C***
C*    CHECK IF DECODING OF ONLY BLOCKS 0 AND 1 HAS BEEN REQUESTED.
C***
C
      IF (ILENFP.EQ.1.OR.ILENIN.EQ.1.OR.ILENLG.EQ.1) RETURN
      IF (ILENCH.EQ.1) RETURN
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
C********************************************************************
C*
C*    BLOCK 4 - BINARY DATA BLOCK.
C*
C********************************************************************
C
C
C
C***
C*    GET LENGTH OF BINARY DATA BLOCK.
C***
C
C
      IPW = IWORD
      IPB = IOFF
C
      CALL GBYTE (IPSCOD(IWORD),LBIN,IOFF,24)
      CALL OFFSET (IOFF,1,IWORD,24,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C
C
C
C
C
C
C***
C*    OCTET 4 IS RESERVED.
C*    (IT IS USED IN OLDER VERSIONS OF BUDGET VALUE RECORDS AND
C*    IT GIVES THE STARTING OCTET ADDRESS OF LOGICAL VALUES, A
C*    FIXED VALUE OF 34 OR 38)
C***
C
      CALL GBYTE (IPSCOD(IWORD),ISTLG,IOFF,8)
      IOLDBV = 0
      IF (ISTLG.EQ.34.OR.ISTLG.EQ.38) IOLDBV = 1
C
C
      CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C
C
C
C
C
C
C
C***
C*    OCTET 5 - MISSING DATA FLAG. (IN OLDER VERSIONS OF BUDGET VALUE
C*    RECORD IT GIVES STARTING OCTET ADDRESS OF INTEGER VALUES)
C***
C
      CALL GBYTE (IPSCOD(IWORD),IMISS,IOFF,8)
      CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
      IF (IOLDBV.EQ.1)
     C   THEN
             ISTIN = IMISS
             IMISS = 0
         ENDIF
C
C
C
C
C
C
C
C***
C*    OCTETS 6-7 IN OLDER VERSIONS OF BUDGET VALUE RECORDS GIVE
C*    STARTING BYTE ADDRESS OF FLOATING POINT VALUES SECTION.
C*    OCTETS 8-9 GIVING THE LENGTH OF THE FLOATING POINT DATA
C*    SECTION IN OCTETS.
C***
C
      IF (IOLDBV.EQ.1)
     C   THEN
             CALL GBYTE (IPSCOD(IWORD),ISTFP,IOFF,16)
             CALL OFFSET (IOFF,1,IWORD,16,NBIT,ILENPS,IERR)
             IF (IERR.NE.0) RETURN
C
             CALL GBYTE (IPSCOD(IWORD),LENFP,IOFF,16)
             CALL OFFSET (IOFF,1,IWORD,16,NBIT,ILENPS,IERR)
             IF (IERR.NE.0) RETURN
C
C            NUMBER OF FLOATING POINT VALUES.
C            4 BYTES PER FLOATING POINT NUMBER.
C
             JLENFP = LENFP / 4
C
C            NUMBER OF LOGICAL VALUES.
C
             JLENLG = ISTIN - ISTLG
C
C            NUMBER OF INTEGER VALUES.
C            2 OCTETS PER INTEGER.
C
             NBYTE = 2
             IBITS = NBYTE * 8
             JLENIN = ( ISTFP - ISTIN ) / NBYTE
C
C            NO CHARACTER DATA IN OLD FORMAT.
C
             JLENCH = 0
C
C            CHECK NUMBERS OF DATA ITEMS AGAINST DIMENSIONS
C            OF TARGET ARRAYS.
C
             IF (JLENFP.GT.ILENFP)
     C          THEN
                    IERR = -3
                    WRITE (*,9001) JLENFP
                ENDIF
C
             IF (JLENLG.GT.ILENLG)
     C          THEN
                    IERR = -3
                    WRITE (*,9002) JLENLG
                ENDIF
C
             IF (JLENIN.GT.ILENIN)
     C          THEN
                    IERR = -3
                    WRITE (*,9003) JLENIN
                ENDIF
C
             IF (IERR.NE.0) RETURN
C
C            NOW WE GO AROUND THE HOUSES AS THIS OLD DATA
C            HAS THE DIFFERENT DATA TYPES STORED IN A DIFFERENT
C            ORDER.
C
C            LOGICAL VARIABLES FIRST.
C
             GO TO 450
C
         ENDIF
C
C
C
C
C
C
C***
C*    OCTET 6 - NUMBER OF BYTES PER INTEGER.
C***
C
      CALL GBYTE (IPSCOD(IWORD),NBYTE,IOFF,8)
      CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
      IBITS = NBYTE * 8
C
C
C
C
C
C
C
C***
C*    OCTETS 7-8 ARE RESERVED.
C***
C
      CALL OFFSET (IOFF,2,IWORD,8,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C
C
C
C
C
C
C***
C*    OCTETS 9-11 - NUMBER OF CHARACTERS IN CHARACTER SECTION.
C***
C
      CALL GBYTE (IPSCOD(IWORD),JLENCH,IOFF,24)
      CALL OFFSET (IOFF,1,IWORD,24,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
      IF (JLENCH.GT.ILENCH)
     C   THEN
             IERR = -3
             WRITE (*,9004) JLENCH
             RETURN
         ENDIF
C
C
C
C
C
C
C
C
C***
C*    OCTETS 12-14 , NUMBER OF FLOATING POINT VALUES.
C***
C
      CALL GBYTE (IPSCOD(IWORD),JLENFP,IOFF,24)
      CALL OFFSET (IOFF,1,IWORD,24,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
      IF (JLENFP.GT.ILENFP)
     C   THEN
             IERR = -3
             WRITE (*,9001) JLENFP
             RETURN
         ENDIF
C
C
C
C
C
C
C
C
C
C***
C*    OCTETS 15-17 , NUMBER OF INTEGER VALUES.
C***
C
      CALL GBYTE (IPSCOD(IWORD),JLENIN,IOFF,24)
      CALL OFFSET (IOFF,1,IWORD,24,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
      IF (JLENIN.GT.ILENIN)
     C   THEN
             IERR = -3
             WRITE (*,9003) JLENIN
             RETURN
         ENDIF
C
C
C
C
C
C
C
C
C
C***
C*    OCTETS 18-20 , NUMBER OF LOGICALS.
C***
C
      CALL GBYTE (IPSCOD(IWORD),JLENLG,IOFF,24)
      CALL OFFSET (IOFF,1,IWORD,24,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
      IF (JLENLG.GT.ILENLG)
     C   THEN
             IERR = -3
             WRITE (*,9002) JLENLG
             RETURN
         ENDIF
C
C
C
C
C
C
C
C
C
C***
C*    OCTETS 21-23 , NUMBER OF RESERVED OCTETS.
C***
C
      CALL GBYTE (IPSCOD(IWORD),IRSERV,IOFF,24)
      CALL OFFSET (IOFF,1,IWORD,24,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C
C
C
C
C
C
C
C
C***
C*    OCTETS 24-32 ARE RESERVED
C***
C
      CALL OFFSET (IOFF,9,IWORD,8,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C
C
C
C
C
C
C***
C*    OCTET 33- ,GET FLOATING POINT NUMBERS AND CONVERT.
C***
C
  250 CONTINUE
C
      IF (JLENFP.NE.0)
     C   THEN
C
             DO 300 I=1,JLENFP
                CALL GBYTE (IPSCOD(IWORD),IEXP,IOFF,8)
                CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENPS,IERR)
                IF (IERR.NE.0) RETURN
                CALL GBYTE (IPSCOD(IWORD),IMANT,IOFF,24)
                CALL OFFSET (IOFF,1,IWORD,24,NBIT,ILENPS,IERR)
                IF (IERR.NE.0) RETURN
                CALL DECFP (FPDATA(I),IEXP,IMANT)
  300        CONTINUE
C
         ENDIF
C
C     NO CHARACTER DATA IN OLD BUDGET VALUE RECORDS.
C
      IF (IOLDBV.EQ.1) GO TO 550
C
C
C
C
C
C
C
C
C
C***
C*    GET INTEGER VALUES.
C***
C
  350 CONTINUE
C
      IF (JLENIN.NE.0)
     C   THEN
C
C            SET UP MASK FOR NEGATIVE NUMBERS
C
             NEGSIN = 2 ** (IBITS-1)
C
C            MAXIMUM SIZE INTEGER IN GIVEN FIELD WIDTH.
C
             MAXINT = NEGSIN - 1
C
             DO 400 I=1,JLENIN
                CALL GBYTE (IPSCOD(IWORD),KTEMP,IOFF,IBITS)
                CALL OFFSET (IOFF,1,IWORD,IBITS,NBIT,ILENPS,IERR)
                IF (IERR.NE.0) RETURN
C
C               HANDLE NEGATIVE NUMBERS
C
                IF (KTEMP.GT.MAXINT)
     C             THEN
                       KTEMP = KTEMP - NEGSIN
                       KTEMP = - KTEMP
                   ENDIF
C
                INDATA(I) = KTEMP
  400        CONTINUE
C
         ENDIF
C
C
C     FOR OLDER BUDGET VALUES OUR LAST STOP IS FOR FLOATING POINT
C     VALUES.
C
      IF (IOLDBV.EQ.1) GO TO 250
C
C
C
C***
C*    GET LOGICALS AND CONVERT.
C***
C
  450 CONTINUE
C
      IF (JLENLG.NE.0)
     C   THEN
C
             DO 500 I=1,JLENLG
                CALL GBYTE (IPSCOD(IWORD),KTEMP,IOFF,8)
                CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENPS,IERR)
                IF (IERR.NE.0) RETURN
C
                LGDATA(I) = .FALSE.
                IF (KTEMP.EQ.1) LGDATA(I) = .TRUE.
  500        CONTINUE
C
         ENDIF
C
C
C     FOR OLD BUDGET VALUE VERSION WE JUMP AGAIN.
C     NEXT STOP ON TOUR IS FOR INTEGER VALUES.
C
      IF (IOLDBV.EQ.1) GO TO 350
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
C
C
C***
C*    GET CHARACTER DATA.
C***
C
      IF (JLENCH.NE.0)
     C   THEN
             DO 530 I=1,JLENCH
                CALL GBYTE (IPSCOD(IWORD),KTEMP,IOFF,8)
                CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENPS,IERR)
                IF (IERR.NE.0) RETURN
                DO 525 J=1,NCHAR
                   IF (KTEMP.EQ.IASCII(J))
     C                THEN
                          CHDATA(I:I) = CHAR(J)
                          GO TO 530
                      ENDIF
  525           CONTINUE
                IERR = -6
                WRITE (*,9016)
 9016           FORMAT (1H ,'DECOPS : Character data contains other ',
     C                 'than _ - . , letters, figures or spaces.')
                RETURN
  530       CONTINUE
         ENDIF
C
C
C
C
C
C
C
C***
C*    RESERVED BYTES IN DATA BLOCK.
C***
C
      CALL OFFSET (IOFF,IRSERV,IWORD,8,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C
C
C
C
C
C
C
C
C***
C*    SKIP ANY ZERO FILL AT END OF DATA BLOCK.
C***
C
  550 CONTINUE
C
      L = (IWORD-IPW) * NBIT + IOFF -IPB
      L = LBIN * 8 - L
      IF (L.NE.0)
     C    THEN
              CALL OFFSET (IOFF,1,IWORD,L,NBIT,ILENPS,IERR)
              IF (IERR.NE.0) RETURN
          ENDIF
C
C
C
C
C********************************************************************
C*
C*    BLOCK 5 - END BLOCK.
C*
C********************************************************************
C
C     CHECK 7 7 7 7 AT END OF CODED DATA.
C
C
      CALL GBYTES (IPSCOD(IWORD),IBLOCK(1),IOFF,8,0,4)
C
C     55 IS THE VALUE OF ASCII CHARACTER '7'
C
      DO 600 I=1,4
         IF (IBLOCK(I).NE.55) IERR = 1
  600 CONTINUE
C
      IF (IERR.EQ.1) WRITE (*,*) ' NO 7777 GROUP FOUND '
C
C
C
C     SET NUMBER OF VALUES NEGATIVE, IF MISSING DATA VALUES.
C
      IF (IMISS.GE.8)
     C   THEN
             JLENCH = -JLENCH
             IMISS  =  IMISS - 8
         ENDIF
C
      IF (IMISS.GE.4)
     C   THEN
             JLENFP = -JLENFP
             IMISS  =  IMISS - 4
         ENDIF
C
      IF (IMISS.GE.2)
     C   THEN
             JLENIN = -JLENIN
             IMISS  =  IMISS - 2
         ENDIF
C
      IF (IMISS.EQ.1) JLENLG = -JLENLG
C
C
      RETURN
C
 9001 FORMAT (1H ,'DECOPS : Target real array must be at least ',
     C             I10,' words long.')
C
 9002 FORMAT (1H ,'DECOPS : Target logical array must be at least ',
     C             I10,' words long.')
C
 9003 FORMAT (1H ,'DECOPS : Target integer array must be at least ',
     C             I10,' words long.')
C
 9004 FORMAT (1H ,'DECOPS : Target character string must be at least ',
     C             I10,' characters long.')
C
C
      END
