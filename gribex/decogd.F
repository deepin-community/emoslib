C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C


      SUBROUTINE DECOGD (FPDATA,ILENF,IBITS,NBIT,IB1PAR,IB2PAR,
     C              IB3PAR,FVERT,ILENV,IGRIB,ILENG,IWORD,
     C              JLENV,JLENF,RMISS,IERR)
C
C---->
C*     NAME     : DECOGD
C*
C*     FUNCTION : DECODE WMO GRIB CODED DATA.
C*
C*     INPUT    : NBIT    - NUMBER OF BITS IN COMPUTER WORD
C*
C*                IGRIB   - INTEGER ARRAY CONTAINING DATA IN GRIB CODE.
C*                ILENG   - LENGTH OF ARRAY IGRIB.
C*
C*                FVERT   - REAL ARRAY TO RECEIVE VERTICAL COORDINATE
C*                          PARAMETERS.
C*                ILENV   - LENGTH OF ARRAY FVERT.
C*
C*                FPDATA  - REAL ARRAY TO RECEIVE DECODED DATA VALUES.
C*                ILENF   - LENGTH OF ARRAY FPDATA.
C*
C*                          *****  IF THIS IS 1 ONLY
C*                          THE PRODUCT AND GRID DEFINITION BLOCKS
C*                          ARE DECODED. *****
C*
C*                IB1PAR  - INTEGER ARRAY (DIMENSION 18) TO RECEIVE
C*                          PRODUCT DEFINITION INFORMATION.
C*
C*                IB2PAR  - INTEGER ARRAY (DIMENSION 11) TO RECEIVE GRID
C*                          DEFINITION INFORMATION.
C*
C*     OUTPUT   : PARAMETERS FROM BLOCK 1 , PRODUCT DEFINITION BLOCK.
C*                --------------------------------------------------
C*                IB1PAR  - INTEGER ARRAY (DIMENSION 18) OF PRODUCT
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
C*                PARAMETERS FROM BLOCK 2 , GRID DEFINITION BLOCK.
C*                -----------------------------------------------
C*                IB2PAR - INTEGER ARRAY (DIMENSION 11) CONTAINING GRID
C*                         DEFINITION INFORMATION.
C*                         USE VARIES WITH DATA REPRESENTATION TYPE.
C*
C*                WORD      LAT/LONG GRID
C*                ----      -------------
C*                 1        DATA REPRESENTATION TYPE (CODE TABLE 6).
C*                 2        NO. OF POINTS ALONG A LATITUDE
C*                 3        NO. OF POINTS ALONG A MERIDIAN.
C*                 4        LATITUDE OF ORIGIN (SOUTH - IVE).
C*                 5        LONGITUDE OF ORIGIN (WEST - IVE).
C*                 6        RESOLUTION FLAG. (CODE TABLE 7)
C*                 7        LATITUDE OF EXTREME POINT (SOUTH - IVE).
C*                 8        LONGITUDE OF EXTREME POINT (WEST - IVE).
C*                 9        LATITUDE INCREMENT
C*                10        LONGITUDE INCREMENT
C*                11        SCANNING MODE FLAGS (CODE TABLE 8)
C*
C*                WORD      GAUSSIAN GRID
C*                ----      -------------
C*                1-9       AS FOR LAT/LONGITUDE GRID.
C*                10        THE NUMBER OF LATITUDE LINES BETWEEN A POLE
C*                          AND THE EQUATOR.
C*                11        SCANNING MODE FLAGS (CODE TABLE 8)
C*
C*                WORD      SPHERICAL HARMONICS
C*                ----      -------------------
C*                  1       DATA REPRESENTATION TYPE (CODE TABLE 6)
C*                  2       J - PENTAGONAL RESOLUTION PARAMETER
C*                  3       K - PENTAGONAL RESOLUTION PARAMETER
C*                  4       M - PENTAGONAL RESOLUTION PARAMETER
C*                  5       REPRESENTATION TYPE (CODE TABLE 9)
C*                  6       REPRESENTATION MODE (CODE TABLE 10)
C*                7-11      NOT USED
C*
C*
C*                USE FOR OTHER DATA REPRESENTATION TYPES IS
C*
C*                FVERT  - REAL ARRAY OF VERTICAL COORDINATE PARAMETERS
C*                JLENV  - NUMBER OF VALUES IN THIS ARRAY.
C*
C*                USED FOR HYBRID LEVELS ONLY.
C*
C*                PARAMETERS FROM BLOCK 3 , BIT-MAP DEFINITION BLOCK.
C*                --------------------------------------------------
C*
C*                IBMAP     -  ARRAY (DIMENSION IBLEN) CONTAINING BIT MA
C*                IBLEN     -  NUMBER OF WORDS IN ARRAY IBMAP.
C*                IBVAL     -  NUMBER OF VALUES ON THIS ARRAY.
C*                IERR      -  ERROR INDICATOR
C*                             -5, MISSING PREDEFINED BIT MAP.
C*
C*
C*                PARAMETERS FROM BLOCK 4 , BINARY DATA BLOCK.
C*                -------------------------------------------
C*                FPDATA  - ARRAY OF FLOATING POINT VALUES.
C*                JLENF   - NUMBER OF VALUES IN THIS ARRAY.
C*                          JLENF IS NEGATIVE IF MISSING DATA CODED
C*
C*                IBITS   - NUMBER OF BITS FOR CODED DATA VALUES.
C*
C*                IWORD   - NUMBER OF WORDS DECODED.
C*
C*                IERR    - ERROR INDICATOR.
C*
C*                          0, NO ERROR.
C*
C*                         -1, NUMBER OF BITS PER DATA VALUE EXCEEDS
C*                             WORD LENGTH, OR MAXIMUM ALLOWED.
C*                         -2, INPUT ARRAY HOLDS FEWER CODED VALUES
C*                             THAN EXPECTED.
C*                         -3, OUTPUT ARRAY TOO SMALL TO HOLD DECODED
C*                             DATA VALUES.
C*                         -4, CURRENTLY UNDEFINED OPTION SELECTED.
C*
C*                          1, NO BLOCK 5 FOUND.
C*
C*    GENERAL  : DECOGD CALLS GBYTE
C*                            GBYTES
C*                            OFFSET
C*                            DECFP
C*                            SBYTE
C*
C*   Modifications
C*   -------------
C
C940309     J.D.Chambers     ECMWF      09:03:94
C940309     Maximum size for bitmap array increased to 132000
C940309     to allow for products 0.5 * 0.5 degrees, whole
C940309     hemisphere (N or S).
C*
C     ---------------------------------------------------------------
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
      DIMENSION FPDATA(ILENF),FVERT(ILENV)
      DIMENSION IBLOCK(24),IGRIB(ILENG)
      DIMENSION IB1PAR(18),IB2PAR(11),IB3PAR(2),ILAT(2)
C
C940309 INTEGER*4 IBMAP(41472)
      INTEGER*4 IBMAP(132000)
C940309 DIMENSION FPDATAG(41472)
      DIMENSION FPDATAG(132000)
      CHARACTER*20 BITMAP
      CHARACTER YCHAR
C
      EQUIVALENCE (ZGRIB,JGRIB)
C
C     SET MAXIMUM NUMBER OF BITS PER DATA FIELD.
C
      DATA IMAX /32/
C
      ZSCALE = 0.
      IERR = 0
C
      RMISS=1.7E38
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
C     EXTRACT 4 OCTETS CONTAINING ASCII G R I B .
C
      IWORD = 1
      IOFF  = 0
      IBYTE = 8
      NVAL  = 4
C
      CALL GBYTES (IGRIB(1),IBLOCK(1),0,IBYTE,0,NVAL)
      CALL OFFSET (IOFF,NVAL,IWORD,IBYTE,NBIT,ILENG,IERR)
      IF (IERR.NE.0) RETURN
C
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
      CALL GBYTES (IGRIB(IWORD),IBLOCK(1),IOFF,IBYTE,0,NVAL)
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
      CALL GBYTES (IGRIB(IWORD),IBLOCK(5),IOFF,IBYTE,0,NVAL)
      CALL OFFSET (IOFF,NVAL,IWORD,IBYTE,NBIT,ILENG,IERR)
      IF (IERR.NE.0) RETURN
C
C     TRANSFER PRODUCT DEFINITION INFORMATION TO OUTPUT ARRAY.
C     EXCEPT FOR NUMBER AVERAGED.
C
      DO 100 I=1,17
         IB1PAR(I) = IBLOCK(I+4*ISNEW)
  100 CONTINUE
C
C
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
      IF (IB1PAR(6).EQ.100.OR.IB1PAR(6).EQ.103.OR.IB1PAR(6).EQ.105.
     C    OR.IB1PAR(6).EQ.107.OR.IB1PAR(6).EQ.109)
     C    THEN
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
C
C
C********************************************************************
C*
C*    BLOCK 2 - GRID DESCRIPTION BLOCK.
C*
C********************************************************************
C
C***
C*    CHECK FLAG INDICATING IF BLOCK 2 IS INCLUDED IN CODE.
C***
C
C     INC IS SET TO 1 , IF BLOCK 2 IS INCLUDED.
C
      INC = 0
C
C
      IF (ISNEW.EQ.0)
     C   THEN
C            OLD VERSION OF CODE
C
C            BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
C
C                NONE             00000000               0
C                 2               00000001               1
C                 3               00000010               2
C             2 AND 3             00000011               3
C
             IF (IB1PAR(4).EQ.1.OR.IB1PAR(4).EQ.3) INC = 1
         ELSE
C            NEW VERSION
C
C            BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
C
C                NONE             00000000               0
C                 2               10000000             128
C                 3               01000000              64
C             2 AND 3             11000000             192
C
             IF (IB1PAR(4).EQ.128.OR.IB1PAR(4).EQ.192) INC = 1
         ENDIF
C
      IF (INC.EQ.1)
     C   THEN
C
C
C***
C*           LENGTH OF GRID DESCRIPTION BLOCK.
C***
C
             CALL GBYTE (IGRIB(IWORD),LBLK,IOFF,24)
             CALL OFFSET (IOFF,1,IWORD,24,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
C
C            LENGTH IS 32 OCTETS FOR LAT/LONG, GAUSSIAN AND SPHERICAL
C            HARMONICS . FOR ANY DATA  ON HYBRID LEVELS THE
C            VERTICAL COORDINATES ARE ADDED.
C            GET NUMBER OF VERTICAL COORDINATE PARAMETERS, IF ANY.
C
             JLENV = (LBLK - 32) / 4
             IF(ILENV.LT.JLENV)
     C          THEN
                    IERR = -3
                    WRITE (*,9001) JLENV,ILENV
 9001               FORMAT (1H ,'NUMBER OF VERTICAL COORDINATES - ',I4,
     C                          ', ARRAY SIZE IS - ',I4)
                    RETURN
                ENDIF
C
C
C***
C*           NUMBER OF UNUSED BITS AT END OF BLOCK.
C***
C
             CALL GBYTE (IGRIB(IWORD),LNIL,IOFF,8)
             CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
C
C
C
C
C***
C*           NEXT OCTET IS RESERVED.
C***
C
             CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
C
C
C
C
C
C
C
C***
C*           DATA REPRESENTATION TYPE.
C***
C
             CALL GBYTE (IGRIB(IWORD),IB2PAR(1),IOFF,8)
             CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
C
C            LAT/LONGITUDE GRID, GAUSSIAN GRID AND SPHERICAL HARMONICS
C            ARE THE ONLY DATA REPRESENTATIONS HANDLED.
C
             IF (IB2PAR(1).NE.0.AND.IB2PAR(1).NE.4.AND.IB2PAR(1).NE.50)
     C          THEN
                     WRITE (*,*)'GRID DESCRIPTION BLOCK NOT YET DEFINED'
                     IERR = -4
                     RETURN
                ENDIF
C
C
C
C
C***
C*           LAT/LONG OR GAUSSIAN GRID.
C***
C
             IF (IB2PAR(1).EQ.0.OR.IB2PAR(1).EQ.4)
     C         THEN
C
C                  NUMBER OF LAT/LONG POINTS.
C
                   CALL GBYTES (IGRIB(IWORD),IB2PAR(2),IOFF,16,0,2)
                   CALL OFFSET (IOFF,2,IWORD,16,NBIT,ILENG,IERR)
                   IF (IERR.NE.0) RETURN
C
C                  LAT/LONG OF ORIGIN.
C
                   CALL GBYTES (IGRIB(IWORD),ILAT(1),IOFF,24,0,2)
                   CALL OFFSET (IOFF,2,IWORD,24,NBIT,ILENG,IERR)
                   IF (IERR.NE.0) RETURN
C
C                  IF SIGN BIT SET TO 1 , VALUES ARE NEGATIVE.
C
                   DO 200 I=1,2
                      IB2PAR(I+3) = ILAT(I)
                      IF (ILAT(I).GT.8388608)
     C                   THEN
                             IB2PAR(I+3) = IB2PAR(I+3) - 8388608
                             IB2PAR(I+3) = - IB2PAR(I+3)
                         ENDIF
  200             CONTINUE
C
C
C                  RESOLUTION FLAG.
C
                   CALL GBYTE (IGRIB(IWORD),IB2PAR(6),IOFF,8)
                   CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
                   IF (IERR.NE.0) RETURN
C
C
C                  LAT/LONG OF EXTREME POINTS.
C
                   CALL GBYTES (IGRIB(IWORD),ILAT(1),IOFF,24,0,2)
                   CALL OFFSET (IOFF,2,IWORD,24,NBIT,ILENG,IERR)
                   IF (IERR.NE.0) RETURN
C
C                  IF SIGN BIT SET TO 1 , VALUES ARE NEGATIVE.
C
                   DO 300 I=1,2
                      IB2PAR(I+6) = ILAT(I)
                      IF (ILAT(I).GT.8388608)
     C                   THEN
                             IB2PAR(I+6) = IB2PAR(I+6) - 8388608
                             IB2PAR(I+6) = - IB2PAR(I+6)
                         ENDIF
  300             CONTINUE
C
C                 DIRECTION INCREMENTS / NUMBER OF LATITUDE LINES.
C
                  CALL GBYTES (IGRIB(IWORD),IB2PAR(9),IOFF,16,0,2)
                  CALL OFFSET (IOFF,2,IWORD,16,NBIT,ILENG,IERR)
                  IF (IERR.NE.0) RETURN
C
C                 SCANNING MODE FLAGS.
C
                  CALL GBYTE (IGRIB(IWORD),IB2PAR(11),IOFF,8)
                  CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
                  IF (IERR.NE.0) RETURN
C
C                 4 RESERVED OCTETS.
C
                  CALL OFFSET (IOFF,4,IWORD,8,NBIT,ILENG,IERR)
                  IF (IERR.NE.0) RETURN
C
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
C
C
C
C
C
C***
C*            SPHERICAL HARMONIC DATA.
C***
C
              IF (IB2PAR(1).EQ.50)
     C           THEN
C
C                    PENTAGONAL RESOLUTION PARAMETERS.
C
                     CALL GBYTES (IGRIB(IWORD),IB2PAR(2),IOFF,16,0,3)
                     CALL OFFSET (IOFF,3,IWORD,16,NBIT,ILENG,IERR)
                     IF (IERR.NE.0) RETURN
C
C                    REPRESENTATION TYPE AND MODE.
C
                     CALL GBYTES (IGRIB(IWORD),IB2PAR(5),IOFF,8,0,2)
                     CALL OFFSET (IOFF,2,IWORD,8,NBIT,ILENG,IERR)
                     IF (IERR.NE.0) RETURN
C
C                    18 RESERVED OCTETS.
C
                     CALL OFFSET (IOFF,18,IWORD,8,NBIT,ILENG,IERR)
                     IF (IERR.NE.0) RETURN
C
                 ENDIF
C
C
C
C
C***
C*               VERTICAL COORDINATE PARAMETERS FOR HYBRID LEVELS.
C***
C
                 IF (JLENV.NE.0)
     C              THEN
                        DO 400 I=1,JLENV
                           CALL GBYTE (IGRIB(IWORD),IEXP,IOFF,8)
                           CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
                           IF (IERR.NE.0) RETURN
                           CALL GBYTE (IGRIB(IWORD),IMANT,IOFF,24)
                           CALL OFFSET (IOFF,1,IWORD,24,NBIT,ILENG,IERR)
                           IF (IERR.NE.0) RETURN
                           CALL DECFP (FVERT(I),IEXP,IMANT)
  400                   CONTINUE
                    ENDIF
C
         ENDIF
C
C
C     RETURN IF ONLY PRODUCT AND GRID DEFINITION BLOCKS REQUIRED.
C
      IF (ILENF.EQ.1) RETURN
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
C*    BLOCK 3 - BIT MAP BLOCK.
C*
C********************************************************************
C
C     INC IS SET TO 1 , IF BLOCK 3 IS INCLUDED.
C
      INC = 0
C
C
      IF (ISNEW.EQ.0)
     C   THEN
C            OLD VERSION OF CODE
C
C            BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
C
C                NONE             00000000               0
C                 2               00000001               1
C                 3               00000010               2
C             2 AND 3             00000011               3
C
             IF (IB1PAR(4).EQ.2.OR.IB1PAR(4).EQ.3) INC = 1
         ELSE
C            NEW VERSION
C
C            BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
C
C                NONE             00000000               0
C                 2               10000000             128
C                 3               01000000              64
C             2 AND 3             11000000             192
C
             IF (IB1PAR(4).EQ.64.OR.IB1PAR(4).EQ.192) INC = 1
         ENDIF
C
      IF (INC.EQ.1)
     C     THEN
C
C
C*            GET LENGTH OF THE BIT MAP BLOCK
C
C
      CALL GBYTE (IGRIB(IWORD),IB3LEN,IOFF,24)
      CALL OFFSET(IOFF,1,IWORD,24,NBIT,ILENG,IERR)
      IF( IERR.NE.0 ) RETURN
C
C
C*            GET NUMBER OF UNUSED BITS AT THE END OF THE BLOCK 3.
C
C
      CALL GBYTE(IGRIB(IWORD),INUB,IOFF,8)
      CALL OFFSET(IOFF,1,IWORD,8,NBIT,ILENG,IERR)
      IF( IERR.NE.0 ) RETURN
C
C*            CHECK IF BIT MAP FOLLOWS
C
      CALL GBYTE (IGRIB(IWORD),IBMF,IOFF,16)
      CALL OFFSET(IOFF,1,IWORD,16,NBIT,ILENG,IERR)
      IF( IERR.NE.0 ) RETURN
C
      IB3PAR(1)=IBMF
C
      IF( IBMF.EQ.0 )
     1   THEN
            IBVAL=(IB3LEN-6)*8-INUB
C
C940309     IF(IBVAL.GT.41472) THEN
            IF(IBVAL.GT.132000) THEN
               IERR=-3
               WRITE(*,8888) IBVAL
 8888          FORMAT(1H ,'BIT MAP ARRAY TOO SMALL - IBVAL =',I12)
               RETURN
            END IF

            DO 450 J=1,IBVAL
C
             CALL GBYTE(IGRIB(IWORD),IBMAP(J),IOFF,1)
             CALL OFFSET(IOFF,1,IWORD,1,NBIT,ILENG,IERR)
             IF(IERR.NE.0) RETURN
C
 450         CONTINUE
C
             CALL OFFSET(IOFF,1,IWORD,INUB,NBIT,ILENG,IERR)
C
           ELSE
C
C*            GET PREDEFINED BIT MAP
C
              WRITE(YCHAR,'(I5)') IBMF
C
              BITMAP='BIT_MAP_'//YCHAR//'.DAT'
C
              OPEN(UNIT=10,IOSTAT=IOS,ERR=455,FILE=BITMAP,
     1        FORM='UNFORMATTED',STATUS='OLD')
C
              READ(10) IBVAL,IBLEN,(IBMAP(I),I=1,IBLEN)
C
              CLOSE(10)
C
              GO TO 460
C
 455          CONTINUE
C
              IERR=-5
              WRITE(*,9906) IOS
 9906         FORMAT(1H ,'ERROR ON UNIT 10, ERROR NO. ',I6)
              RETURN
C
 460          CONTINUE
C
           ENDIF
C
C
       END IF
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
      CALL GBYTE (IGRIB(IWORD),LBIN,IOFF,24)
      CALL OFFSET (IOFF,1,IWORD,24,NBIT,ILENG,IERR)
      IF (IERR.NE.0) RETURN
C
C
C
C
C***
C*    4 BIT FLAG / 4 BIT COUNT OF UNUSED BITS AT END OF BLOCK OCTET.
C***
C
      CALL GBYTE (IGRIB(IWORD),IFLAG,IOFF,8)
      CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
      IF (IERR.NE.0) RETURN
C
      IF (ISNEW.EQ.0)
     C    THEN
C             OLD VERSION OF CODE
C
C             0000---- GRID POINT
C             0001---- SPHERICAL HARMONICS
C
              IREP = IFLAG / 16
              LNIL = IFLAG - IREP * 16
C
          ELSE
C             NEW VERSION OF CODE
C
C             0000---- GRID POINT
C             1000---- SPHERICAL HARMONICS
C
              IREP = IFLAG / 128
              LNIL = IFLAG - IREP * 128
C
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
C*    GET SCALE FACTOR
C***
C
      CALL GBYTE (IGRIB(IWORD),JSCALE,IOFF,16)
      CALL OFFSET (IOFF,1,IWORD,16,NBIT,ILENG,IERR)
      IF (IERR.NE.0) RETURN
C
      ISCALE = JSCALE
      IF (JSCALE.GE.32768)
     C    THEN
              ISCALE = ISCALE - 32768
              ISCALE = - ISCALE
          ENDIF
C
C
C
C
C***
C*    GET REFERENCE VALUE (FMIN) IN GRIB FORMAT (IEXP,IMANT)
C***
C
C
      CALL GBYTE (IGRIB(IWORD),IEXP,IOFF,8)
      CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
      IF (IERR.NE.0) RETURN
      CALL GBYTE (IGRIB(IWORD),IMANT,IOFF,24)
      CALL OFFSET (IOFF,1,IWORD,24,NBIT,ILENG,IERR)
      IF (IERR.NE.0) RETURN
C
C
C     CHECK FOR MISSING DATA INDICATORS.
C
      IMISS = 0
      IF (JSCALE.EQ.65535.AND.IEXP.EQ.255.AND.IMANT.EQ.16777215)
     C   IMISS = 1
C
C     CONVERT REFERENCE VALUE AND SCALE FACTOR.
C
      IF (IMISS.EQ.0)
     C   THEN
             CALL DECFP (FMIN,IEXP,IMANT)
             ZSCALE = 2.**ISCALE
         ENDIF
C
C
C
C
C***
C*    GET NUMBER OF BITS IN EACH DATA VALUE.
C***
C
      CALL GBYTE (IGRIB(IWORD),IBITS,IOFF,8)
      CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
      IF (IERR.NE.0) RETURN
C
C
C     CHECK NUMBER OF BITS PER DATA FIELD.
C
      IF (IBITS.GT.NBIT.OR.IBITS.GT.IMAX)
     C    THEN
              IERR = -1
              WRITE (*,9002) IBITS,NBIT,IMAX
 9002         FORMAT (1H ,'NUMBER OF BITS PER DATA VALUE, ',I3,
     C                'EXCEEDS WORD LENGTH, ',I3,' OR MAXIMUM ',
     C                ' PERMITTED VALUE, ',I3)
              RETURN
          ENDIF
C
C***
C*    IF DATA IS IN SPHERICAL HARMONIC FORM, NEXT 4 OCTETS
C*    CONTAIN THE REAL (0,0) COEFFICIENT.
C***
C
      IF (IREP.EQ.1)
     C    THEN
C
C            GET REAL (0,0) COEFFICIENT IN GRIB FORMAT AND
C            CONVERT TO FLOATING POINT.
C
             CALL GBYTE (IGRIB(IWORD),IEXP,IOFF,8)
             CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
             CALL GBYTE (IGRIB(IWORD),IMANT,IOFF,24)
             CALL OFFSET (IOFF,1,IWORD,24,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
             IF (IMISS.EQ.1)
     C          THEN
                    FPDATA(1) = 0.0
                ELSE
                    CALL DECFP (FPDATA(1),IEXP,IMANT)
                ENDIF
          ENDIF
C
C
C
C
C*****
C*    DECODE DATA VALUES TO FLOATING POINT AND STORE IN FPDATA.
C*****
C
C     FIRST CALCULATE THE NUMBER OF DATA VALUES.
C
      JLENF = LBIN - 11 - IREP * 4
      JLENF = (JLENF * 8 - LNIL) / IBITS
C
C     CHECK LENGTH OF OUTPUT ARRAY.
C
      IF (JLENF+IREP.GT.ILENF)
     C   THEN
             IERR = -3
             WRITE (*,9003) JLENF,ILENF
 9003        FORMAT (1H ,'NUMBER OF VALUES TO BE DECODED IS - ',I7,
     C                    ', ARRAY SIZE - ',I7)
             RETURN
         ENDIF
C
      CALL GBYTES (IGRIB(IWORD),FPDATA(IREP+1),IOFF,IBITS,0,JLENF)
      CALL OFFSET (IOFF,JLENF,IWORD,IBITS,NBIT,ILENG,IERR)
      IF (IERR.NE.0) RETURN
C
      DO 500 I= IREP+1,IREP+JLENF
            IF (IMISS.EQ.1)
     C         THEN
                   FPDATA(1) = 0.0
               ELSE
                   ZGRIB     = FPDATA (I)
                   FPDATA(I) = FMIN + JGRIB * ZSCALE
               ENDIF
  500 CONTINUE
C
C     INCLUDE (0,0) COEFFICIENT IN COUNT, IF SPHERICAL HARMONIC DATA.
C
      JLENF = IREP + JLENF
C
C
C
C
C***
C*    SKIP ANY ZERO FILL AT END OF DATA BLOCK.
C***
C
      L = (IWORD-IPW) * NBIT + IOFF -IPB
      L = LBIN * 8 - L
      IF (L.NE.0)
     C    THEN
              CALL OFFSET (IOFF,1,IWORD,L,NBIT,ILENG,IERR)
              IF (IERR.NE.0) RETURN
          ENDIF
C
C      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C*         IF BIT MAP PRESENT FILL MISSING VALUES BY RMISS.
C
      IF(IB1PAR(4).EQ.64.OR.IB1PAR(4).EQ.192) THEN
C
         JI=0
         DO 647 J=1,IBVAL
         IF(IBMAP(J).EQ.0) THEN
            FPDATAG(J)=RMISS
         ELSE
            JI=JI+1
            FPDATAG(J)=FPDATA(JI)
         END IF
 647     CONTINUE
C
         JLENF = IBVAL
         DO 649 I=1,JLENF
         FPDATA(I)=FPDATAG(I)
 649     CONTINUE
C
      END IF
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
      CALL GBYTES (IGRIB(IWORD),IBLOCK(1),IOFF,8,0,4)
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
      IF (IMISS.EQ.1) JLENF = - JLENF
C
      RETURN
      END
