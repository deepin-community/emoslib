C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C


      SUBROUTINE CODEGB (FPDATA,ILENF,IBITS,NBIT,IB1PAR,
     C              IB2PAR,FVERT,ILENV,IGRIB,ILENG,IWORD,
     C              IROUND,IERR)
C
C---->
C*
C*     NAME     : CODEGB
C*
C*     FUNCTION : CODE ARRAY OF FLOATING POINT VALUES
C*                IN WMO GRIB CODE.
C*
C*     INPUT    : PARAMETERS FOR BLOCK 1 , PRODUCT DEFINITION BLOCK.
C*                --------------------------------------------------
C*                IB1PAR - INTEGER ARRAY (DIMENSION 18) OF PRODUCT
C*                         DEFINITION INFORMATION.
C*
C*                WORD      CONTENTS
C*                ----      --------
C*                  1       ORIGINATING CENTRE IDENTIFIER.
C*                  2       MODEL IDENTIFICATION.
C*                  3       GRID DEFINITION.
C*                  4       FLAG ( CODE TABLE 1 ).
C*                  5       PARAMETER IDENTIFIER (CODE TABLE 2).
C*                  6       TYPE OF LEVEL (CODE TABLE 3).
C*                 7-8      VALUE(S) OF LEVEL (CODE TABLE 3).
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
C*                          VALUE(S) OF LEVEL CAN OCCUPY 2 WORDS.
C*                          FOR A LAYER THE FIRST WORD DEFINES THE TOP
C*                          AND THE SECOND THE BOTTOM OF THE LAYER.
C*                          FOR A SINGLE LEVEL, ONLY THE FIRST WORD IS
C*                          USED.
C*
C*                PARAMETERS FOR BLOCK 2 , GRID DEFINITION BLOCK.
C*                -----------------------------------------------
C*                IB2PAR - INTEGER ARRAY (DIMENSION 11) CONTAINING GRID
C*                         DEFINITION INFORMATION.
C*                         USE VARIES WITH REPRESENTATION TYPE.
C*
C*                WORD      LAT/LONG GRID
C*                ----      -------------
C*                 1        DATA REPRESENTATION TYPE (CODE TABLE 6)
C*                 2        NO. OF POINTS ALONG A LATITUDE
C*                 3        NO. OF POINTS ALONG A MERIDIAN
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
C*                TO BE DEFINED LATER.
C*
C*                FVERT  - REAL ARRAY OF VERTICAL COORDINATE PARAMETERS
C*                ILENV  - LENGTH OF THIS ARRAY (NUMBER OF COORDINATE
C*                         PARAMETERS).
C*
C*                USED FOR HYBRID LEVELS ONLY.
C*
C*
C*                PARAMETERS FOR BLOCK 3 , BIT-MAP DEFINITION BLOCK.
C*                --------------------------------------------------
C*
C*                TO BE DEFINED LATER.
C*
C*                PARAMETERS FOR BLOCK 4 , BINARY DATA BLOCK.
C*                -------------------------------------------
C*                FPDATA  - ARRAY OF FLOATING POINT VALUES.
C*
C*                          ****
C*                              VALUES IN THIS ARRAY ARE OVERWRITTEN
C*                              BY THE SUBROUTINE.
C*                                                              ****
C*
C*                ILENF   - LENGTH OF THIS ARRAY (NUMBER OF DATA
C*                          VALUES TO BE CODED)
C*                IBITS   - NUMBER OF BITS FOR CODED DATA VALUES.
C*
C*                          TO INDICATE MISSING DATA VALUES, ILENF IS
C*                          MADE NEGATIVE AND ALL DATA VALUES SET TO 0.
C*
C*                OTHER PARAMETERS
C*                ----------------
C*
C*                NBIT    - NUMBER OF BITS IN COMPUTER WORD.
C*
C*                IGRIB   - ARRAY TO RECEIVE DATA IN GRIB CODE.
C*                ILENG   - LENGTH OF THIS ARRAY.
C*
C*                IROUND  - 0 , NO ROUNDING.
C*                          1 , ROUND TO NEAREST 120 OCTETS.
C*
C*    OUTPUT    : IGRIB   - DATA CODED IN GRIB CODE.
C*                IWORD   - NUMBER OF WORDS OCCUPIED BY CODED DATA.
C*
C*                IERR    - ERROR INDICATOR.
C*
C*                           0, NO ERROR.
C*
C*                          -1, NUMBER OF BITS PER DATA VALUE EXCEEDS
C*                              WORD LENGTH, OR MAXIMUM ALLOWED.
C*                          -2, OUTPUT ARRAY TOO SMALL TO HOLD CODED
C*                              DATA VALUES.
C*                          -3, CURRENTLY UNDEFINED OPTION SELECTED.
C*                          -4, MISSING DATA FIELD CONTAINS NON-ZERO.
C*
C*                           1, INVALID ORIGINATING CENTRE.
C*                           2, INVALID MODEL IDENTIFICATION.
C*                           3, INVALID GRID DEFINITION.
C*                           4, INVALID BLOCK INDICATOR FLAG
C*                           5, INVALID PARAMETER IDENTIFIER.
C*                           6, INVALID LEVEL TYPE INDICATOR.
C*                           7, ERROR IN DESCRIPTION OF LEVELS.
C*                           8, DATE/TIME ERROR.
C*                           9, INVALID TIME UNIT OR RANGE.
C*                          10, INVALID DATA REPRESENTATION TYPE.
C*                          11, INVALID EXTREME POINT/INCREMENT FLAG.
C*                          12, INVALID SCANNING MODE FLAGS.
C*
C*    JOHN HENNESSY , ECMWF , NOVEMBER 1985
C*
C     Modifications.
C     -------------
C
C          J. Hennessy ECMWF 08:10:90
C          Use temporary array IDATB to enable loop vectorisation.
C          Add M. Hamrud's modifications to us a vectorising version of
C          SBYTES.
C
C          J. Hennessy ECMWF 29:11:90
C          Pad last bytes in the word after 7777 correctly with
C          binary zeroes.
C
C          J. Hennessy ECMWF 19:11:92
C          Dimension of IDATB increased to 300000.
C
C----<
C     ---------------------------------------------------------------
C
C
C
C
C
      DIMENSION FPDATA(*),ILAT(2)
c--      DIMENSION IDATB(ILENF)
      DIMENSION IDATB(300000)
      DIMENSION IMASK(100)
      DIMENSION IGRIB(ILENG),IBLOCK(24)
c--      DIMENSION FVERT(ILENV),IB1PAR(18),IB2PAR(11)
      DIMENSION FVERT(ILENV),IB1PAR(*),IB2PAR(*)
C
      EQUIVALENCE (ZGRIB,JGRIB)
C
C
C     SET MAXIMUM NUMBER OF BITS PER DATA FIELD.
C
      DATA IMAX /60/
C
C     CLEAR ERROR INDICATOR.
C
      IERR = 0
C
C
C
C
C
C********************************************************************
C*
C*    CHECK FOR MISSING DATA INDICATORS.
C*
C********************************************************************
C
C
      IMISS = 0
      IF (ILENF.LT.0)
     C   THEN
             KLENF = - ILENF
             IMISS = 1
             DO 50 I=1,KLENF
                IF (FPDATA(I).NE.0.)
     C             THEN
                       IERR = -4
                       WRITE (*,9012)
 9012                  FORMAT (1H ,'NON-ZERO VALUE IN MISSING DATA',
     C                          ' FIELD.')
                       RETURN
                   ENDIF
   50        CONTINUE
         ELSE
             KLENF = ILENF
         ENDIF
C
C
C
C
C********************************************************************
C*
C*    CHECK NUMBER OF BITS PER DATA FIELD.
C*
C********************************************************************
C
C
      IF (IBITS.GT.NBIT.OR.IBITS.GT.IMAX)
     C    THEN
              IERR = -1
              WRITE (*,9000) IBITS,NBIT,IMAX
 9000         FORMAT (1H ,'NUMBER OF BITS PER DATA VALUE, ',I3,
     C                'EXCEEDS WORD LENGTH, ',I3,' OR MAXIMUM ',
     C                ' PERMITTED VALUE, ',I3)
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
C
C
C********************************************************************
C*
C*    BLOCK 0 - INDICATOR BLOCK.
C*
C********************************************************************
C
C
C
C***
C*    THE LETTERS G R I B ARE INSERTED IN THE
C*    FIRST 4 ELEMENTS OF 'IBLOCK'.
C***
C
      IBLOCK(1) = 71
      IBLOCK(2) = 82
      IBLOCK(3) = 73
      IBLOCK(4) = 66
C
      IWORD = 1
      IBYTE = 8
      NVAL  = 4
      IOFF  = 0
C
C     PACK 4 8-BIT FIELDS IN BLOCK 0 OF CODED ARRAY.
C
      CALL SBYTES (IGRIB(IWORD),IBLOCK(1),IOFF,IBYTE,0,NVAL)
      CALL OFFSET (IOFF,NVAL,IWORD,IBYTE,NBIT,ILENG,IERR)
      IF (IERR.NE.0) RETURN
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
C*    BLOCK 1 - PRODUCT DEFINITION BLOCK.
C*
C********************************************************************
C
C
C
C***
C*    LENGTH OF BLOCK 1 (IN OCTETS) IN FIRST 3 OCTETS.
C***
C
      IBLOCK(1) = 0
      IBLOCK(2) = 0
      IBLOCK(3) = 24
C
C
C
C
C***
C*    FOURTH OCTET RESERVED AND SET TO 0.
C***
C
      IBLOCK(4) = 0
C
C
C
C
C
C
C***
C*    IDENTIFICATION OF CENTRE
C***
      IBLOCK(5) = IB1PAR(1)
      IF (IB1PAR(1).LT.1.OR.IB1PAR(1).GT.98)
     C   THEN
             IERR = 1
             WRITE (*,9001) IB1PAR(1)
 9001        FORMAT (1H ,'INVALID ORIGINATING CENTRE ',I3)
             RETURN
         ENDIF
C
C
C
C
C
C
C
C***
C*    MODEL IDENTIFICATION
C***
C
      IBLOCK(6) = IB1PAR(2)
      IF (IB1PAR(2).LT.1.OR.IB1PAR(2).GT.255)
     C   THEN
             IERR = 2
             WRITE (*,9002) IB1PAR(2)
 9002        FORMAT (1H ,'INVALID MODEL IDENTIFICATION ',I4)
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
C*    GRID DEFINITION
C***
C
      IBLOCK(7) = IB1PAR(3)
      IF (IB1PAR(3).LT.1.OR.IB1PAR(3).GT.255)
     C   THEN
             IERR = 3
             WRITE (*,9003) IB1PAR(3)
 9003        FORMAT (1H ,'INVALID GRID IDENTIFICATION ',I4)
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
C*    OPTIONAL BLOCK INDICATOR FLAGS ( CODE TABLE 1 ).
C***
C
C     BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
C
C         NONE             00000000               0
C          2               10000000             128
C          3               01000000              64
C         2 AND 3          11000000             192
C
      IBLOCK(8) = IB1PAR(4)
C
C     SHIFT 2-BIT FLAG FIELD RIGHT BY 6 BITS TO FACILITATE CHECKING
C
      KTEMP = IB1PAR(4) / 64
C
      IF (KTEMP.LT.0.OR.KTEMP.GT.3)    IERR = 4
C
C     CHECK IF GRID SPECIFIED IS UNCATALOGUED.
C
      IF (IB1PAR(3).EQ.255.AND.IB1PAR(4).EQ.0)  IERR = 4
      IF (IB1PAR(3).EQ.255.AND.IB1PAR(4).EQ.64) IERR = 4
C
      IF (IERR.NE.0)
     C   THEN
             CALL PRTBIN (IB1PAR(4),8,KTEMP,ERR)
             WRITE (*,9004) KTEMP
 9004        FORMAT (1H ,'INVALID BLOCK INDICATOR FLAG ',I8.8)
             RETURN
         ENDIF
C
C
C
C
C***
C*    PARAMETER IDENTIFIER ( CODE TABLE 2 ).
C***
C
      IBLOCK(9) = IB1PAR(5)
      IF (IB1PAR(5).LT.0.OR.IB1PAR(5).GT.255)
     C   THEN
             IERR = 5
             WRITE (*,9005) IB1PAR(5)
 9005        FORMAT (1H ,'INVALID PARAMETER ',I4)
             RETURN
         ENDIF
C
C
C
C
C***
C*    INDICATOR OF LEVEL TYPE ( CODE TABLE 3 ).
C***
C
      IBLOCK(10) = IB1PAR(6)
C
C*    CODE FIGURES FOR LEVEL TYPE RANGE FROM 0 - 110, WITH 200
C     BEING USED FOR PSEUDO-LEVELS.
C
      IF (IB1PAR(6).LT.0.OR.IB1PAR(6).GT.110) IERR = 6
      IF (IB1PAR(6).EQ.200) IERR = 0
      IF (IERR.EQ.6)
     C   THEN
             WRITE (*,9006) IB1PAR(6)
 9006        FORMAT (1H ,'INVALID LEVEL TYPE ',I4)
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
C
C
C
C
C
C
C
C
C***
C*    DESCRIPTION OF LEVEL OR LAYER ( CODE TABLE 3 ).
C***
C
C
C
C     FOR CERTAIN SPECIAL LEVELS NO FURTHER DESCRIPTION IS
C     NECESSARY, AND THE FIELDS ARE SET TO 0.
C
      IF (IB1PAR(6).LT.100.OR.IB1PAR(6).EQ.102)
     C    THEN
              IB1PAR(7) = 0
              IB1PAR(8) = 0
          ENDIF
C
      IBLOCK(11) = IB1PAR(7)
      IBLOCK(12) = IB1PAR(8)
C
C     CERTAIN LEVEL TYPES REQUIRE THAT THE DESCRIPTION OCCUPY
C     BOTH 8 BIT FIELDS.
C
      IF (IB1PAR(6).EQ.100.OR.IB1PAR(6).EQ.103.OR.IB1PAR(6).EQ.105.
     C    OR.IB1PAR(6).EQ.107.OR.IB1PAR(6).EQ.109)
     C    THEN
C             8 LOW ORDER BITS IN IBLOCK(12)
C             HIGH ORDER BITS IN IBLOCK(11)
C
              IBLOCK(12) = IBLOCK(11)
              IBLOCK(11) = IBLOCK(11) / 256
              IBLOCK(12) = IBLOCK(12) - IBLOCK(11) * 256
C
C             CHECK MAXIMUM POSSIBLE IN 16 BITS.
C
              IF (IB1PAR(7).GT.65535) IERR = 7
          ELSE
              IF (IB1PAR(7).GT.255.OR.IB1PAR(8).GT.255) IERR = 7
          ENDIF
C
      IF (IERR.NE.0)
     C    THEN
              WRITE (*,9007) IB1PAR(7),IB1PAR(8)
 9007         FORMAT (1H ,'LEVEL DESCRIPTION ERROR ',I8,3X,I8)
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
C*    DATE AND TIME. CHECK VALID RANGES AND MISSING DATA VALUES.
C***
C
      IBLOCK(13) = IB1PAR(9)
      IERY = 0
      IF (IB1PAR(9).LT.0.OR.IB1PAR(9).GT.99) IERY = 1
      IF (IB1PAR(9).EQ.255) IERY = 0
      IBLOCK(14) = IB1PAR(10)
      IERM = 0
      IF (IB1PAR(10).LT.1.OR.IB1PAR(10).GT.12) IERM = 1
      IF (IB1PAR(10).EQ.255) IERM = 0
      IBLOCK(15) = IB1PAR(11)
      IERD = 0
      IF (IB1PAR(11).LT.1.OR.IB1PAR(11).GT.31) IERD = 1
      IF (IB1PAR(11).EQ.255) IERD = 0
      IBLOCK(16) = IB1PAR(12)
      IERH = 0
      IF (IB1PAR(12).LT.0.OR.IB1PAR(12).GT.23) IERH = 1
      IF (IB1PAR(12).EQ.255) IERH = 0
      IBLOCK(17) = IB1PAR(13)
      IERN = 0
      IF (IB1PAR(13).LT.0.OR.IB1PAR(13).GT.59) IERN = 1
      IF (IB1PAR(13).EQ.255) IERN = 0
C
      IERR = IERY + IERM + IERD + IERH + IERN
C
      IF (IERR.NE.0)
     C    THEN
              IERR = 8
              WRITE (*,9008) IB1PAR(9),IB1PAR(10),IB1PAR(11),IB1PAR(12),
     C                       IB1PAR(13)
 9008         FORMAT (1H ,'INVALID DATE/TIME ',3I2,' / ',2I2)
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
C*    UNIT OF TIME - ( CODE TABLE 4 ).
C*    TIME RANGE(S) AND TIME RANGE INDICATOR ( CODE TABLE 5 ).
C***
C
C     TIME UNIT.
C
      IBLOCK(18) = IB1PAR(14)
      IF (IB1PAR(14).LT.0.OR.IB1PAR(14).GT.7) IERR = 9
C
C*    TIME UNIT CODES ARE IN THE RANGE 0 - 7, WITH 254 USED FOR SECONDS.
C
      IF (IB1PAR(14).EQ.254) IERR = 0
C
C     THERE CAN BE ONE OR TWO TIME RANGES, EACH IN 1 OCTECT OR
C     ONE TIME RANGE IN 2 OCTECTS.
C
      IF (IB1PAR(17).EQ.0.OR.IB1PAR(17).EQ.1
     C .OR.IB1PAR(17).EQ.10)   IB1PAR(16)  = 0
C
C     CROSS CHECK RANGE VALUES AND FLAG VALUES.
C
      IF (IB1PAR(15).GT.65535.OR.IB1PAR(15).LT.0)   IERR = 9
      IF (IB1PAR(15).GT.255.AND.IB1PAR(17).NE.10)   IERR = 9
      IF (IB1PAR(16).GT.255.OR.IB1PAR(16).LT.0)     IERR = 9
      IF (IB1PAR(17).LT.0.OR.IB1PAR(17).GT.10)      IERR = 9
      IF (IERR.NE.0)
     C    THEN
              WRITE (*,9009) IB1PAR(14),IB1PAR(15),IB1PAR(16),IB1PAR(17)
 9009         FORMAT (1H ,'TIME UNIT/TIME 1/TIME 2/INDICATOR ERROR - ',
     C                I8,2X,'/',I8,2X,'/',I8,2X,'/',I8)
              RETURN
          ENDIF
C
      IBLOCK(19) = IB1PAR(15)
      IBLOCK(20) = IB1PAR(16)
C
C     ONE TIME RANGE OCCUPYING BOTH OCTETS.
C
      IF (IB1PAR(17).EQ.10)
     C   THEN
C            8 LOW ORDER BITS IN IBLOCK(20)
C            HIGH ORDER BITS IN IBLOCK(19)
C
             IBLOCK(20) = IBLOCK(19)
             IBLOCK(19) = IBLOCK(19) / 256
             IBLOCK(20) = IBLOCK(20) - IBLOCK(19) * 256
         ENDIF
C
C     TIME RANGE INDICATOR
C
      IBLOCK(21) = IB1PAR(17)
C
C
C
C
C
C
C
C***
C*    NUMBER AVERAGED.
C***
C
      IBLOCK(22) = 0
      IBLOCK(23) = 0
C
C
C     CHECK CONSISTENCY INDICATOR AND NUMBER FIELD.
C
      IF (IB1PAR(17).EQ.3.AND.IB1PAR(18).EQ.0)
     C   THEN
             IERR = 12
             WRITE (*,9013) IB1PAR(17),IB1PAR(18)
 9013        FORMAT (1H ,'INDICATOR/NUMBER AVERAGED ERROR - ',
     C                I8,2X,'/',I8)
             RETURN
         ENDIF
C
C     VALUE IN TWO OCTETS.
C
      IBLOCK(22) = IB1PAR(18)
C
C     8 LOW ORDER BITS IN IBLOCK(23)
C     HIGH ORDER BITS IN IBLOCK(22)
C
      IBLOCK(23) = IBLOCK(22)
      IBLOCK(22) = IBLOCK(22) / 256
      IBLOCK(23) = IBLOCK(23) - IBLOCK(22) * 256
C
C
C
C
C
C
C***
C*    RESERVED ELEMENT.
C***
C
      IBLOCK(24) = 0
C
C
C
C
C
C
C
C***
C*
C*    PACK 24 8-BIT FIELDS OF BLOCK 1 IN CODED ARRAY.
C*
C***
C
      IBYTE = 8
      NVAL = 24
C
      CALL SBYTES (IGRIB(IWORD),IBLOCK(1),IOFF,IBYTE,0,NVAL)
      CALL OFFSET (IOFF,NVAL,IWORD,IBYTE,NBIT,ILENG,IERR)
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
C
C
C
C***
C*    SET ARRAY IBLOCK TO 0 . IT IS USED TO ZERO FILL RESERVED OCTETS.
C***
C
      DO 100 I=1,24
         IBLOCK(I) = 0
  100 CONTINUE
C
C
C
C
C***
C*    LAT/LONGITUDE GRID, GAUSSIAN GRID AND SPHERICAL HARMONICS
C*    ARE THE ONLY DATA REPRESENTATIONS HANDLED.
C***
C
      IF (IB1PAR(4).EQ.128.OR.IB1PAR(4).EQ.192)
     C   THEN
             IF (IB2PAR(1).NE.0.AND.IB2PAR(1).NE.4.AND.IB2PAR(1).NE.50)
     C          THEN
                     WRITE (*,*)'GRID DESCRIPTION BLOCK NOT YET DEFINED'
                     IERR = -3
                     RETURN
                ENDIF
C
C
C
C
C***
C*       LENGTH OF GRID DESCRIPTION BLOCK.
C***
C
C        LENGTH IS 32 OCTETS FOR LAT/LONG, GAUSSIAN AND SPHERICAL
C        HARMONICS . FOR ANY DATA  ON HYBRID LEVELS THE
C        VERTICAL COORDINATES ARE ADDED.
C
         I = 32
         IF (IB1PAR(6).GT.108) I = I + ILENV * 4
C
         CALL SBYTE (IGRIB(IWORD),I,IOFF,24)
         CALL OFFSET (IOFF,1,IWORD,24,NBIT,ILENG,IERR)
         IF (IERR.NE.0) RETURN
C
C
C
C
C***
C*       NUMBER OF UNUSED BITS AT END OF BLOCK.
C*       CURRENT DEFINITION IMPLIES 0.
C***
C
         CALL SBYTE (IGRIB(IWORD),IBLOCK(1),IOFF,8)
         CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
         IF (IERR.NE.0) RETURN
C
C
C
C
C
C***
C*       NEXT OCTET IS RESERVED.
C***
C
         CALL SBYTE (IGRIB(IWORD),IBLOCK(1),IOFF,8)
         CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
         IF (IERR.NE.0) RETURN
C
C
C
C
C
C
C***
C*       DATA REPRESENTATION TYPE.
C***
C
         IF (IB2PAR(1).LT.0.OR.IB2PAR(1).GT.4) IERR = 10
         IF (IB2PAR(1).EQ.50) IERR = 0
         IF (IERR.NE.0)
     C      THEN
                IERR = 10
                WRITE (*,9010) IB2PAR(1)
 9010           FORMAT (1H ,'INVALID DATA REPRESENTATION TYPE - ',I3)
                RETURN
            ENDIF
         CALL SBYTE (IGRIB(IWORD),IB2PAR(1),IOFF,8)
         CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
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
C*       LAT/LONG OR GAUSSIAN GRID.
C***
C
         IF (IB2PAR(1).EQ.0.OR.IB2PAR(1).EQ.4)
     C    THEN
C
C            NUMBER OF LAT/LONG POINTS.
C
             CALL SBYTES (IGRIB(IWORD),IB2PAR(2),IOFF,16,0,2)
             CALL OFFSET (IOFF,2,IWORD,16,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
C
C            LAT/LONG OF ORIGIN.
C            SIGN BIT SET TO 1 IF VALUES ARE NEGATIVE.
C
             DO 200 I=1,2
                ILAT(I) = IB2PAR(I+3)
                ISIGN   = 0
                IF (ILAT(I).LT.0)
     C            THEN
                      ISIGN   = 8388608
                      ILAT(I) = - ILAT(I)
                  ENDIF
                ILAT(I) = ILAT(I) + ISIGN
  200        CONTINUE
C
             CALL SBYTES (IGRIB(IWORD),ILAT(1),IOFF,24,0,2)
             CALL OFFSET (IOFF,2,IWORD,24,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
C
C            RESOLUTION FLAG.
C
C            INCREMENTS       BINARY CODE     DECIMAL VALUE
C
C             NOT GIVEN        00000000              0
C              GIVEN           10000000            128
C
C
C            SHIFT 1 BIT FLAG FIELD TO LOW ORDER BIT
C
             KTEMP = IB2PAR(6) / 128
C
C            CHECK ON VALID RESOLUTION FLAG FIELD IS IGNORED IF
C            TYPE OF LEVEL IS GIVEN AS 200 - USED BY ECMWF FOR
C            PACKING DIAGNOSTIC FIELDS IN PSEUDO GRIB.
C
             IF (KTEMP.LT.0.OR.KTEMP.GT.1.AND.IB1PAR(6).NE.200)
     C          THEN
                    CALL PRTBIN (IB2PAR(6),8,KTEMP,ERR)
                    WRITE (*,9011) KTEMP
 9011               FORMAT (1H ,'INVALID RESOLUTION FLAG ',I8.8)
                    IERR = 11
                    RETURN
                ENDIF
C
C            SET TO ALL 1-BITS UNUSED INCREMENT FIELDS.
C
             IF (IB2PAR(6).EQ.0)
     C          THEN
C                   1111111111111111 BINARY = 65535 DECIMAL
                    IB2PAR(9)  = 65535
                    IB2PAR(10) = 65535
                ENDIF
C
             CALL SBYTE (IGRIB(IWORD),IB2PAR(6),IOFF,8)
             CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
C
C            LAT/LONG OF EXTREME POINT.
C
C            SIGN BIT SET TO 1 IF VALUES ARE NEGATIVE.
C
             DO 300 I=1,2
                ILAT(I) = IB2PAR(I+6)
                ISIGN   = 0
                IF (ILAT(I).LT.0)
     C            THEN
                      ISIGN   = 8388608
                      ILAT(I) = - ILAT(I)
                  ENDIF
                ILAT(I) = ILAT(I) + ISIGN
  300        CONTINUE
C
             CALL SBYTES (IGRIB(IWORD),ILAT(1),IOFF,24,0,2)
             CALL OFFSET (IOFF,2,IWORD,24,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
C
C
C
C            DIRECTION INCREMENTS / NUMBER OF LATITUDE LINES.
C
             CALL SBYTES (IGRIB(IWORD),IB2PAR(9),IOFF,16,0,2)
             CALL OFFSET (IOFF,2,IWORD,16,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
C
C            SCANNING MODE FLAGS.
C
C            VALID VALUES       VALUE / 32
C             BINARY             DECIMAL
C
C              00000000               0
C              10000000               4
C              01000000               2
C              11000000               6
C              00100000               1
C              10100000               5
C              01100000               3
C              11100000               7
C
C            SHIFT 3 BIT FLAG FIELD TO LOW ORDER BITS.
C
             KTEMP = IB2PAR(11) / 32
C
C            CHECK ON VALID SCANNING MODE FLAG FIELD IS IGNORED IF
C            TYPE OF LEVEL IS GIVEN AS 200 - USED BY ECMWF FOR
C            PACKING DIAGNOSTIC FIELDS IN PSEUDO GRIB.
C
             IF (KTEMP.LT.0.OR.KTEMP.GT.7.AND.IB1PAR(6).NE.200)
C
     C          THEN
                    CALL PRTBIN (IB2PAR(11),8,KTEMP,ERR)
                    WRITE (*,9014) KTEMP
 9014               FORMAT (1H ,'INVALID SCANNING MODE FLAGS ',I8.8)
                    IERR = 12
                    RETURN
                ENDIF
C
             CALL SBYTE (IGRIB(IWORD),IB2PAR(11),IOFF,8)
             CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
C
C            4 RESERVED OCTETS.
C
             CALL SBYTES (IGRIB(IWORD),IBLOCK(1),IOFF,8,0,4)
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
C***
C*       SPHERICAL HARMONIC DATA.
C***
C
         IF (IB2PAR(1).EQ.50)
     C    THEN
C
C            PENTAGONAL RESOLUTION PARAMETERS.
C
             CALL SBYTES (IGRIB(IWORD),IB2PAR(2),IOFF,16,0,3)
             CALL OFFSET (IOFF,3,IWORD,16,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
C
C            REPRESENTATION TYPE AND MODE.
C
             CALL SBYTES (IGRIB(IWORD),IB2PAR(5),IOFF,8,0,2)
             CALL OFFSET (IOFF,2,IWORD,8,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
C
C            18 RESERVED OCTETS.
C
             CALL SBYTES (IGRIB(IWORD),IBLOCK(1),IOFF,8,0,18)
             CALL OFFSET (IOFF,18,IWORD,8,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
C
          ENDIF
C
C
C
C
C***
C*       ADD VERTICAL COORDINATE PARAMETERS FOR HYBRID LEVELS.
C***
C
         IF (IB1PAR(6).GT.108)
     C    THEN
             DO 400 I=1,ILENV
                 CALL CONFP (FVERT(I),IEXP,IMANT)
                 CALL SBYTE (IGRIB(IWORD),IEXP,IOFF,8)
                 CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
                 IF (IERR.NE.0) RETURN
                 CALL SBYTE (IGRIB(IWORD),IMANT,IOFF,24)
                 CALL OFFSET (IOFF,1,IWORD,24,NBIT,ILENG,IERR)
                 IF (IERR.NE.0) RETURN
  400        CONTINUE
C
          ENDIF
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
C********************************************************************
C*
C*    BLOCK 3 (IF REQUIRED) - BIT MAP BLOCK.
C*
C********************************************************************
C
      IF (IB1PAR(4).EQ.64.OR.IB1PAR(4).EQ.192)
     C     THEN
               WRITE (*,*)'BIT MAP BLOCK NOT YET DEFINED'
               IERR = -3
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
C*    BLOCK 4 - BINARY DATA BLOCK.
C*
C********************************************************************
C
C
C***
C*    RETAIN POINTERS TO WORD AND BIT POSITION OF BINARY DATA
C*    BLOCK LENGTH FIELD. ENTER LENGTH AS 0.
C***
C
      IPW = IWORD
      IPB = IOFF
C
      CALL SBYTE (IGRIB(IWORD),IBLOCK(1),IOFF,24)
      CALL OFFSET (IOFF,1,IWORD,24,NBIT,ILENG,IERR)
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
C
C
C***
C*    4 BIT FLAG / 4 BIT COUNT OF UNUSED BITS AT END OF BLOCK OCTET.
C***
C
C     FLAG IS 1000 FOR SPHERICAL HARMONICS, 0000 FOR LAT/LONG
C     OR GAUSSIAN GRID.
C
      IREP = 0
      IF (IB2PAR(1).EQ.50) IREP = 1
C
C     FLAG FIELD IS IREP SHIFTED LEFT BY 7 BITS
C
      IFLAG = IREP * 128
C
      CALL SBYTE (IGRIB(IWORD),IFLAG,IOFF,8)
      CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
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
C
C
C***
C*    FIND MAXIMUM AND MINIMUM VALUES IN DATA ARRAY. FOR
C*    DATA IN SPHERICAL HARMONIC FORM THE FIRST WORD CONTAINS
C*    THE REAL (0,0)COEFFICIENT, WHICH IS TREATED SEPARATELY.
C***
C
C
      ILEN = KLENF - IREP
C--      CALL MXMN (FPDATA(IREP+1),ILEN,FMAX,FMIN)
      CALL MAXMIN (FPDATA(IREP+1),ILEN,FMAX,FMIN)
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
C***
C*    SCALE FACTOR.
C***
C
C     CALCULATE SCALE FACTOR
C
      S = (FMAX-FMIN)/(2**(IBITS+1)-1)
      IF (S.NE.0.) S = ALOG(S)/ALOG(2.) + 2.
      ISCALE = MIN(INT(S),INT(S+SIGN(1.,S)))
C
C
C
C
C     SET SIGN BIT (BIT 16) AND PUT SCALE FACTOR INTO REMAINING
C     15 BITS OF CODED 16 BIT FIELD.
C
      JSCALE = ISCALE
      ISIGN = 0
      IF (JSCALE.LT.0)
     C    THEN
              ISIGN  = 32768
              JSCALE = - JSCALE
          ENDIF
      JSCALE = JSCALE + ISIGN
C
      ZSCALE = 2.**ISCALE
C
C     FOR MISSING DATA SCALE FACTOR IS SET TO ALL 1-BITS.
C
      IF (IMISS.EQ.1) JSCALE = 65535
C
      CALL SBYTE (IGRIB(IWORD),JSCALE,IOFF,16)
      CALL OFFSET (IOFF,1,IWORD,16,NBIT,ILENG,IERR)
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
C*    MINIMUM (REFERENCE) VALUE.
C***
C
C     CONVERT MINIMUM VALUE (FMIN) TO GRIB FORMAT (IEXP,IMANT)
C
      CALL CONFP (FMIN,IEXP,IMANT)
C
C     EXPONENT AND MANTISSA SET TO ALL 1-BITS FOR MISSING DATA.
C
      IF (IMISS.EQ.1)
     C   THEN
             IEXP  = 255
             IMANT = 16777215
         ENDIF
      CALL SBYTE (IGRIB(IWORD),IEXP,IOFF,8)
      CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
      IF (IERR.NE.0) RETURN
      CALL SBYTE (IGRIB(IWORD),IMANT,IOFF,24)
      CALL OFFSET (IOFF,1,IWORD,24,NBIT,ILENG,IERR)
      IF (IERR.NE.0) RETURN
C
C
C
C
C***
C*    NUMBER OF BITS IN EACH DATA VALUE.
C***
C
      CALL SBYTE (IGRIB(IWORD),IBITS,IOFF,8)
      CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
      IF (IERR.NE.0) RETURN
C
C
C
C
C***
C*    IF SPHERICAL HARMONICS DATA, NEXT 4 OCTETS ARE DIFFERENT FORMAT.
C***
C
      IF (IREP.EQ.1)
     C    THEN
C
C            STORE REAL (0,0) COEFFICIENT IN FLOATING
C            POINT FORM.
C
             CALL CONFP (FPDATA(1),IEXP,IMANT)
C
             CALL SBYTE (IGRIB(IWORD),IEXP,IOFF,8)
             CALL OFFSET (IOFF,1,IWORD,8,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
             CALL SBYTE (IGRIB(IWORD),IMANT,IOFF,24)
             CALL OFFSET (IOFF,1,IWORD,24,NBIT,ILENG,IERR)
             IF (IERR.NE.0) RETURN
          ENDIF
C
C
C
C
C***
C*    SCALE AND STORE DATA VALUES.
C***
C
      DO 600 I = IREP+1 , KLENF
C--         JGRIB     = NINT ( (FPDATA(I)-FMIN) / ZSCALE )
C--         FPDATA(I) = ZGRIB
         IDATB(I) = NINT ( (FPDATA(I)-FMIN) / ZSCALE )
  600 CONTINUE
C
      ILENFM = KLENF - IREP
      IMASK(2) = 0
      IOFFX    = IOFF
      CALL GSBITE (IGRIB(IWORD),IDATB(IREP+1),IOFFX,IBITS,0,ILENFM,
     C              NBIT,IMASK,'C')
C--      CALL SBYTES (IGRIB(IWORD),FPDATA(IREP+1),IOFF,IBITS,0,ILENFM)
      CALL OFFSET (IOFF,ILENFM,IWORD,IBITS,NBIT,ILENG,IERR)
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
C
C
C***
C*    ENTER LENGTH OF BINARY DATA BLOCK, HAVING ENSURED THAT
C*    THE LENGTH IS AN EVEN NUMBER OF OCTETS.
C***
C
C     LENGTH OF BINARY DATA BLOCK IN BITS.
C
      LBIN = (IWORD-IPW) * NBIT + IOFF - IPB
C
      L = LBIN / 16
      L = LBIN - ( L*16 )
C
C     FILL UNUSED PORTION OF LAST 2 OCTETS WITH BINARY ZEROES.
C
      LNIL = 0
      IF (L.NE.0)
     C    THEN
              LNIL = 16 - L
              CALL SBYTE (IGRIB(IWORD),IBLOCK(1),IOFF,LNIL)
              CALL OFFSET (IOFF,1,IWORD,LNIL,NBIT,ILENG,IERR)
              IF (IERR.NE.0) RETURN
          ENDIF
C
      LBIN = (IWORD-IPW) * NBIT + IOFF - IPB
C
C     ENTER LENGTH - IN OCTETS - OF DATA BLOCK.
C
      LBIN = LBIN / 8
      CALL SBYTE (IGRIB(IPW),LBIN,IPB,24)
      CALL OFFSET (IPB,1,IPW,24,NBIT,ILENG,IERR)
C
C     ENTER NUMBER OF UNUSED BITS IN FLAG/BIT COUNT FIELD.
C
      IFLAG = IFLAG + LNIL
      CALL SBYTE (IGRIB(IPW),IFLAG,IPB,8)
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
C
C
C
C********************************************************************
C*
C*    BLOCK 5 - END BLOCK.
C*
C********************************************************************
C
C
C***
C*    ADD 7 7 7 7 TO CODED DATA.
C***
C
      IBLOCK(1) = 55
      IBLOCK(2) = 55
      IBLOCK(3) = 55
      IBLOCK(4) = 55
C
      CALL SBYTES (IGRIB(IWORD),IBLOCK(1),IOFF,8,0,4)
      CALL OFFSET (IOFF,4,IWORD,8,NBIT,ILENG,IERR)
      IF (IERR.NE.0) RETURN
C
C
C***
C*    SET ANY UNUSED PART OF LAST WORD TO BINARY ZEROES.
C***
C
      IF (IOFF.NE.0)
     C   THEN
             KBITS = NBIT - IOFF
             IBLOCK(1) = 0
             CALL SBYTE (IGRIB(IWORD),IBLOCK(1),IOFF,KBITS)
         ELSE
             IWORD = IWORD - 1
         ENDIF
C
C
C
C***
C*    ROUND TO 120 OCTETS, IF REQUIRED.
C***
C
      IF (IROUND.EQ.1)
     C   THEN
             NUMBIT = IWORD * NBIT
             I = NUMBIT / 960
             I = I * 960
             I = NUMBIT - I
             IF (I.NE.0) I = (960 - I) / NBIT
C
             DO 700 J=IWORD+1,IWORD+I
                IGRIB(J) = 0
  700        CONTINUE
C
             IWORD = IWORD + I
         ENDIF
C
      RETURN
      END
