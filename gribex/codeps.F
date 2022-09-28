C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CODEPS( NAME,CHDATA,ILENCH,FPDATA,ILENFP,INDATA,ILENIN,
     X                   NBYTE,LGDATA,ILENLG,IB1PAR,IPSCOD,ILENPS,
     X                   IRSERV,NBIT,IWORD,IROUND,IERR)
C
C---->
C**** CODEPS - Pack pseudo-grib product.
C
C     Purpose.
C     --------
C
C     Packs character data and arrays of floating point, integer
C     and logical values in machine-independent pseudo-grib format.
C
C**   Interface.
C     ----------
C
C     CALL CODEPS( NAME,CHDATA,ILENCH,FPDATA,ILENFP,INDATA,ILENIN,
C    X                   NBYTE,LGDATA,ILENLG,IB1PAR,IPSCOD,ILENPS,
C    X                   IRSERV,NBIT,IWORD,IROUND,IERR)
C
C
C
C     Input Parameters.
C     -----------------
C
C     NAME    - 4 Letters (Code name for section 0).
C     CHDATA  - Character string (letters and figures only) (section 4).
C     ILENCH  - Length of this string (Maximum length is 600).
C               To indicate missing data ILENCH is made
C               negative and string is all spaces.
C     FPDATA  - Array of floating point values (section 4).
C     ILENFP  - Length of this array (number of data values to be coded)
C               To indicate missing data values, ILENFP is made
C               negative and all data values are set to 0.
C     INDATA  - Array of integer values (section 4).
C     ILENIN  - Length of this array (number of data values to be coded)
C               To indicate missing data values, ILENIN is made
C               negative and all data values are set to 0.
C     NBYTE   - Number of bytes (8-bit) needed for integers.
C     LGDATA  - Array of logical values (section 4).
C     ILENLG  - Length of this array (number of data values to be coded)
C               To indicate missing data values, ILENLG is made
C               negative and all data values are set .false.
C     IB1PAR  - Integer array of data definition information (section 1)
C     ILENPS  - Length of array IPSCOD in bytes.
C     IRSERV  - Number of bytes reserved in data section for future use.
C     NBIT    - Number of bits per computer word.
C     IROUND  - 0 , no rounding.
C               1 , round to nearest 120 octets.
C
C     Output Parameters.
C     -----------------
C
C     IPSCOD  - Data coded in pseudo-grib code.
C     ILENPS  - Length of this array.
C     IWORD   - Number of words occupied by coded data.
C     IERR    - Error indicator.
C                0, no error.
C               -1, number of bytes per integer exceeds
C                   word length, or maximum allowed, is 0
C                   or negative.
C               -2, output array too small to hold coded
C                   data values.
C               -3, currently undefined option selected.
C               -4, missing data field contains non-zero.
C                   ( space for missing character data,
C                   and .false. for logicals.)
C               -5, integer larger than field width has
C                   been encountered.
C               -6, character data contains other than
C                   letters, figures or spaces.
C                1, invalid originating centre.
C                2, invalid model identification.
C                3, invalid grid identification.
C                4, invalid section indicator flag
C                5, invalid parameter identifier.
C                6, invalid level type.
C                7, level description error.
C                8, date/time error.
C                9, invalid time unit or range.
C               10, indicator/number averaged error.
C
C
C     Externals.
C     ----------
C
C     SBYTE
C     SBYTES
C     CONFPA
C     PRTBIN
C     OFFSET
C
C
C     Comments.
C     ---------
C
C     Section 1 , data definition section (IB1PAR).
C
C     Word      Contents
C     ----      --------
C       1       Originating centre identifier.
C       2       Model identification.
C       3       Grid definition.
C       4       Flag (table 1)
C       5       Parameter identifier (code table 2).
C       6       Type of level (code table 3).
C      7-8      Value(s) of levels (code table 3).
C       9       Year of data
C      10       Month of data
C      11       Day of data
C      12       Hour of data
C      13       Minute of data
C      14       Time unit (code table 4).
C      15       Time range one
C      16       Time range two
C      17       Time range flag (code table 5).
C      18       Number averaged.
C
C     Code tables are those for WMO GRIB code.
C
C     Value(s) of level can occupy 2 words.
C     For a layer the first word defines the top
C     and the second the bottom of the layer.
C     For a single level, only the first word is used.
C
C     Elements not used or inapplicable should be set to 255.
C
C
C     Author.
C     -------
C
C     J.Hennessy   05.02.86
C
C
C     Modifications.
C     --------------
C
C     J.Hennessy   06.06.86
C                  19.08.86
C                  21.08.86
C
C     J.Hennessy   17.09.90
C     Lower case letters as well as the characters
C     _ - . , added.
C
C     J.D.Chambers    ECMWF    13.09.94
C     Inhibit abort if NOABORT = 1, use grbcom.h
C
C     J.D.Chambers    ECMWF    12.08.96
C     Call CONFP3 instead of CONFP.
C
C     J.D.Chambers    ECMWF    April 1998
C     Call CONFPA instead of CONFP3.
C     (Vectorization improvement from D.Dent).
C
C----<
C********************************************************************
C
      IMPLICIT NONE
C
C     Parameters
      INTEGER NTYPE, NCHAR
      PARAMETER (NTYPE=2)
      PARAMETER (NCHAR=67)
C
C     Subroutine arguments
      CHARACTER*4 NAME
      CHARACTER*(*) CHDATA
      INTEGER ILENCH, ILENFP, INDATA, ILENIN, NBYTE, ILENLG, IB1PAR
      INTEGER IPSCOD, ILENPS, IRSERV, NBIT, IWORD, IROUND, IERR
      REAL FPDATA
      LOGICAL     LGDATA
      DIMENSION FPDATA(*), INDATA(*), LGDATA(*)
      DIMENSION IPSCOD(*), IB1PAR(*)
C
C     Local variables
      CHARACTER*1 CHAR
      DIMENSION CHAR(NCHAR)
      INTEGER IBLOCK
      DIMENSION IBLOCK(24)
      INTEGER IASCII
      DIMENSION IASCII(NCHAR)
      CHARACTER*4 VNAME
      DIMENSION VNAME(NTYPE)
      INTEGER JPAR, JLEN
      DIMENSION JPAR(NTYPE),JLEN(NTYPE)
C
C
      DATA IASCII /65 ,66 ,67 ,68 ,69 ,70 ,71 ,72 ,73 ,74 ,75 ,76 ,77 ,
     X             78 ,79 ,80 ,81 ,82 ,83 ,84 ,85 ,86 ,87 ,88 ,89 ,90 ,
     X             48 ,49 ,50 ,51 ,52 ,53 ,54 ,55 ,56 ,57 ,32 ,
     X             97 ,98 ,99 ,100,101,102,103,104,105,106,107,108,109,
     X             110,111,112,113,114,115,116,117,118,119,120,121,122,
     X             95 ,46 ,44 ,45  /
C
      DATA CHAR   /'A','B','C','D','E','F','G','H','I','J','K','L','M',
     X             'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
     X             '0','1','2','3','4','5','6','7','8','9',' ' ,
     X             'a','b','c','d','e','f','g','h','i','j','k','l','m',
     X             'n','o','p','q','r','s','t','u','v','w','x','y','z',
     X             '_','.',',','-' /
C
      DATA VNAME  /'BUDG','TIDE'/
C
      DATA JLEN   / 6000 , 25320/
C
      DATA JPAR   / 128  , 127  /
C
C     SET MAXIMUM NUMBER OF BITS FOR INTEGER FIELDS.
      INTEGER ICMISS, KLENCH, I, IFMISS, KLENFP, IIMISS, KLENIN
      INTEGER ILMISS, KLENLG, IBITS, IBYTE, NVAL, IOFF, KTEMP
      INTEGER KERR, IPW, IPB, NEGSIN, MAXINT, ISIGN
CC    INTEGER IEXP, IMANT
      INTEGER NTRUE, J, LBIN, L, LNIL, KBITS, NUMBIT, N
C
#include "grbcom.h"
C
C
C     Clear error indicator.
      IERR = 0
C
C
C********************************************************************
C
C     Check for missing data indicators.
C
C********************************************************************
C
C     Character data.
C
      ICMISS = 0
      IF (ILENCH.LT.0) THEN
        KLENCH = -ILENCH
        ICMISS = 1
        DO 50 I=1,KLENCH
          IF (CHDATA(I:I).NE.' ') THEN
            IERR = -4
            WRITE(*,*) 'CODEPS: Invalid value in missing data field.'
            RETURN
          ENDIF
   50   CONTINUE
      ELSE
        KLENCH = ILENCH
      ENDIF
C
C     Floating point numbers.
C
      IFMISS = 0
      IF (ILENFP.LT.0) THEN
        KLENFP = - ILENFP
        IFMISS = 1
        DO 100 I=1,KLENFP
          IF (FPDATA(I).NE.0.) THEN
            IERR = -4
            WRITE(*,*) 'CODEPS: Invalid value in missing data field.'
            RETURN
          ENDIF
  100   CONTINUE
      ELSE
        KLENFP = ILENFP
      ENDIF
C
C     Integers.
C
      IIMISS = 0
      IF (ILENIN.LT.0) THEN
        KLENIN = - ILENIN
        IIMISS = 1
        DO 200 I=1,KLENIN
          IF (INDATA(I).NE.0) THEN
            IERR = -4
            WRITE(*,*) 'CODEPS: Invalid value in missing data field.'
            RETURN
          ENDIF
  200   CONTINUE
      ELSE
        KLENIN = ILENIN
      ENDIF
C
C     Logicals.
C
      ILMISS = 0
      IF (ILENLG.LT.0) THEN
        KLENLG = - ILENLG
        ILMISS = 1
        DO 300 I=1,KLENLG
          IF (LGDATA(I).NEQV..FALSE.) THEN
            IERR = -4
            WRITE(*,*) 'CODEPS: Invalid value in missing data field.'
            RETURN
          ENDIF
  300   CONTINUE
      ELSE
        KLENLG = ILENLG
      ENDIF
C
C
C
C********************************************************************
C
C     Check number of bytes for integer numbers.
C
C********************************************************************
C
      IBITS = NBYTE * 8
C
      IF (IBITS.GT.NBIT) THEN
        WRITE(*,*) 'CODEPS: Number of bits per data value ', IBITS
        WRITE(*,*) '        exceeds word length ', NBIT
        IERR = -1
        RETURN
      ENDIF
C
      IF (IBITS.LE.0) THEN
        WRITE(*,*) 'CODEPS: Maximum permitted value ', IBITS
        WRITE(*,*) '        should be positive.'
        IERR = -1
        RETURN
      ENDIF
C
C
C********************************************************************
C
C     Section 0 - INDICATOR Section.
C
C********************************************************************
C
C
C     The letters of the code name are inserted in the
C     first 4 elements of 'IBLOCK'.
      DO 400 I=1,36
         IF (NAME(1:1).EQ.CHAR(I)) IBLOCK(1) = IASCII(I)
         IF (NAME(2:2).EQ.CHAR(I)) IBLOCK(2) = IASCII(I)
         IF (NAME(3:3).EQ.CHAR(I)) IBLOCK(3) = IASCII(I)
         IF (NAME(4:4).EQ.CHAR(I)) IBLOCK(4) = IASCII(I)
  400 CONTINUE
C
C     Pack 4 8-bit fields in section 0 of coded array.
      IWORD = 1
      IBYTE = 8
      NVAL  = 4
      IOFF  = 0
      CALL SBYTES(IPSCOD(IWORD),IBLOCK(1),IOFF,IBYTE,0,NVAL)
      CALL OFFSET(IOFF,NVAL,IWORD,IBYTE,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C
C********************************************************************
C
C     Section 1 - PRODUCT DEFINITION Section.
C
C********************************************************************
C
C
C     Length of section 1 (in octets) in first 3 octets.
      IBLOCK(1) = 0
      IBLOCK(2) = 0
      IBLOCK(3) = 24
C
C     Fourth octet reserved and set to 0.
      IBLOCK(4) = 0
C
C     Identification of centre
      IBLOCK(5) = IB1PAR(1)
      IF (IB1PAR(1).LT.1.OR.IB1PAR(1).GT.98) THEN
        IERR = 1
        WRITE(*,*) 'CODEPS: Invalid originating centre ', IB1PAR(1)
        RETURN
      ENDIF
C
C     Model identification
      IBLOCK(6) = IB1PAR(2)
      IF (IB1PAR(2).LT.1.OR.IB1PAR(2).GT.255) THEN
        IERR = 2
        WRITE(*,*) 'CODEPS: Invalid model identification ', IB1PAR(2)
        RETURN
      ENDIF
C
C     Grid definition
      IBLOCK(7) = IB1PAR(3)
      IF (IB1PAR(3).LT.1.OR.IB1PAR(3).GT.255) THEN
        IERR = 3
        WRITE(*,*) 'CODEPS: Invalid grid definition ', IB1PAR(3)
        RETURN
      ENDIF
C
C     Optional section indicator flags ( code table 1 ).
C
C     Sections included  Binary value      Decimal value
C
C         NONE             00000000               0
C          2               10000000             128
C          3               01000000              64
C         2 AND 3          11000000             192
C
      IBLOCK(8) = IB1PAR(4)
C
C     Shift 2-bit flag field right by 6 bits to facilitate checking
      KTEMP = IB1PAR(4) / 64
      IF (KTEMP.NE.0) IERR = 4
      IF (IERR.NE.0) THEN
        CALL PRTBIN(IB1PAR(4),8,KTEMP,KERR)
        WRITE(*,*) 'CODEPS: Invalid section indicator flag ', KTEMP
        WRITE(*,*) 'CODEPS: Return code from PRTBIN = ', KERR
        RETURN
      ENDIF
C
C     Parameter identifier ( code table 2 ).
      IBLOCK(9) = IB1PAR(5)
      IF (IB1PAR(5).LT.0.OR.IB1PAR(5).GT.255) THEN
        IERR = 5
        WRITE(*,*) 'CODEPS: Invalid parameter ', IB1PAR(5)
        RETURN
      ENDIF
C
C     Indicator of level type ( code table 3 ).
      IBLOCK(10) = IB1PAR(6)
      IF (IB1PAR(6).LT.0.OR.IB1PAR(6).GT.255) THEN
        IERR = 6
        WRITE(*,*) 'CODEPS: Invalid level type ', IB1PAR(6)
        RETURN
      ENDIF
C
C     Description of level or layer ( code table 3 ).
C
C     For certain special levels no further description is
C     necessary, and the fields are set to 0.
C
      IF (((IB1PAR(6).LT.100).AND.(IB1PAR(6).NE.20)).OR.
     X     (IB1PAR(6).EQ.102) .OR.
     X     (IB1PAR(6).EQ.200) .OR.
     X     (IB1PAR(6).EQ.201)) THEN
        IB1PAR(7) = 0
        IB1PAR(8) = 0
      ENDIF
C
      IBLOCK(11) = IB1PAR(7)
      IBLOCK(12) = IB1PAR(8)
C
C     Certain level types require that the description occupy
C     both 8 bit fields.
C
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
C
C       8 low order bits in IBLOCK(12)
C       High order bits in IBLOCK(11)
        IBLOCK(12) = IBLOCK(11)
        IBLOCK(11) = IBLOCK(11) / 256
        IBLOCK(12) = IBLOCK(12) - IBLOCK(11) * 256
C
C       Check maximum possible in 16 bits not exceeded.
        IF (IB1PAR(7).GT.65535) IERR = 7
      ELSE
        IF (IB1PAR(7).GT.255.OR.IB1PAR(8).GT.255) IERR = 7
      ENDIF
C
      IF (IERR.NE.0) THEN
        WRITE(*,*) 'CODEPS: Level description error ', IB1PAR(7),
     X                                                  IB1PAR(8)
        RETURN
      ENDIF
C
C     Date and time
C     (Allow: YYYY format, years 1900 upto 2154
C      or       YY format, years   00 upto  254)
C
      IF( IB1PAR(9).GT.1900 ) THEN
        IBLOCK(13) = IB1PAR(9) - 1900
      ELSE
        IBLOCK(13) = IB1PAR(9)
      ENDIF
      IF (IBLOCK(13).LT.0.OR.IBLOCK(13).GT.254) IERR = 8
C
      IBLOCK(14) = IB1PAR(10)
      IF (IB1PAR(10).LT.1.OR.IB1PAR(10).GT.12) IERR = 8
      IBLOCK(15) = IB1PAR(11)
      IF (IB1PAR(11).LT.1.OR.IB1PAR(11).GT.31) IERR = 8
      IBLOCK(16) = IB1PAR(12)
      IF (IB1PAR(12).LT.0.OR.IB1PAR(12).GT.23) IERR = 8
      IBLOCK(17) = IB1PAR(13)
      IF (IB1PAR(13).LT.0.OR.IB1PAR(13).GT.59) IERR = 8
C
      IF (IERR.NE.0) THEN
        WRITE(*,9010) IB1PAR(9),IB1PAR(10),IB1PAR(11),IB1PAR(12),
     X                       IB1PAR(13)
 9010   FORMAT('CODEPS: Invalid date/time ',I4.4,2I2.2,' / ',2I2.2)
        RETURN
      ENDIF
C
C     Unit of time - ( code table 4 ).
C     Time range(s) and time range indicator ( code table 5 ).
C
C     Time unit.
      IBLOCK(18) = IB1PAR(14)
      IF (IB1PAR(14).LT.0.OR.IB1PAR(14).GT.7) IERR = 9
C
C     There can be one or two time ranges, each in 1 octect or
C     one time range in 2 octects.
      IF (IB1PAR(17).EQ.0.OR.
     X    IB1PAR(17).EQ.1.OR.
     X    IB1PAR(17).EQ.10)   IB1PAR(16)  = 0
C
C     Cross check range values and flag values.
      IF (IB1PAR(15).GT.65535.OR.IB1PAR(15).LT.0)   IERR = 9
      IF (IB1PAR(15).GT.255.AND.IB1PAR(17).NE.10)   IERR = 9
      IF (IB1PAR(16).GT.255.OR.IB1PAR(16).LT.0)     IERR = 9
      IF (IB1PAR(17).LT.0.OR.IB1PAR(17).GT.10)      IERR = 9
      IF (IERR.NE.0) THEN
        WRITE(*,9011) IB1PAR(14),IB1PAR(15),IB1PAR(16),IB1PAR(17)
 9011   FORMAT(1H ,'CODEPS: Time unit/Time 1/Time 2/indicator error - ',
     X                I8,2X,'/',I8,2X,'/',I8,2X,'/',I8)
        RETURN
      ENDIF
C
      IBLOCK(19) = IB1PAR(15)
      IBLOCK(20) = IB1PAR(16)
C
C     One time range occupying both octets.
      IF (IB1PAR(17).EQ.10) THEN
C
C       8 low order bits in IBLOCK(20)
C       High order bits in IBLOCK(19)
        IBLOCK(20) = IBLOCK(19)
        IBLOCK(19) = IBLOCK(19) / 256
        IBLOCK(20) = IBLOCK(20) - IBLOCK(19) * 256
      ENDIF
C
C     Time range indicator
      IBLOCK(21) = IB1PAR(17)
C
C     Number averaged.
      IBLOCK(22) = 0
      IBLOCK(23) = 0
C
C     Check consistency of indicator and number averaged fields.
      IF (IB1PAR(17).EQ.3.AND.IB1PAR(18).EQ.0) THEN
        IERR = 10
        WRITE(*,9012) IB1PAR(17),IB1PAR(18)
 9012   FORMAT(1H ,'CODEPS: Indicator/number averaged error - ',
     C                I8,2X,'/',I8)
        RETURN
      ENDIF
C
C     Value in two octets.
      IBLOCK(22) = IB1PAR(18)
C
C     8 low order bits in IBLOCK(23)
C     High order bits in IBLOCK(22)
      IBLOCK(23) = IBLOCK(22)
      IBLOCK(22) = IBLOCK(22) / 256
      IBLOCK(23) = IBLOCK(23) - IBLOCK(22) * 256
C
C     Reserved element.
      IBLOCK(24) = 0
C
C     Pack 24 8-bit fields of section 1 in coded array.
      IBYTE = 8
      NVAL = 24
      CALL SBYTES(IPSCOD(IWORD),IBLOCK(1),IOFF,IBYTE,0,NVAL)
      CALL OFFSET(IOFF,NVAL,IWORD,IBYTE,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C     Set array IBLOCK to 0 . it is used to zero fill reserved octets.
      DO 500 I=1,24
         IBLOCK(I) = 0
  500 CONTINUE
C
C
C********************************************************************
C
C     Section 4 - BINARY DATA Section.
C
C********************************************************************
C
C
C     Octets 1-3 - length of data section (in octets).
C
C     Retain pointers to word and bit position of binary data
C     section length field. enter length as 0.
C
      IPW = IWORD
      IPB = IOFF
      CALL SBYTE(IPSCOD(IWORD),IBLOCK(1),IOFF,24)
      CALL OFFSET(IOFF,1,IWORD,24,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C     Octet 4 is reserved.
      CALL SBYTE(IPSCOD(IWORD),IBLOCK(1),IOFF,8)
      CALL OFFSET(IOFF,1,IWORD,8,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C     Octet 5 - missing data flag.
C
C     1---  missing character data
C     -1--  missing floating point data
C     --1-  missing integer data
C     ---1  missing logical data
C
      KTEMP = ICMISS*8 + IFMISS*4 + IIMISS*2 + ILMISS
      CALL SBYTE(IPSCOD(IWORD),KTEMP,IOFF,8)
      CALL OFFSET(IOFF,1,IWORD,8,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C     Octet 6 - number of bytes per integer.
      CALL SBYTE(IPSCOD(IWORD),NBYTE,IOFF,8)
      CALL OFFSET(IOFF,1,IWORD,8,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C     Octets 7-8 reserved.
      CALL SBYTES(IPSCOD(IWORD),IBLOCK(1),IOFF,8,0,2)
      CALL OFFSET(IOFF,2,IWORD,8,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C     Octets 9-11 - number of characters in character data.
      CALL SBYTE(IPSCOD(IWORD),KLENCH,IOFF,24)
      CALL OFFSET(IOFF,1,IWORD,24,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C     Octets 12-14 - number of floating point values
      CALL SBYTE(IPSCOD(IWORD),KLENFP,IOFF,24)
      CALL OFFSET(IOFF,1,IWORD,24,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C     Octets 15-17 - number of integer values
      CALL SBYTE(IPSCOD(IWORD),KLENIN,IOFF,24)
      CALL OFFSET(IOFF,1,IWORD,24,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C     Octets 18-20 - number of logicals
      CALL SBYTE(IPSCOD(IWORD),KLENLG,IOFF,24)
      CALL OFFSET(IOFF,1,IWORD,24,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C     Octets 21-23 - number of bytes in data section reserved.
      CALL SBYTE(IPSCOD(IWORD),IRSERV,IOFF,24)
      CALL OFFSET(IOFF,1,IWORD,24,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C     Octets 24-32 are reserved.
      CALL SBYTES(IPSCOD(IWORD),IBLOCK(1),IOFF,8,0,9)
      CALL OFFSET(IOFF,9,IWORD,8,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C     Octet 33-   floating point numbers.
C
C     Convert values to pseudo-grib format (IEXP,IMANT)
C
      IF (KLENFP.NE.0)
     X  CALL CONFPA(FPDATA,KLENFP,IOFF,IPSCOD(IWORD),IWORD,ILENPS,NBIT)
C
C     Integer values
C
      IF (KLENIN.NE.0) THEN
C
C       Set up mask for negative numbers.
        NEGSIN = 2 ** (IBITS-1)
C
C       Maximum size integer in given field width.
        MAXINT = NEGSIN - 1
C
        DO 700 I=1,KLENIN
          KTEMP = INDATA(I)
C
C         Handle negative numbers.
          ISIGN = 0
          IF (KTEMP.LT.0) THEN
            KTEMP = - KTEMP
            ISIGN = NEGSIN
          ENDIF
C
C         Check size of numbers.
          IF (KTEMP.GT.MAXINT) THEN
            IERR = -5
            WRITE(*,*) 'CODEPS: Integer larger than field specified'
            RETURN
          ENDIF
C
C         Include sign bit.
          KTEMP = KTEMP + ISIGN
          CALL SBYTE(IPSCOD(IWORD),KTEMP,IOFF,IBITS)
          CALL OFFSET(IOFF,1,IWORD,IBITS,NBIT,ILENPS,IERR)
          IF (IERR.NE.0) RETURN
C
  700   CONTINUE
C
      ENDIF
C
C     Logical values
C
      IF (KLENLG.NE.0) THEN
        DO 800 I=1,KLENLG
          NTRUE = 0
          IF (LGDATA(I).EQV..TRUE.) NTRUE = 1
          CALL SBYTE(IPSCOD(IWORD),NTRUE,IOFF,8)
          CALL OFFSET(IOFF,1,IWORD,8,NBIT,ILENPS,IERR)
          IF (IERR.NE.0) RETURN
  800   CONTINUE
C
      ENDIF
C
C     Character data.
C
      IF (KLENCH.NE.0) THEN
        DO 850 I=1,KLENCH
          DO 825 J=1,NCHAR
            IF (CHDATA(I:I).EQ.CHAR(J)) THEN
              KTEMP = IASCII(J)
              CALL SBYTE(IPSCOD(IWORD),KTEMP,IOFF,8)
              CALL OFFSET(IOFF,1,IWORD,8,NBIT,ILENPS,IERR)
              IF (IERR.NE.0) RETURN
              GO TO 850
            ENDIF
  825     CONTINUE
          IERR = -6
          WRITE(*,*) 'CODEPS: Character data contains other than '
          WRITE(*,*) '         _ - . , letters, figures or spaces.'
          RETURN
  850   CONTINUE
      ENDIF
C
C     Reserved bytes
C
      IF (IRSERV.NE.0) THEN
        DO 900 I=1,IRSERV
          CALL SBYTE(IPSCOD(IWORD),IBLOCK(1),IOFF,8)
          CALL OFFSET(IOFF,1,IWORD,8,NBIT,ILENPS,IERR)
          IF (IERR.NE.0) RETURN
  900   CONTINUE
C
      ENDIF
C
C     Enter length of binary data section, having ensured that
C     the length is an even number of octets.
C
C     Length of binary data section in bits.
C
      LBIN = (IWORD-IPW) * NBIT + IOFF - IPB
      L = LBIN / 16
      L = LBIN - ( L*16 )
C
C     Fill unused portion of last 2 octets with binary zeroes.
      LNIL = 0
      IF (L.NE.0) THEN
        LNIL = 16 - L
        CALL SBYTE(IPSCOD(IWORD),IBLOCK(1),IOFF,LNIL)
        CALL OFFSET(IOFF,1,IWORD,LNIL,NBIT,ILENPS,IERR)
        IF (IERR.NE.0) RETURN
      ENDIF
      LBIN = (IWORD-IPW) * NBIT + IOFF - IPB
C
C     Enter length - in octets - of data section.
      LBIN = LBIN / 8
      CALL SBYTE(IPSCOD(IPW),LBIN,IPB,24)
      CALL OFFSET(IPB,1,IPW,24,NBIT,ILENPS,IERR)
C
C********************************************************************
C
C     Section 5 - END Section.
C
C********************************************************************
C
C
C     Add 7 7 7 7 to coded data.
      IBLOCK(1) = 55
      IBLOCK(2) = 55
      IBLOCK(3) = 55
      IBLOCK(4) = 55
      CALL SBYTES(IPSCOD(IWORD),IBLOCK(1),IOFF,8,0,4)
      CALL OFFSET(IOFF,4,IWORD,8,NBIT,ILENPS,IERR)
      IF (IERR.NE.0) RETURN
C
C     Set any unused part of last word to binary zeroes.
      IF (IOFF.NE.0) THEN
        KBITS = NBIT - IOFF
        CALL SBYTE(IPSCOD(IWORD),IBLOCK(1),IOFF,KBITS)
      ELSE
        IWORD = IWORD - 1
      ENDIF
C
C     Round to 120 octets, if required.
      IF (IROUND.EQ.1) THEN
        NUMBIT = IWORD * NBIT
        I = NUMBIT / 960
        I = I * 960
        I = NUMBIT - I
        IF (I.NE.0) I = (960 - I) / NBIT
        DO 1000 J=IWORD+1,IWORD+I
          IPSCOD(J) = 0
 1000   CONTINUE
C
        IWORD = IWORD + I
      ENDIF
C
C     Check consistency of record name, parameter number and
C     record length.
C
C     This may cause problems in the future if old
C     versions of the budget value records are unpacked
C     and then repacked.
C
C     Diagnostic records can have different lengths.
C
      IF (NAME.EQ.'DIAG') GO TO 9999
C
      N = (NBIT * IWORD) / 8
      KERR = 0
      DO 1100 I=1,NTYPE
        IF (VNAME(I).EQ.NAME) THEN
          IF (N.NE.JLEN(I)) KERR = 1
          IF (IB1PAR(5).NE.JPAR(I)) KERR = 1
          IF (KERR.NE.0) THEN
            WRITE(*,9013) N , IB1PAR(5)
 9013       FORMAT(1H ,'CODEPS: Illegal record length = ',I6,
     X      ' octets, or invalid parameter no. = ', I6,'.')
C
            IF( NOABORT .EQ. 0 ) CALL ABORT
          ENDIF
          GO TO 9999
        ENDIF
C
 1100 CONTINUE
C
      WRITE(*,*) 'CODEPS: Invalid record name supplied - ', NAME
      IF( NOABORT .EQ. 0 ) CALL ABORT
C
C
 9999 RETURN
      END
