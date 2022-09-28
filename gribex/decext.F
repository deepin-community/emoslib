C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE DECEXT (KBIT,PDATA,KDATA,KGRIB,KLENB,KI,KJ,KERR)
C
C---->
C***** DECEXT - Extract values from GRIB coded data.
C
C     Purpose.
C     --------
C
C           Return unpacked values for a series of grid points from an
C           array of packed grib data, without unpacking all values.
C
C**   Interface.
C     ----------
C
C           CALL DECEXT (KBIT,PDATA,KDATA,KGRIB,KLENB,KI,KJ,KERR)
C
C           INTEGER    K.
C           REAL       P.
C           LOGICAL    O.
C           CHARACTER  H.
C
C               Input parameters.
C               -----------------
C
C                 KBIT    - Number of bits in computer word
C
C                 PDATA   - Array to receive unpacked values.
C
C                 KDATA   - Size of array PDATA (number of grid points
C                           required).
C
C                 KGRIB   - Integer array containing data in grib code.
C
C                 KLENB   - Length of array KGRIB (in words).
C
C                 KI      - I indices of required grid points.
C                           (dimensioned KDATA).
C
C                 KJ      - J indices of required grid points.
C                           (dimensioned KDATA).
C
C
C               Output parameters.
C               ------------------
C
C                 PDATA(N) - Value of grid point KI(N),KJ(N)
C
C                 KERR     - 0 if no error.
C                            -1 If not grid point field.
C                            -2 If error in routine OFFSET.
C
C     Method.
C     -------
C
C           The self defining fields of section 0 - 3 of the GRIB code
C           are used to step down to the data part of section 4. The
C           reference value and scale factor are extracted and converted
C           The packed increments for the required
C           grid points are extracted and the values calculated.
C
C     Externals.
C     ----------
C
C           GBYTE
C           GBYTES
C           DECFP
C           OFFSET
C
C     Reference.
C     ----------
C
C           MARS User Guide for details of GRIB code and fields.
C
C     Comments.
C     ---------
C
C           Subroutine contains section 0 - 2 , and section 9.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF     09/05/88
C
C     Modifications.
C     --------------
C
C           None
C
C     ------------------------------------------------------------------
C----<
C
C
C
C
C
C*    SECTION 0 . DEFINITION OF VARIABLES.
C     ------------------------------------------------------------------
C
      IMPLICIT LOGICAL     ( L , O , G )
      IMPLICIT CHARACTER*8 ( C , H , Y )
      IMPLICIT INTEGER     ( I , J , K , M , N)
C
      DIMENSION KGRIB(KLENB)
      DIMENSION PDATA(KDATA)
      DIMENSION KI(KDATA)
      DIMENSION KJ(KDATA)
      DIMENSION IB1PAR(18)
      DIMENSION IB2PAR(12)
      DIMENSION IBLOCK(24)
C
      EQUIVALENCE (ZGRIB,JGRIB)
C
C     SET MAXIMUM NUMBER OF BITS PER DATA FIELD.
C
      DATA IMAX /60/
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
C*    SECTION 1 . SKIP DOWN THROUGH GRIB SECTION TO GET TO DATA SECTION.
C     ------------------------------------------------------------------
C
 1000 CONTINUE
      ZSCALE = 0.
C
C     BLOCK 0 - INDICATOR BLOCK.
C
C     SKIP 4 OCTETS CONTAINING ASCII G R I B .
C
      IWORD = 1
      IOFF  = 0
      IBYTE = 8
      IVAL  = 4
C
      CALL OFFSET (IOFF,IVAL,IWORD,IBYTE,KBIT,KLENB,KERR)
      IF (KERR.NE.0) GO TO 9000
C
C     BLOCK 1 - PRODUCT DEFINITION BLOCK.
C
C     EXTRACT FIRST 4 OCTETS OF BLOCK 1.
C
      IVAL  = 4
C
      CALL GBYTES (KGRIB(IWORD),IBLOCK(1),IOFF,IBYTE,0,IVAL)
      CALL OFFSET (IOFF,IVAL,IWORD,IBYTE,KBIT,KLENB,KERR)
      IF (KERR.NE.0) GO TO 9000
C
C     IF THESE OCTETS DO NOT CONTAIN 0, 0, 24 AND 0 RESPECTIVELY
C     THEN THE DATA IS IN THE OLD VERSION OF THE CODE.
C
      IF (IBLOCK(1).EQ.0.AND.IBLOCK(2).EQ.0.AND.IBLOCK(3).EQ.24.
     C    AND.IBLOCK(4).EQ.0)
     C      THEN
                ISNEW = 1
                IVAL = 20
            ELSE
                ISNEW = 0
                IVAL = 16
            ENDIF
C
C     EXTRACT NEXT 16 OR 20 OCTETS OF BLOCK 1 - NUMBER DEPENDS
C     ON VERSION OF CODE.
C
      CALL GBYTES (KGRIB(IWORD),IBLOCK(5),IOFF,IBYTE,0,IVAL)
      CALL OFFSET (IOFF,IVAL,IWORD,IBYTE,KBIT,KLENB,KERR)
      IF (KERR.NE.0) GO TO 9000
C
C     TRANSFER PRODUCT DEFINITION INFORMATION.
C
      DO 1100 J1100=1,17
         IB1PAR(J1100) = IBLOCK(J1100+4*ISNEW)
 1100 CONTINUE
C
C     BLOCK 2 - GRID DESCRIPTION BLOCK.
C
C     CHECK FLAG INDICATING IF BLOCK 2 IS INCLUDED IN CODE.
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
C            LENGTH OF GRID DESCRIPTION BLOCK.
C
             CALL GBYTE (KGRIB(IWORD),IBLK2,IOFF,24)
             CALL OFFSET (IOFF,1,IWORD,24,KBIT,KLENB,KERR)
             IF (KERR.NE.0) GO TO 9000
C
             ILENV = (IBLK2 - 32) / 4
C
C            NUMBER OF UNUSED BITS AND RESERVED BYTE SKIPPED.
C
             CALL OFFSET (IOFF,2,IWORD,8,KBIT,KLENB,KERR)
             IF (KERR.NE.0) GO TO 9000
C
C            DATA REPRESENTATION TYPE.
C
             CALL GBYTE (KGRIB(IWORD),IB2PAR(1),IOFF,8)
             CALL OFFSET (IOFF,1,IWORD,8,KBIT,KLENB,KERR)
             IF (KERR.NE.0) GO TO 9000
C
C            CHECK FOR GAUSSIAN OR LAT/LONG GRID.
C
             IF (IB2PAR(1).NE.0.AND.IB2PAR(1).NE.4)
     C          THEN
                    KERR = -1
                    GO TO 9000
                ENDIF
C
C            NUMBER OF LATITUDE AND LONGITUDE POINTS.
C
             CALL GBYTES (KGRIB(IWORD),IB2PAR(2),IOFF,16,0,2)
             CALL OFFSET (IOFF,2,IWORD,16,KBIT,KLENB,KERR)
             IF (KERR.NE.0) GO TO 9000
C
C            LAT/LONG OF ORIGIN SKIPPED.
C
             CALL OFFSET (IOFF,2,IWORD,24,KBIT,KLENB,KERR)
             IF (KERR.NE.0) GO TO 9000
C
C            RESOLUTION FLAG SKIPPED.
C
             CALL OFFSET (IOFF,1,IWORD,8,KBIT,KLENB,KERR)
             IF (KERR.NE.0) GO TO 9000
C
C            LAT/LONG OF EXTREME POINT SKIPPED.
C
             CALL OFFSET (IOFF,2,IWORD,24,KBIT,KLENB,KERR)
             IF (KERR.NE.0) GO TO 9000
C
C            DIRECTION INCREMENTS/ NO. OF LATITUDE LINES SKIPPED.
C
             CALL OFFSET (IOFF,2,IWORD,16,KBIT,KLENB,KERR)
             IF (KERR.NE.0) GO TO 9000
C
C            SCANNING MODE FLAG SKIPPED.
C
             CALL OFFSET (IOFF,1,IWORD,8,KBIT,KLENB,KERR)
             IF (KERR.NE.0) GO TO 9000
C
C            4 RESERVED OCTETS SKIPPED.
C
             CALL OFFSET (IOFF,4,IWORD,8,KBIT,KLENB,KERR)
             IF (KERR.NE.0) GO TO 9000
C
          ENDIF
C
C
C
C
C     BLOCK 3 - BIT MAP BLOCK.
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
               CALL GBYTE (KGRIB(IWORD),IBLK3,IOFF,24)
               CALL OFFSET (IOFF,IBLK3,IWORD,8,KBIT,KLENB,KERR)
               IF (KERR.NE.0) GO TO 9000
           ENDIF
C
C
C
C
C
C
C
C
C*    SECTION 2. EXTRACT PACKED INCREMENTS AND CALCULATE VALUES.
C     ------------------------------------------------------------------
C
 2000  CONTINUE
C
C     BLOCK 4 - BINARY DATA BLOCK.
C     SKIP LENGTH OF BINARY DATA BLOCK.
C
      CALL OFFSET (IOFF,1,IWORD,24,KBIT,KLENB,KERR)
      IF (KERR.NE.0) GO TO 9000
C
C
C     4 BIT FLAG / 4 BIT COUNT OF UNUSED BITS AT END OF BLOCK OCTET.
C
      CALL OFFSET (IOFF,1,IWORD,8,KBIT,KLENB,KERR)
      IF (KERR.NE.0) GO TO 9000
C
C
C     GET SCALE FACTOR
C
      CALL GBYTE (KGRIB(IWORD),JSCALE,IOFF,16)
      CALL OFFSET (IOFF,1,IWORD,16,KBIT,KLENB,KERR)
      IF (KERR.NE.0) GO TO 9000
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
C     GET REFERENCE VALUE (FMIN) IN GRIB FORMAT (IEXP,IMANT)
C
C
      CALL GBYTE (KGRIB(IWORD),IEXP,IOFF,8)
      CALL OFFSET (IOFF,1,IWORD,8,KBIT,KLENB,KERR)
      IF (KERR.NE.0) GO TO 9000
      CALL GBYTE (KGRIB(IWORD),IMANT,IOFF,24)
      CALL OFFSET (IOFF,1,IWORD,24,KBIT,KLENB,KERR)
      IF (KERR.NE.0) GO TO 9000
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
C     GET NUMBER OF BITS IN EACH DATA VALUE.
C
      CALL GBYTE (KGRIB(IWORD),IBITS,IOFF,8)
      CALL OFFSET (IOFF,1,IWORD,8,KBIT,KLENB,KERR)
      IF (KERR.NE.0) GO TO 9000
C
C
C     CHECK NUMBER OF BITS PER DATA FIELD.
C
      IF (IBITS.GT.KBIT.OR.IBITS.GT.IMAX)
     C    THEN
              KERR = -1
              WRITE (*,9001) IBITS,KBIT,IMAX
              GO TO 9000
          ENDIF
C
C
C
C
C     DECODE DATA VALUES TO FLOATING POINT.
C
      IPW = IWORD
      IPB = IOFF
C
      DO 2100 J2100 = 1,KDATA
         IWORD = IPW
         IOFF  = IPB
         ILENF = (KI(J2100)-1) * IB2PAR(2) + KJ(J2100) - 1
         CALL OFFSET (IOFF,ILENF,IWORD,IBITS,KBIT,KLENB,KERR)
         IF (KERR.NE.0) GO TO 9000
         CALL GBYTE (KGRIB(IWORD),IDATA,IOFF,IBITS)
         PDATA(J2100)  = FMIN + IDATA * ZSCALE
 2100 CONTINUE
C
      IF (IMISS.EQ.0) GO TO 9000
C
      DO 2200 J2200 = 1,KDATA
         PDATA(J2200)  = 0.0
 2200 CONTINUE
C
C
C
C*    SECTION 9. FORMAT STATEMENTS. RETURN.
C     ------------------------------------------------------------------
C
 9000 CONTINUE
C
 9001 FORMAT (1H ,'NUMBER OF BITS PER DATA VALUE, ',I3,
     C        'EXCEEDS WORD LENGTH, ',I3,' OR MAXIMUM ',
     C        ' PERMITTED VALUE, ',I3)
      RETURN
C
      END
