C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE PRTBL2 (IB2PAR)
C
C---->
C*
C*    NAME      : PRTBL2
C*
C*    FUNCTION  : PRINT THE INFORMATION IN THE GRDI DEFINITION
C*                BLOCK (BLOCK 2) OF DECODED GRIB DATA.
C*
C*    INPUT     : IB2PAR - ARRAY OF DECODED PARAMETERS FROM BLOCK 2.
C*
C*    JOHN HENNESSY  ECMWF 28 MAY 1985
C*
C*
C     ---------------------------------------------------------------
C----<
C
      DIMENSION IB2PAR(*)
C
      WRITE (*,9000)
 9000 FORMAT (1H0,'BLOCK 2 - GRID DEFINITION BLOCK',/)
C
      WRITE (*,9001) IB2PAR(1)
 9001 FORMAT (1H0,'DATA REPRESENTATION TYPE (TABLE 6)   ',I9)
C
C     SPHERICAL HARMONIC DATA
C
      IF (IB2PAR(1).EQ.50)
     C   THEN
C
             WRITE (*,9002) IB2PAR(2)
 9002        FORMAT (1H ,'J - PENTAGONAL RESOLUTION PARAMETER. ',I9)
             WRITE (*,9003) IB2PAR(3)
 9003        FORMAT (1H ,'K - PENTAGONAL RESOLUTION PARAMETER. ',I9)
             WRITE (*,9004) IB2PAR(4)
 9004        FORMAT (1H ,'M - PENTAGONAL RESOLUTION PARAMETER. ',I9)
             WRITE (*,9005) IB2PAR(5)
 9005        FORMAT (1H ,'REPRESENTATION TYPE (TABLE 9)        ',I9)
             WRITE (*,9006) IB2PAR(6)
 9006        FORMAT (1H ,'REPRESENTATION MODE (TABLE 10).      ',I9)
C
             RETURN
C
         ENDIF
C
C     GAUSSIAN AND REGULAR LATITUDE / LONGITUDE GRIDS.
C
      IF (IB2PAR(1).EQ.0.OR.IB2PAR(1).EQ.4)
     C   THEN
C
             WRITE (*,9102) IB2PAR(2)
 9102        FORMAT (1H ,'NO. OF POINTS ALONG A LATITUDE.      ',I9)
             WRITE (*,9103) IB2PAR(3)
 9103        FORMAT (1H ,'NO. OF POINTS ALONG A MERIDIAN.      ',I9)
             WRITE (*,9104) IB2PAR(4)
 9104        FORMAT (1H ,'LATITUDE OF ORIGIN (SOUTH -IVE)      ',I9)
             WRITE (*,9105) IB2PAR(5)
 9105        FORMAT (1H ,'LONGITUDE OF ORIGIN (WEST -IVE)      ',I9)
             WRITE (*,9106) IB2PAR(6)
 9106        FORMAT (1H ,'EXTREME POINT / INCREMENTS FLAG.     ',I9)
             WRITE (*,9107) IB2PAR(7)
 9107        FORMAT (1H ,'LATITUDE OF EXTREME POINT(SOUTH -IVE)',I9)
             WRITE (*,9108) IB2PAR(8)
 9108        FORMAT (1H ,'LONGITUDE OF EXTREME POINT(WEST -IVE)',I9)
             WRITE (*,9109) IB2PAR(9)
 9109        FORMAT (1H ,'LATITUDE INCREMENT.                  ',I9)
             IF (IB2PAR(1).EQ.0)
     C          THEN
                    WRITE (*,9110) IB2PAR(10)
 9110               FORMAT(1H ,'LONGITUDE INCREMENT.                 ',
     C                         I9)
                ENDIF
             IF (IB2PAR(1).EQ.4)
     C          THEN
                    WRITE (*,8110) IB2PAR(10)
 8110               FORMAT(1H ,'NO. OF LATITUDE LINES POLE / EQUATOR.',
     C                         I9)
                ENDIF
             WRITE (*,9111) IB2PAR(11)
 9111        FORMAT (1H ,'SCANNING MODE (CODE TABLE 8).        ',I9)
C
             RETURN
C
         ENDIF
C
      END
