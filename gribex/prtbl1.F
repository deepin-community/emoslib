C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE PRTBL1 (IB1PAR)
C
C---->
C**************************************************************
C*
C*    NAME      : PRTBL1
C*
C*    FUNCTION  : PRINT THE INFORMATION IN THE PRODUCT DEFINITION
C*                BLOCK (BLOCK 1) OF DECODED GRIB DATA.
C*
C*    INPUT     : IB1PAR - ARRAY OF DECODED PARAMETERS FROM BLOCK 1.
C*
C*    JOHN HENNESSY  ECMWF 28 MAY 1985
C*
C*
C**************************************************************
C----<
C
      DIMENSION IB1PAR(*)
C
      WRITE (*,9000)
 9000 FORMAT (1H0,'BLOCK 1 - PRODUCT DEFINITION BLOCK',/)
C
      WRITE (*,9001) IB1PAR(1)
 9001 FORMAT (1H0,'ORIGINATING CENTRE IDENTIFIER.       ',I9)
      WRITE (*,9002) IB1PAR(2)
 9002 FORMAT (1H ,'MODEL IDENTIFICATION.                ',I9)
      WRITE (*,9003) IB1PAR(3)
 9003 FORMAT (1H ,'GRID DEFINITION.                     ',I9)
      WRITE (*,9004) IB1PAR(4)
 9004 FORMAT (1H ,'FLAG (CODE TABLE 1)                  ',I9)
      WRITE (*,9005) IB1PAR(5)
 9005 FORMAT (1H ,'PARAMETER IDENTIFIER (CODE TABLE 2). ',I9)
      WRITE (*,9006) IB1PAR(6)
 9006 FORMAT (1H ,'TYPE OF LEVEL (CODE TABLE 3).        ',I9)
      WRITE (*,9007) IB1PAR(7)
 9007 FORMAT (1H ,'VALUE 1 OF LEVEL (CODE TABLE 3).     ',I9)
      WRITE (*,9008) IB1PAR(8)
 9008 FORMAT (1H ,'VALUE 2 OF LEVEL (CODE TABLE 3).     ',I9)
      WRITE (*,9009) IB1PAR(9)
 9009 FORMAT (1H ,'YEAR OF DATA.                        ',I9)
      WRITE (*,9010) IB1PAR(10)
 9010 FORMAT (1H ,'MONTH OF DATA.                       ',I9)
      WRITE (*,9011) IB1PAR(11)
 9011 FORMAT (1H ,'DAY OF DATA.                         ',I9)
      WRITE (*,9012) IB1PAR(12)
 9012 FORMAT (1H ,'HOUR OF DATA.                        ',I9)
      WRITE (*,9013) IB1PAR(13)
 9013 FORMAT (1H ,'MINUTE OF DATA.                      ',I9)
      WRITE (*,9014) IB1PAR(14)
 9014 FORMAT (1H ,'TIME UNIT (CODE TABLE 4).            ',I9)
      WRITE (*,9015) IB1PAR(15)
 9015 FORMAT (1H ,'TIME RANGE ONE.                      ',I9)
      WRITE (*,9016) IB1PAR(16)
 9016 FORMAT (1H ,'TIME RANGE TWO.                      ',I9)
      WRITE (*,9017) IB1PAR(17)
 9017 FORMAT (1H ,'TIME RANGE FLAG (CODE TABLE 5).      ',I9)
C
      RETURN
C
      END
