C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE PRTBK1 (IB1PAR)
C
C---->
C
C     NAME      : PRTBK1
C
C     FUNCTION  : PRINT THE INFORMATION IN THE PRODUCT DEFINITION
C                 BLOCK (BLOCK 1) OF DECODED GRIB DATA.
C
C     INPUT     : IB1PAR - ARRAY OF DECODED PARAMETERS FROM BLOCK 1.
C
C     OUTPUT    : FLAG FIELDS ARE PRINTED IN BINARY REPRESENTATION.
C                 OTHER FIELDS AS INTEGERS.
C
C     JOHN HENNESSY  ECMWF OCTOBER 1985
C
C      Modifications.
C      --------------
C            J. Hennessy   ECMWF 23:11:90
C            Printed output changed from all upper-case.
C
C     ---------------------------------------------------------------
C----<
C
      IMPLICIT INTEGER (A-Z)
C
      DIMENSION IB1PAR(*)
C
      WRITE (*,'(1H )')
      WRITE (*,9000)
 9000 FORMAT (1H ,'Section 1 - Product definition section.')
C
      WRITE (*,'(1H )')
      WRITE (*,9001) IB1PAR(1)
 9001 FORMAT (1H ,'Originating centre identifier.       ',I9)
      WRITE (*,9002) IB1PAR(2)
 9002 FORMAT (1H ,'Model identification.                ',I9)
      WRITE (*,9003) IB1PAR(3)
 9003 FORMAT (1H ,'Grid definition.                     ',I9)
      NBIT = 8
      CALL PRTBIN (IB1PAR(4),NBIT,OUT,ERR)
      WRITE (*,9004) OUT
 9004 FORMAT (1H ,'Flag (Code Table 1)                   ',I8.8)
      WRITE (*,9005) IB1PAR(5)
 9005 FORMAT (1H ,'Parameter identifier (Code Table 2). ',I9)
      WRITE (*,9006) IB1PAR(6)
 9006 FORMAT (1H ,'Type of level (Code Table 3).        ',I9)
      WRITE (*,9007) IB1PAR(7)
 9007 FORMAT (1H ,'Value 1 of level (Code Table 3).     ',I9)
      WRITE (*,9008) IB1PAR(8)
 9008 FORMAT (1H ,'Value 2 of level (Code Table 3).     ',I9)
      WRITE (*,9009) IB1PAR(9)
 9009 FORMAT (1H ,'Year of reference time of data.      ',I9)
      WRITE (*,9010) IB1PAR(10)
 9010 FORMAT (1H ,'Month of reference time of data.     ',I9)
      WRITE (*,9011) IB1PAR(11)
 9011 FORMAT (1H ,'Day of reference time of data.       ',I9)
      WRITE (*,9012) IB1PAR(12)
 9012 FORMAT (1H ,'Hour of reference time of data.      ',I9)
      WRITE (*,9013) IB1PAR(13)
 9013 FORMAT (1H ,'Minute of reference time of data.    ',I9)
      WRITE (*,9014) IB1PAR(14)
 9014 FORMAT (1H ,'Time unit (Code Table 4).            ',I9)
      WRITE (*,9015) IB1PAR(15)
 9015 FORMAT (1H ,'Time range one.                      ',I9)
      WRITE (*,9016) IB1PAR(16)
 9016 FORMAT (1H ,'Time range two.                      ',I9)
      WRITE (*,9017) IB1PAR(17)
 9017 FORMAT (1H ,'Time range indicator (Code Table 5)  ',I9)
      WRITE (*,9018) IB1PAR(18)
 9018 FORMAT (1H ,'Number averaged.                     ',I9)
C
      RETURN
C
      END
