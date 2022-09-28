C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C


      SUBROUTINE PRTBK2 (IB2PAR)
C
C---->
C
C     NAME      : PRTBK2
C
C     FUNCTION  : PRINT THE INFORMATION IN THE GRDI DEFINITION
C                 BLOCK (BLOCK 2) OF DECODED GRIB DATA.
C
C     INPUT     : IB2PAR - ARRAY OF DECODED PARAMETERS FROM BLOCK 2.
C
C     OUTPUT    : FLAG FIELDS ARE PRINTED IN BINARY REPRESENTATION.
C                 OTHER FIELDS AS INTEGERS.
C
C     JOHN HENNESSY  ECMWF OCTOBER 1985
C
C     Modifications.
C     --------------
C
C         J. Hennessy   ECMWF  23:11:90
C         Output changed from all upper-case.
C
C     ---------------------------------------------------------------
C----<
C
      IMPLICIT INTEGER (A-Z)
C
      DIMENSION IB2PAR(*)
C
      WRITE (*,'(1H )')
      WRITE (*,9000)
 9000 FORMAT (1H ,'Section 2 - Grid definition section.')
C
      WRITE (*,'(1H )')
      WRITE (*,9001) IB2PAR(1)
 9001 FORMAT (1H ,'Data representation type (Table 6)   ',I9)
C
C     SPHERICAL HARMONIC DATA
C
      IF (IB2PAR(1).EQ.50)
     C   THEN
C
             WRITE (*,9002) IB2PAR(2)
 9002        FORMAT (1H ,'J - Pentagonal resolution parameter. ',I9)
             WRITE (*,9003) IB2PAR(3)
 9003        FORMAT (1H ,'K - Pentagonal resolution parameter. ',I9)
             WRITE (*,9004) IB2PAR(4)
 9004        FORMAT (1H ,'M - Pentagonal resolution parameter. ',I9)
             WRITE (*,9005) IB2PAR(5)
 9005        FORMAT (1H ,'Representation type (Table 9)        ',I9)
             WRITE (*,9006) IB2PAR(6)
 9006        FORMAT (1H ,'Representation mode (Table 10).      ',I9)
             WRITE (*,9007) (IB2PAR(I),I=7,11)
 9007        FORMAT (1H ,'Not used.                            ',I9)
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
 9102        FORMAT (1H ,'No. of points along a latitude.      ',I9)
             WRITE (*,9103) IB2PAR(3)
 9103        FORMAT (1H ,'No. of points along a meridian.      ',I9)
             WRITE (*,9104) IB2PAR(4)
 9104        FORMAT (1H ,'Latitude of origin.                  ',I9)
             WRITE (*,9200)
 9200        FORMAT (1H ,'(Southern latitudes are negative.)')
             WRITE (*,9105) IB2PAR(5)
 9105        FORMAT (1H ,'Longitude of origin.                 ',I9)
             WRITE (*,9201)
 9201        FORMAT (1H ,'(Western longitudes are negative.)')
             NBIT = 8
             CALL PRTBIN (IB2PAR(6),NBIT,OUT,ERR)
             WRITE (*,9106) OUT
 9106        FORMAT (1H ,'Resolution flag.                      ',I8.8)
             WRITE (*,9107) IB2PAR(7)
 9107        FORMAT (1H ,'Latitude of extreme point.           ',I9)
             WRITE (*,9200)
             WRITE (*,9108) IB2PAR(8)
 9108        FORMAT (1H ,'Longitude of extreme point.          ',I9)
             WRITE (*,9201)
             WRITE (*,9109) IB2PAR(9)
 9109        FORMAT (1H ,'East-West increment.                 ',I9)
             IF (IB2PAR(1).EQ.0)
     C          THEN
                    WRITE (*,9110) IB2PAR(10)
 9110               FORMAT (1H ,'North-South increment.               ',
     C                         I9)
                ENDIF
             IF (IB2PAR(1).EQ.4)
     C          THEN
                    WRITE (*,8110) IB2PAR(10)
 8110               FORMAT(1H ,'No. of latitude lines pole / equator.',
     C                         I9)
                ENDIF
             NBIT = 8
             CALL PRTBIN (IB2PAR(11),NBIT,OUT,ERR)
             WRITE (*,9111) OUT
 9111        FORMAT (1H ,'Scanning mode flags (Code Table 8)    ',I8.8)
C
             RETURN
C
         ENDIF
C
      END
