C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GENGRIB( VALUES, LAT, LONG, NBITS, GRIB, GRIBSIZE)
C
C---->
C**** GENGRIB
C
C     Purpose.
C     --------
C
C     Creates a generic GRIB field product.
C
C
C**   Interface.
C     ----------
C
C     CALL GENGRIB( VALUES, LAT, LONG, NBITS, GRIB, GRIBSIZE)
C
C
C     Input
C     -----
C
C     VALUES   -  array of real numbers
C     LAT      -  first dimension of the array
C                 (e.g. number of points along a meridian )
C     LONG     -  second dimension of the array
C                 (e.g. number of points along a parallel )
C     NBITS    -  number of bits to use for each packed value
C                 If NBITS = 0, a value is calculated for the given
C                 field.
C     GRIBSIZE -  size in bytes of output array GRIB
C
C
C     Output
C     ------
C
C     GRIB     -  packed GRIB record
C
C
C     Method.
C     -------
C
C     Packs the matrix of real values as a 500 hPa geopotential first
C     guess field for 94/1/1 1200.
C
C
C     Externals.
C     ----------
C
C     GRIBEX
C     CNBITS
C
C
C     Reference.
C     ----------
C
C     Binary data representation - FM 92 GRIB, ECMWF, May 1992
C
C
C     Comments.
C     ---------
C
C     If number of bits for packing is not supplied by caller, calculate
C     the number of bits needed to code the range of field values.
C     Apply (arbitrary) lower limit unless the number of bits is 1 (for
C     bit fields for example) when the value is left as 1.
C
C
C     Author.
C     -------
C
C     J.D.Chambers           ECMWF      28.07.94
C
C
C     Modifications.
C     --------------
C
C     None.
C
C   
C----<
C     ----------------------------------------------------------------
C*    Section 0 . Definition of variables.
C     ----------------------------------------------------------------
C                                                                
      IMPLICIT NONE 
C
C     Parameters
      INTEGER JPLOW
      PARAMETER ( JPLOW = 16 )
C
C     Arguments
      INTEGER LAT,LONG
      REAL VALUES( LAT, LONG)
      INTEGER GRIB(*), GRIBSIZE
      INTEGER NBITS
C
C     Local variables
      INTEGER KSEC0(2), KSEC1(43), KSEC2(23), KSEC3(2), KSEC4(33)
      REAL PSEC2(10), PSEC3(2)
      INTEGER LOOP, KLENG, KLENP, KWORD, KRET
C
      INTEGER CNBITS
C                                             
C     ----------------------------------------------------------------
C*    Section 1 . Setup section 1.
C     ----------------------------------------------------------------
C                                                                    
  100 CONTINUE                                                    
C                                                               
      KSEC1( 1) = 128
      KSEC1( 2) = 98
      KSEC1( 3) = 255
      KSEC1( 4) = 255
      KSEC1( 5) = 128
      KSEC1( 6) = 129
      KSEC1( 7) = 100
      KSEC1( 8) = 500
      KSEC1( 9) = 0
      KSEC1(10) = 94
      KSEC1(11) = 1
      KSEC1(12) = 1
      KSEC1(13) = 12
      KSEC1(14) = 0
      KSEC1(15) = 1
      KSEC1(16) = 0
      KSEC1(17) = 0
      KSEC1(18) = 0
      KSEC1(19) = 0
      KSEC1(20) = 0
      KSEC1(21) = 20
      DO 110 LOOP = 22, 36
        KSEC1(LOOP) = 0
  110 CONTINUE
      KSEC1(24) = 1
      KSEC1(37) = 1
      KSEC1(38) = 1
      KSEC1(39) = 1
      KSEC1(40) = 1060
      KSEC1(41) = 1413829460
      KSEC1(42) = 0
      KSEC1(43) = 0
C                                             
C     ----------------------------------------------------------------
C*    Section 2 . Setup section 2.
C     ----------------------------------------------------------------
C                                                                    
  200 CONTINUE                                                    
C                                                               
      KSEC2( 1) = 0
      KSEC2( 2) = LONG
      KSEC2( 3) = LAT
      KSEC2( 4) = 90000
      KSEC2( 5) = 0
      KSEC2( 6) = 128
      KSEC2( 7) = -90000
      KSEC2( 8) = 360000 - (360000/LONG)
      KSEC2( 9) = (360000/LONG)
      KSEC2(10) = (180000/(LAT-1))
      KSEC2(11) = 0
      KSEC2(12) = 0
      KSEC2(13) = 0
      KSEC2(14) = 0
      KSEC2(15) = 0
      KSEC2(16) = 0
      KSEC2(17) = 0
      KSEC2(18) = 0
      KSEC2(19) = 0
      KSEC2(20) = 0
      KSEC2(21) = 0
      KSEC2(22) = 0
      KSEC2(23) = LONG
      DO 210 LOOP =1, 10
        PSEC2(LOOP) = 0.0
  210 CONTINUE
C                                             
C     ----------------------------------------------------------------
C*    Section 3 . Setup section 3.
C     ----------------------------------------------------------------
C                                                                    
  300 CONTINUE                                                    
C                                                               
      KSEC3( 1) = 0
      KSEC3( 2) = -1000000
      PSEC3( 1) = 0
      PSEC3( 2) = -1000000.0
C                                             
C     ----------------------------------------------------------------
C*    Section 4 . Setup section 4.
C     ----------------------------------------------------------------
C                                                                    
  400 CONTINUE                                                    
C                                                               
      KSEC4( 1) = LAT*LONG
C
      IF ( NBITS .LE. 0 ) THEN
C
C       If number of bits for packing not supplied by caller, calculate
C       the number of bits needed to code the range of field values.
        KSEC4( 2) = CNBITS( VALUES, LAT, LONG )
C
C       Apply (arbitrary) lower limit.
        IF ( (KSEC4( 2) .LT. JPLOW) .AND.
     X       (KSEC4( 2) .NE. 1) ) KSEC4( 2) = JPLOW
      ELSE
        KSEC4( 2) = NBITS
      ENDIF
      DO 410 LOOP = 3, 33
        KSEC4(LOOP) = 0
  410 CONTINUE
C                                             
C     ----------------------------------------------------------------
C*    Section 5 . Pack the field.
C     ----------------------------------------------------------------
C                                                                    
  500 CONTINUE                                                    
C                                                               
#ifdef CRAY
      KLENG = GRIBSIZE/8
#else
      KLENG = GRIBSIZE/4
#endif
      KLENP = LAT*LONG
C
Cjdc  CALL GRSVCK(0)
Cjdc  CALL GRSDBG(1)
C
      CALL GRIBEX( KSEC0, KSEC1, KSEC2, PSEC2, KSEC3, PSEC3, KSEC4,
     X             VALUES, KLENP, GRIB, KLENG, KWORD, 'C', KRET)
C                                           
C     ----------------------------------------------------------------
C*    Section 9 . End.
C     ----------------------------------------------------------------
C                                                                
  900 CONTINUE                                           
C                                                    
      RETURN
      END
