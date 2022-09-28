C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      INTEGER FUNCTION CNBITS( VALUES, LAT, LONG )
C
C---->
C**** CNBITS
C
C     Purpose.
C     --------
C
C     Calculate the number of bits needed to code a range of
C     field values.
C
C
C**   Interface.
C     ----------
C
C     CALL CNBITS( VALUES, LAT, LONG )
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
C
C
C     Output
C     ------
C
C     CNBITS return value is required number of bits.
C
C
C     Method.
C     -------
C
C     Finds power of 2 required to scan the range of values.
C
C
C     Externals.
C     ----------
C
C     None.
C
C
C     Reference.
C     ----------
C
C     None.
C
C
C     Comments.
C     ---------
C
C     None.
C
C
C     Author.
C     -------
C
C     J.D.Chambers           ECMWF      8 Nove 1994
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
C     Arguments
      INTEGER LAT,LONG
      REAL VALUES( LAT, LONG)
C
C     Local variables
      INTEGER ILAT,ILON
      REAL RMAXV, RMINV, RANGE
C   
C     ----------------------------------------------------------------
C*    Section 1 . Find maximum and minimum for the given values
C     ----------------------------------------------------------------
C                                                                
 100  CONTINUE
      CNBITS = 0
C
C     Find maximum and minimum for the given values
      RMINV = 0.0
      RMAXV = 0.0
C
      DO 120 ILAT = 1, LAT
        DO 110 ILON = 1, LONG
          IF ( RMINV .GT. (VALUES( ILAT, ILON)) )
     X      RMINV = (VALUES( ILAT, ILON))
          IF ( RMAXV .LT. (VALUES( ILAT, ILON)) )
     X      RMAXV = (VALUES( ILAT, ILON))
  110   CONTINUE
  120 CONTINUE
C
      RANGE = ABS( RMAXV - RMINV )
C
C     Calculate the number of bits needed to span the number range
      IF ( RANGE .EQ. 0.0 ) THEN
        CNBITS = 1
      ELSE
        CNBITS = INT ( (ALOG( RANGE )/ALOG(2.0)) ) + 1
      ENDIF
C
      RETURN
      END
