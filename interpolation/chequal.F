C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      LOGICAL FUNCTION CHEQUAL( HIN, KFIRST, KLAST, HCOMP)
C
C---->
C**** CHEQUAL
C
C     Purpose
C     -------
C
C     Compare two ASCII character strings without regard to case.
C
C
C     Interface
C     ---------
C
C     Input
C     -----
C
C     HIN   - First string
C     KFIRST- position of first character in string for comparison
C     KLAST - position of last character in string for comparison
C     HCOMP - Second string
C
C
C     Method
C     ------
C
C     Compare values as uppercase letters.
C
C
C     Externals
C     ---------
C
C     None.
C
C
C     Author
C     ------
C
C     J.D.Chambers       ECMWF        January 1995.
C
C----<
C
      IMPLICIT NONE
C
C     Function arguments
C
      CHARACTER*(*) HIN, HCOMP
      INTEGER  KFIRST, KLAST, LOOP, ITEMP1, ITEMP2, ITEMPA
#ifdef rs6000
      ITEMPA = ICHAR( 'a' )
#else
      DATA ITEMPA/97/  !ITEMPA = ICHAR( 'a' )
#endif

      CHEQUAL = .FALSE.
      IF( (KFIRST.GT.KLAST)
     .    .OR. (LEN(HIN)  .LT.KLAST)
     .    .OR. (LEN(HCOMP).LT.KLAST) ) RETURN

      DO LOOP =  KFIRST, KLAST
        ITEMP1 = ICHAR( HIN  (LOOP:LOOP) )
        ITEMP2 = ICHAR( HCOMP(LOOP:LOOP) )
        IF ( ITEMP1 .LT. ITEMPA ) ITEMP1 = ITEMP1 + 32
        IF ( ITEMP2 .LT. ITEMPA ) ITEMP2 = ITEMP2 + 32
        IF ( ITEMP1 .NE. ITEMP2 ) RETURN
      ENDDO

      CHEQUAL = .TRUE.

      END
