C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION YYYY2CY(YYYY, CENTURY, YEAR)
C
C---->
C**** YYYY2CY
C
C     Purpose
C     -------
C
C     Return WMO defined century and year from YYYY date.
C
C
C     Interface
C     ---------
C
C     IRET = YYYY2CY(YYYY, CENTURY, YEAR)
C
C     Input
C     -----
C
C     YYYY    = Integer date in YYYY formay (eg 1998)
C
C
C     Output
C     ------
C
C     CENTURY = Integer century in CC format (eg 20 for 1998)
C     YEAR    = Integer year in range 1 to 100 (eg 98 for 1998)
C
C     Returns 0 if function worked OK.
C     Otherwise an illegal year has been give.
C
C
C     Method
C     ------
C
C     Last year in century is year 100
C     (eg year 2000 is century 20, year 100)
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     Sept 1998
C
C
C ------------------------------------------------------------------
C----<
C
      IMPLICIT NONE
C
C     Function arguments
C
      INTEGER YYYY, CENTURY, YEAR
C
C ------------------------------------------------------------------
C*    Section 1.   Check valid input year (0000 to 9999)
C ------------------------------------------------------------------
C
  100 CONTINUE
C
C     Check valid input year (0000 to 9999)
C
      IF( (YYYY.LT.0).OR.(YYYY.GT.9999) ) THEN
        YYYY2CY = 1
        RETURN
      ENDIF
C
      YEAR = YYYY - 1
      YEAR = MOD(YEAR,100) + 1
C
      CENTURY = (YYYY/100 +1 )
      IF( YEAR.EQ.100 ) CENTURY = CENTURY - 1
C
C ------------------------------------------------------------------
C*    Section 9.   Return
C ------------------------------------------------------------------
C
  900 CONTINUE
C
      YYYY2CY = 0
      RETURN
      END

