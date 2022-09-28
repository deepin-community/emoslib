C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION NUMPTWE(WEST,EAST,GRIDSTEP)
C
C---->
C**** NUMPTWE
C
C     Purpose
C     -------
C
C     Calculate number of grid points in the range from west to east.
C
C
C     Interface
C     ---------
C
C     IRET = NUMPTWE(WEST,EAST,GRIDSTEP)
C
C     Input
C     -----
C
C     WEST     - Western boundary in degrees
C     EAST     - Eastern boundary in degrees
C     GRIDSTEP - Grid interval in degrees
C
C
C     Output
C     ------
C
C     Function returns the number of grid points in the east-west
C     interval.
C
C
C     Method
C     ------
C
C     The grid is assumed to start at Greenwich.
C
C
C     Externals
C     ---------
C
C     None.
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     Nov 1997
C
C
C     Modifications
C     -------------
C
C     None.
C
C----<
C
      IMPLICIT NONE
C
C     Function arguments
C
      REAL EAST, WEST, GRIDSTEP
C
C     Local variables
C
      INTEGER NL
      REAL*8 DEAST, DWEST
      REAL*8 LEFT, RIGHT, STEP, EPS
      DATA EPS/1.0E-3/
C
C     Statement function
      LOGICAL EQUAL
      REAL*8 A, B
      EQUAL(A,B) = ( ABS(A-B) .LT.EPS )
C
C ------------------------------------------------------------------
C     Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
C
C     Make all longitudes positive for calculation of difference
C
      DEAST = EAST
      DWEST = WEST
      RIGHT = DEAST + 360.0
      LEFT  = DWEST + 360.0
      STEP  = GRIDSTEP
      IF( RIGHT.LT.LEFT) RIGHT = RIGHT + 360.0
C
C ------------------------------------------------------------------
C     Section 2.   Calculate the number of points in the interval
C ------------------------------------------------------------------
C
  200 CONTINUE
C
C     Equal boundaries
C
      IF( EQUAL(LEFT,RIGHT) ) THEN
        NUMPTWE = 1
        GOTO 900
      ENDIF
C
C     Calculate offset of westernmost gridpoint
C
      NL = INT((LEFT+EPS)/STEP)
      IF( .NOT. EQUAL(LEFT,(NL*STEP)) ) NL = NL + 1
C
      RIGHT = RIGHT - (NL*STEP)
      NUMPTWE = INT((RIGHT+EPS)/STEP) + 1
C
C ------------------------------------------------------------------
C     Section 9.   Return
C ------------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END
