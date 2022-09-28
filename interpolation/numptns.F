C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION NUMPTNS(NORTH,SOUTH,GRIDSTEP)
C
C---->
C**** NUMPTNS
C
C     Purpose
C     -------
C
C     Calculate number of grid points in the range from north to south.
C
C
C     Interface
C     ---------
C
C     IRET = NUMPTNS(NORTH,SOUTH,GRIDSTEP)
C
C     Input
C     -----
C
C     NORTH    - Northern boundary in degrees
C     SOUTH    - Southern boundary in degrees
C     GRIDSTEP - Grid interval in degrees
C
C
C     Output
C     ------
C
C     Function returns the number of grid points in the north-south
C     interval.
C
C
C     Method
C     ------
C
C     The grid is assumed to be centred on the Equator.
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
C     J.D.Chambers     ECMWF     Dec 1997
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
      REAL SOUTH, NORTH, GRIDSTEP
C
C     Local variables
C
      INTEGER N1, N2
      REAL TOP, BOTTOM
C
C ------------------------------------------------------------------
C     Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
C
      BOTTOM = SOUTH
      TOP    = NORTH
      IF( BOTTOM.GT.TOP) THEN
        BOTTOM = NORTH
        TOP    = SOUTH
      ENDIF
C ------------------------------------------------------------------
C     Section 2.   Calculate the number of points in the interval
C ------------------------------------------------------------------
C
  200 CONTINUE
C
      N1 = NINT(BOTTOM/GRIDSTEP)
      N2 = NINT(TOP/GRIDSTEP)
      NUMPTNS = N2 - N1 + 1
C
C ------------------------------------------------------------------
C     Section 9.   Return
C ------------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END
