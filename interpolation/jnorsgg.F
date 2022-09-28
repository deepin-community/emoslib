C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION JNORSGG( PLAT, PGGLAT, KNUM, KUP)
C
C---->
C**** JNORSGG
C
C     PURPOSE
C     _______
C
C     This routine determines which latitude row in a gaussian grid is
C     north or south of a given latitude.
C
C
C     INTERFACE
C     _________
C
C     I = JNORSGG( PLAT, PGGLAT, KNUM, KUP)
C
C
C     Input parameters
C     ________________
C
C     PLAT   - Given latitude
C     PGGLAT - Array of gaussian grid latitudes (both north and south
C              values)
C     KNUM   - Gaussian grid number
C     KUP    - Indicator to say if the returned row number should be
C              north or south of the given latitude
C              = 0 for south, = 1 for north
C
C
C     Output parameters
C     ________________
C
C     Returned value is the latitude row number.
C
C
C     Common block usage
C     __________________
C
C     None
C
C
C     METHOD
C     ______
C
C     The nearest gaussian latitude row is found.
C
C
C     EXTERNALS
C     _________
C
C     None
C
C
C     REFERENCE
C     _________
C
C     None
C
C
C     COMMENTS
C     ________
C
C     All input values are assumed to be reasonable!
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers      ECMWF       Jan 1994
C
C
C     MODIFICATIONS
C     _____________
C
C     J.D.Chambers      ECMWF       Mar 1996
C     Guess position to start search (an optimisation dodge)
C
C     J.D.Chambers      ECMWF       Aug 1996
C     Accept latitude if within 1000th of a degree of stored latitude.
C
C     J.D.Chambers      ECMWF       Oct 1996
C     Accept latitude if within 100th of a degree of stored latitude.
C
C----<
C     _______________________________________________________
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
      IMPLICIT NONE
C
#include "parim.h"
C
C     Function arguments
C
      REAL PLAT, PGGLAT
      DIMENSION PGGLAT(*)
      INTEGER KNUM, KUP
C
C     Local variables
C
      INTEGER J202, NPOS, NPLAT, NGGLAT, IGUESS
      REAL JPFACTOR
      PARAMETER ( JPFACTOR = PPMULT/JPMICRO )
C
      REAL DELTA, OLDPLAT
      INTEGER IFIRST
      DATA IFIRST/0/, OLDPLAT/100.0/
      SAVE IFIRST, DELTA, IGUESS, OLDPLAT, NPLAT
C
C     _______________________________________________________
C
C*    Section 1. Initialization.
C     _______________________________________________________
C
  100 CONTINUE
C
C     Setup delta estimate between gaussian latitudes
      IF( IFIRST.NE. KNUM ) THEN
        IFIRST = KNUM
        DELTA  = 90.0/FLOAT(KNUM-1)
      ENDIF
C
      IF( OLDPLAT.NE.PLAT ) THEN
        OLDPLAT = PLAT
        IGUESS = INT( (90.0 - PLAT)/DELTA ) + 1
        NPLAT  = NINT(PLAT*JPFACTOR+0.5)
      ENDIF
C     _______________________________________________________
C
C*    Section 2. Processing.
C     _______________________________________________________
C
  200 CONTINUE
C
C     Check latitudes in the gaussian definition array
CDIR$ NOVECTOR
      DO 202 J202 = IGUESS, 2*KNUM
C       Accept latitude if within 100th of a degree of stored latitude.
        NGGLAT = NINT(PGGLAT(J202)*JPFACTOR+0.5)
        IF ( NPLAT .GE. NGGLAT ) THEN
          IF ( NPLAT .EQ. NGGLAT ) THEN
            NPOS = J202
          ELSE
            NPOS = J202 - KUP
          ENDIF
          GOTO 900
        ENDIF
  202 CONTINUE
C
C
C     On drop-through (e.g. for -90.0), use southernmost latitude
      NPOS = 2*KNUM
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
  900 CONTINUE
C
C     Allow for latitude above northernmost in array (e.g. 90.0).
      IF ( NPOS .LT. 1 ) NPOS = 1
C
      JNORSGG = NPOS
C
      RETURN
      END
