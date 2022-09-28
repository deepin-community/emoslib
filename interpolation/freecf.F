C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE FREECF(KFLAG)
C
C---->
C**** FREECF
C
C     PURPOSE
C     _______
C
C     This routine sets the flag controlling handling of spectral
C     to grid interpolation coefficients memory.
C
C
C     INTERFACE
C     _________
C
C     CALL FREECF(KFLAG)
C
C     Input parameters
C     ________________
C
C     KFLAG - Flag indicating whether flushing of memory is done or not
C              = 1 to turn on flushing
C              = any other value to turn off flushing (default)
C
C
C     Output parameters
C     ________________
C
C     None
C
C
C     Common block usage
C     __________________
C
C     JDCSPGP - flag LFREECF
C
C
C     Method
C     ______
C
C     Sets flags LFREECF and NFREECF
C
C
C     Externals
C     _________
C
C     None
C
C
C     Reference
C     _________
C
C     None
C
C
C     Comments
C     ________
C
C     None.
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers       ECMWF       March 1998
C
C
C     MODIFICATIONS
C     _____________
C
C     None
C
C----<
C
      IMPLICIT NONE
C
#include "jparam2.h"
C
C     Subroutine arguments
C
      INTEGER KFLAG
C
      IF( KFLAG.EQ.1 ) THEN
        LFREECF = .TRUE.
        NFREECF = 11041967
      ELSE
        LFREECF = .FALSE.
        NFREECF = 11041967
      ENDIF
C
      RETURN
      END
