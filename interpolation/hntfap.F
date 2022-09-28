C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HNTFAP(INGRIB,INLEN)
C
C---->
C**** HNTFAP
C
C     Purpose
C     -------
C
C     Prepare to interpolate input field...
C
C
C     Interface
C     ---------
C
C     IRET = HNTFAP(INGRIB,INLEN)
C
C     Input
C     -----
C
C     INGRIB - Input field (packed).
C     INLEN  - Input field length (words).
C
C
C     Output
C     ------
C
C     Field unpacked values are in ZNFELDI, rotated if necessary.
C
C     Returns: 0, if OK. Otherwise, an error occured in interpolation.
C
C
C     Method
C     ------
C
C     Calls HNTFAPS to handle rotation when input and output fields
C     are both spectral.
C
C     Calls HNTFAPH in all other cases.
C
C
C     Externals
C     ---------
C
C     HNTFAPS - Handle rotation when input and output fields
C               are both spectral.
C     HNTFAPH - Handle rotation in all other cases.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     January 31, 2001
C
C
C----<
C
C     -----------------------------------------------------------------|
C*    Section 0.   Variables
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "grfixed.h"
#include "intf.h"
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 40100 )
C
C     Function arguments
C
      INTEGER INGRIB(*),INLEN
C
C     Local variables
C
      INTEGER IRET
C
C     Externals
C
      INTEGER  HNTFAPS, HNTFAPH
      EXTERNAL HNTFAPS, HNTFAPH
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      HNTFAP = 0
C
C     -----------------------------------------------------------------|
C*    Section 2.   Split handling in case there are rotations to do.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Is the interpolation from SH to SH?
C
      IF( ((NIREPR.EQ.JPSPHERE).OR.(NIREPR.EQ.JPSPHROT)) .AND.
     X    ((NOREPR.EQ.JPSPHERE).OR.(NOREPR.EQ.JPSPHROT)) ) THEN
C
        IRET = HNTFAPS(INGRIB,INLEN)
C
      ELSE
C
        IRET = HNTFAPH(INGRIB,INLEN)
C
      ENDIF
C
      IF( IRET.NE.0 ) HNTFAP = JPROUTINE
C
C     -----------------------------------------------------------------|
C*    Section 9.   Closedown.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
