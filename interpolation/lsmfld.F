C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      LOGICAL FUNCTION LSMFLD()
C
C---->
C**** LSMFLD
C
C     Purpose
C     -------
C
C     Determines whether or not the current field is to be
C     considered for 'land-sea mask' treatment.
C
C
C     Interface
C     ---------
C
C     LLSMFLD = LSMFLD()
C
C     Input
C     -----
C
C     None
C
C
C     Output
C     ------
C
C     Function returns .TRUE. if the field is to be treated with
C     land-sea mask.
C
C
C     Method
C     ------
C
C     Uses a set of hardcoded rules.
C
C
C     Externals
C     ---------
C
C     INTLOG  - Logs messages
C
C     Uses common blocks nifld.common and nofld.common.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     April 2001
C
C----<
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
C
C     Local variables
C
      LOGICAL LIKELY
C
C     -----------------------------------------------------------------|
C*    Section 1. Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
C     If 'uselsm' has been forced to 'yes' ...
C
      LSMFLD = .FALSE.
      IF( LSMSET ) THEN
        LSMFLD = LSM
        GOTO 900
      ENDIF
C
C     WMO International table 001 for meteorological parameters
C
      IF ( NITABLE.EQ.1 ) THEN
        LIKELY = ( NIPARAM.NE.2 )
C
C     ECMWF local code table 128 for meteorological parameters
C
      ELSEIF ( (NITABLE.EQ.128).OR.(NITABLE.EQ.129) ) THEN
        LIKELY = ( NIPARAM.NE.151 ).AND.  !msl
     X           ( NIPARAM.NE.172 )       !lsm
      ELSE
        LIKELY = .FALSE.
      ENDIF
C
      IF( .NOT.LIKELY ) THEN
        LSMFLD = .FALSE.
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2. Work through the special cases
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     -----------------------------------------------------------------|
C*    Section 9.   Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      IF( .NOT.LSMFLD ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'LSMFLD: Do not handle field with land-sea mask', JPQUIET)
      ELSE
        CALL INTLOG(JP_DEBUG,
     X    'LSMFLD: Handle field with land-sea mask', JPQUIET)
      ENDIF
C
      RETURN
      END
