C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      LOGICAL FUNCTION PRECIP()
C
C---->
C**** PRECIP
C
C     Purpose
C     -------
C
C     Determines whether or not the current field is to be
C     considered for 'precipitation' treatment.
C
C
C     Interface
C     ---------
C
C     LPRECIP = PRECIP()
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
C     Function returns .TRUE. if the field is to be treated as
C     precipitation.
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
C     If 'useprecip' has been forced to 'yes' ...
C
      IF( LPRECSET ) THEN
        PRECIP = LPREC
        GOTO 900
      ENDIF
C
C     WMO International table 001 for meteorological parameters
C
      IF ( NITABLE.EQ.1 ) THEN
        LIKELY = ( NIPARAM.EQ.61 ).OR.
     X           ( NIPARAM.EQ.62 ).OR.
     X           ( NIPARAM.EQ.63 ).OR.
     X           ( NIPARAM.EQ.64 ).OR.
     X           ( NIPARAM.EQ.65)
C
C     ECMWF local code table 128 for meteorological parameters
C
      ELSEIF ( (NITABLE.EQ.128).OR.(NITABLE.EQ.129) ) THEN
        LIKELY = ( NIPARAM.EQ.142 ).OR.
     X           ( NIPARAM.EQ.143 ).OR.
     X           ( NIPARAM.EQ.144 ).OR.
     X           ( NIPARAM.EQ.228 ).OR.
     X           ( NIPARAM.EQ.239 ).OR.
     X           ( NIPARAM.EQ.240 )
      ELSE
        LIKELY = .FALSE.
      ENDIF
C
      IF( .NOT.LIKELY ) THEN
        PRECIP = .FALSE.
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2. Work through the special cases
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Monthly forecast means using ECMWF local definition 16
C
      IF( NILOCAL.EQ.16 ) THEN
        PRECIP = .FALSE.
        GOTO 900
      ENDIF
C
C     Extreme forecast index using ECMWF local definition 19
C
      IF( NILOCAL.EQ.19 ) THEN
        PRECIP = .FALSE.
        GOTO 900
      ENDIF
C
C     Coupled Atmospheric, Wave and Ocean models (with hindcast support)
C     using ECMWF local definition 23
C
      IF( NILOCAL.EQ.23 ) THEN
        PRECIP = .FALSE.
        GOTO 900
      ENDIF
C
C     ERA fields using code table 160
C
      IF( NITABLE.EQ.160 ) THEN
        PRECIP = .FALSE.
        GOTO 900
      ENDIF
C
      PRECIP = .TRUE.
C
C     -----------------------------------------------------------------|
C*    Section 9.   Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      IF( .NOT.PRECIP ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'PRECIP: Do not handle field as precipitation', JPQUIET)
      ELSE
        CALL INTLOG(JP_DEBUG,
     X    'PRECIP: Handle field as precipitation', JPQUIET)
      ENDIF
C
      RETURN
      END
