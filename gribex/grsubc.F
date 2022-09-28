C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GRSUBC ( KSUBCE )
C
C---->
C**** GRSUBC - controls use of SUB-Centre number use for ECMWF fields.
C
C     Purpose.
C     --------
C
C     Sub-centre use for ECMWF fields encoding switch set on or off
C     for routine GRIBEX.
C
C
C**   Interface.
C     ----------
C
C     CALL GRSUBC ( KSUBCE )
C
C
C     Input Parameter.
C     ----------------
C
C     KSUBCE  -  Sub-centre use for ECMWF fields encoding switch flag,
C               = 0 ,        only explicit values allowed (default)
C               = Non-zero , all valid octet values allowed .
C
C               (Use for encoding type functions)
C
C     Output Parameters.
C     ------------------
C
C     None.
C
C
C     Method.
C     -------
C
C     The variable NSUBCE is set in the common area. All other
C     parameters are also set to default values, if they have
C     not already been set by the user.
C
C
C     Externals.
C     ----------
C
C     GRSDEF - Default setting of global variables.
C
C
C     Reference.
C     ----------
C
C     See subroutine GRIBEX.
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
C     J. Clochard, Meteo France, for ECMWF - June 1999.
C
C
C     Modifications.
C     --------------
C
C     None.
C
C
C     ----------------------------------------------------------------
C*    Section 0 . Definition of variables.
C     ----------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "grbcom.h"
C
C     Subroutine arguments.
      INTEGER KSUBCE
C
C     Local variables.
      INTEGER ISUBCE
C
      LOGICAL LFIRST
      SAVE LFIRST
C
      DATA LFIRST / .TRUE. /
C
C----<
C     ----------------------------------------------------------------
C*    Section 1 . Set values.
C     ----------------------------------------------------------------
C
  100 CONTINUE
C
      ISUBCE = 0
      IF (KSUBCE.NE.0) ISUBCE = 1
C
      IF (LFIRST) THEN
C
C       Common area variables may have not been set, so start by
C       trying to set default values.
C
        CALL GRSDEF
        LFIRST=.FALSE.
C
      ENDIF
C
C       Set sub-centre use for ECMWF fields encoding switch
C       to required value.
C
      NSUBCE   = ISUBCE
C
C     ----------------------------------------------------------------
C*    Section 9 . Return to calling routine.
C     ----------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END
