C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GRSMOK ( KUM2OK )
C
C---->
C**** GRSMOK - controls use of Methods for GRIBEX Option 'K'
C              (2nd-order aggressive packing).
C
C     Purpose.
C     --------
C
C     Exhaustive 2nd-order aggressive packing switch set on or off
C     for routine GRIBEX.
C
C
C**   Interface.
C     ----------
C
C     CALL GRSMOK ( KUM2OK )
C
C
C     Input Parameter.
C     ----------------
C
C     KUM2OK  - Exhaustive 2nd-order aggressive packing switch flag,
C               = 0 ,        user supplies the allowed methods (default)
C               = Non-zero , all applicable methods.
C
C               (Use for HOPER='K' function)
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
C     The variable NUM2OK is set in the common area. All other
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
C     J. Clochard, Meteo France, for ECMWF - January 1998.
C
C
C     Modifications.
C     --------------
C
C     J. Clochard, Meteo France, for ECMWF - June 1999.
C     Use a static variable to determine if initialisation of
C     default values has already been done for common area variables.
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
      INTEGER KUM2OK
C
C     Local variables.
      INTEGER IUM2OK
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
      IUM2OK = 0
      IF (KUM2OK.NE.0) IUM2OK = 1
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
C       Set exhaustive 2nd-order aggressive packing switch
C       to required value.
C
      NUM2OK   = IUM2OK
C
C     ----------------------------------------------------------------
C*    Section 9 . Return to calling routine.
C     ----------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END
