C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GRSREF (PREF)
C
C---->
C**** GRSREF - Set user defined reference value for routine GRIBEX.
C
C     Purpose.
C     --------
C
C           Set user defined reference value for GRIB code for
C           routine GRIBEX.
C
C**   Interface.
C     ----------
C
C           CALL GRSREF (PREF)
C
C           Integer    K.
C           Real       P.
C           Logical    O.
C           Character  H.
C
C               Input Parameters.
C               -----------------
C
C               PREF        - Reference value required.
C
C               Output Parameters.
C               ------------------
C
C               None.
C
C     Method.
C     -------
C
C           The flag variable NFREF is set to indicate that the
C           user has supplied a reference value and the variable
C           FREF is set to the required reference value. These
C           variables are in the common area. All other parameters
C           are also set to default values, if they have not already
C           been set by the user.
C
C     Externals.
C     ----------
C
C           GRSDEF - Default setting of global variables.
C
C     Reference.
C     ----------
C
C           See subroutine GRIBEX.
C
C     Comments.
C     ---------
C
C           Routine contains Sections 0 to 1 and Section 9.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      25.06.91
C
C     Modifications.
C     --------------
C
C           J. Hennessy      ECMWF      28.08.91
C           Changes to some comments only.
C
C           J. Hennessy      ECMWF      14.11.91
C           Changes to common area.
C
C           J.D.Chambers     ECMWF      13.09.94
C           Add NONOFF and NOABORT, use grbcom.h
C
C           J. Clochard, Meteo France, for ECMWF - January 1998.
C           Use GRSDEF routine for default setting.
C
C           J. Clochard, Meteo France, for ECMWF - June 1999.
C           Use a static variable to determine if initialisation of
C           default values has already been done for common area
C           variables.
C
C----<
C     ----------------------------------------------------------------
C*    Section 0 . Definition of variables.
C     ----------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "grbcom.h"
C
      REAL    PREF
C
      LOGICAL LFIRST
      SAVE LFIRST
C
      DATA LFIRST / .TRUE. /
C
C     ----------------------------------------------------------------
C*    Section 1 . Set values.
C     ----------------------------------------------------------------
C
  100 CONTINUE
C
C     If common area variables have not been set, start by
C     setting default values.
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
C
C     Set reference value to required value.
C
      FREF   = PREF
C
C     Reference value supplied by user flag. Set to on.
C
      NFREF  = 1
C
C     ----------------------------------------------------------------
C*    Section 9 . Return to calling routine.
C     ----------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
C
      END
