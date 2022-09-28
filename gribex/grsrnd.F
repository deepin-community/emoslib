C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE GRSRND (KRND)
C
C---->
C**** GRSRND - Set GRIB code rounding/no rounding for routine GRIBEX.
C
C     Purpose.
C     --------
C
C           Switch GRIB length rounding on/off for routine GRIBEX.
C
C**   Interface.
C     ----------
C
C           CALL GRSRND (KRND)
C
C           Integer    K.
C           Real       P.
C           Logical    O.
C           Character  H.
C
C               Input Parameters.
C               -----------------
C
C               KRND        - Value checking switch.
C                            0 , No rounding.
C                            Non-zero , Values rounded.
C
C               Output Parameters.
C               ------------------
C
C               GRSDEF - Default setting of global variables.
C
C     Method.
C     -------
C
C           The variable NRND is set in the common area. All other
C           parameters are also set to default values, if they have
C           not already been set by the user.
C
C     Externals.
C     ----------
C
C           None.
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
C           J. Hennessy      ECMWF      07.01.92
C           Input parameter returned unchanged.
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
      INTEGER KRND
C
      INTEGER IRND
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
      IRND = 0
      IF (KRND.NE.0) IRND = 1
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
C     Set rounding to 120 bytes to required value.
C
      NRND   = IRND
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
