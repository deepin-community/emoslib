C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GRPRS4W(KSEC4)
C
C---->
C**** GRPRS4W
C
C     Purpose.
C     --------
C
C     Print the wave coordinate information in the Binary data
C     section Section (Section 4) of decoded GRIB data.
C
C**   Interface.
C     ----------
C
C     CALL GRPRS4W(KSEC4)
C
C
C     Input Parameters.
C     -----------------
C
C     KSEC4 - Array of decoded integers from Section 4.
C
C     Output Parameters.
C     ------------------
C
C     None.
C
C     Method.
C     -------
C
C     Fields printed as reals.
C
C     Externals.
C     ----------
C
C     SETPAR
C     INXBIT.
C
C     Common block GRPRSCM.
C
C     Reference.
C     ----------
C
C     WMO Manual on Codes re GRIB Code.
C     See also routine GRIBEX.
C
C     Comments.
C     ---------
C
C     Wave coordinate information held in KSEC4 are 32-bit REALs,
C     hence the PTEMP and NTEMP used for printing are 4-byte variables.
C
C     Author.
C     -------
C
C     J.D.Chambers   ECMWF 28.10.97
C     Add printout for ECMWF wave 2Dsp first and second dimension
C     coordinate values
C
C     J.D.Chambers   ECMWF    June 2002
C     Add option to set output stream number using environment variable
C     GRPRS_STREAM
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Subroutine arguments
      INTEGER KSEC4
      DIMENSION KSEC4(*)
C
C     Local variables
C
      INTEGER JLOOP
      REAL*4    PTEMP(100)
      INTEGER*4 NTEMP(100)
      EQUIVALENCE ( PTEMP(1),NTEMP(1) )
C
C     -----------------------------------------------------------------|
C*    Section 1 . Print integer information from KSEC4.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      WRITE(GRPRSM,*)
     X  'Coefficients defining first dimension coordinates:'
      DO 110 JLOOP = 1, KSEC4(53)
        NTEMP(JLOOP) = KSEC4(59 + JLOOP)
        WRITE(GRPRSM,'(F20.10)') PTEMP(JLOOP)
  110 CONTINUE
C
      WRITE(GRPRSM,*)
     X  'Coefficients defining second dimension coordinates:'
      DO 120 JLOOP = 1, KSEC4(55)
        NTEMP(JLOOP) = KSEC4(59 + KSEC4(53) + JLOOP)
        WRITE(GRPRSM,'(F20.10)') PTEMP(JLOOP)
  120 CONTINUE
C
C     -----------------------------------------------------------------|
C     Section 9. Return to calling routine.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
