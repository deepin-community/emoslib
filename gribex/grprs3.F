C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GRPRS3 (KSEC0,KSEC3,PSEC3)
C
C---->
C**** GRPRS3 - Print information from Section 3 of GRIB code.
C
C     Purpose.
C     --------
C
C     Print the information in the Bit-map section
C     Section (Section 3) of decoded GRIB data.
C
C**   Interface.
C     ----------
C
C     CALL GRPRS3 (KSEC0,KSEC3,PSEC3)
C
C     Input Parameters.
C     -----------------
C
C     KSEC0 - Array of decoded integers from Section 0.
C     KSEC3 - Array of decoded integers from Section 3.
C
C     PSEC3 - Array of decoded reals from Section 3.
C
C     Output Parameters.
C     ------------------
C
C     None.
C
C     Method.
C     -------
C
C     Fields printed as integers or reals.
C
C     Externals.
C     ----------
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
C     Routine contains Sections 0 to 1 and Section 9.
C
C     Author.
C     -------
C
C     J. Hennessy    ECMWF 11.09.91
C
C     Modifications.
C     --------------
C
C     J.D.Chambers   ECMWF 20.07.94
C     Change format for printing missing data indicator to
C     allow largest negative (32-bit) number
C
C     J.D.Chambers   ECMWF 14.06.95
C     Change format for printing missing data indicator to
C     allow for MAGICS value (-1.5E+21)
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
      INTEGER KSEC0(*)
      INTEGER KSEC3(*)
      REAL PSEC3(*)
C
C     -----------------------------------------------------------------|
C*    Section 1 . Print required information.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      WRITE(GRPRSM,9000)
      WRITE(GRPRSM,9001)
      WRITE(GRPRSM,9002)
C
      IF( KSEC3(1).NE.0 ) THEN
        WRITE(GRPRSM,9003) KSEC3(1)
      ELSE
        WRITE(GRPRSM,9004)
      ENDIF
      WRITE(GRPRSM,9005) KSEC3(2)
C
      WRITE(GRPRSM,*)
     X  'Missing data value for real data.        ', PSEC3(2)
C
C     -----------------------------------------------------------------|
C*    Section 9 . Format statements. Return to calling routine.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
 9000 FORMAT (1H )
 9001 FORMAT (1H ,'Section 3 - Bit-map Section.')
 9002 FORMAT (1H ,'-------------------------------------')
 9003 FORMAT (1H ,'Predetermined bit-map number.                ',I9)
 9004 FORMAT (1H ,'No predetermined bit-map.')
 9005 FORMAT (1H ,'Missing data value for integer data.    ',I14)
 9006 FORMAT (1H ,'Missing data value for real data.        ',F20.6)
C
      RETURN
C
      END
