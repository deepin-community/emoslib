C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GRPRS0(KSEC0)
C
C---->
C**** GRPRS0 - Print information from Section 0 of GRIB code.
C
C     Purpose.
C     --------
C
C     Print the information in the Indicator
C     Section (Section 0) of decoded GRIB data.
C
C**   Interface.
C     ----------
C
C     CALL GRPRS0(KSEC0)
C
C
C     Input Parameters.
C     -----------------
C
C     KSEC0 - Array of decoded parameters from Section 0.
C
C     Output Parameters.
C     ------------------
C
C     None.
C
C     Method.
C     -------
C
C     Fields are printed as integers.
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
C     Checks environment variable GRPRS_STREAM for output stream number
C     for printing (default is 6)
C
C     Author.
C     -------
C
C     J. Hennessy    ECMWF 18.06.91
C
C     Modifications.
C     --------------
C
C     J. Hennessy    ECMWF 30.08.91
C     Changes to some comments only.
C
C     J.D.Chambers   ECMWF    June 2002
C     Add option to set output stream number using environment variable
C     GRPRS_STREAM
C
C     -----------------------------------------------------------------|
C----<
C
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
      INTEGER KSEC0(*)
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
      WRITE(GRPRSM,9003) KSEC0(1)
      WRITE(GRPRSM,9004) KSEC0(2)
C
C     -----------------------------------------------------------------|
C*    Section 9 . Format statements. Return to calling routine.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
 9000 FORMAT(1H )
 9001 FORMAT(1H ,'Section 0 - Indicator Section.       ')
 9002 FORMAT(1H ,'-------------------------------------')
 9003 FORMAT(1H ,'Length of GRIB message (octets).     ',I9)
 9004 FORMAT(1H ,'GRIB Edition Number.                 ',I9)
C
      RETURN
C
      END
