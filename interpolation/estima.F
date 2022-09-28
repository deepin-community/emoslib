C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION ESTIMA()
C---->
C**** ESTIMA
C
C     Purpose
C     -------
C
C     Calculate size of scratch space required for output field of
C     interpolation.
C
C
C     Interface
C     ---------
C
C     ISIZE = ESTIMA()
C
C
C     Input
C     -----
C
C     Values in common blocks "nofld.common".
C
C
C     Output
C     ------
C
C     Size as number of values (words).
C     Returns 0 if output type not recognised.
C
C
C     Method
C     ------
C
C     Depends on output grid type.
C
C
C     Externals
C     ---------
C
C     INTLOG  - Log error message
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     Feb 1995
C
C----<
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
C
      INTEGER TMP
C
C     -----------------------------------------------------------------|
C     Section 1.  Initialise.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      ESTIMA = 0
      TMP = 0
C
C     -----------------------------------------------------------------|
C*    Section 2.   Spherical harmonic field
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IF( (NOREPR.EQ.JPSPHERE).OR.
     X    (NOREPR.EQ.JPSTRSH).OR.
     X    (NOREPR.EQ.JPSPHROT) ) THEN
cs        ESTIMA = (NORESO+1) * (NORESO+4)
        ESTIMA = (NORESO+1) * (NORESO+2)
        CALL INTLOG(JP_DEBUG,'ESTIMA: spherical harmonic = ',ESTIMA)
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 5.  gaussian field
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
      IF(NOGAUSS.GT.0) THEN
C       ESTIMA = NOGAUSS * NOGAUSS * 8
        ESTIMA = (2*NOGAUSS) * (4*NOGAUSS + 20)  ! account for RGG/octahedral
        CALL INTLOG(JP_DEBUG,'ESTIMA: Gaussian Number = ', NOGAUSS)
        CALL INTLOG(JP_DEBUG,'ESTIMA: any gaussian= ',ESTIMA)
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3.   Regular lat/long field
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      IF( (NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPREGROT) ) THEN
        ESTIMA = (JP360/NOGRID(1) + 1) * (JP180 / NOGRID(2) + 1)
        IF( ABS(NOAREA(4)-NOAREA(2)).GT.0 .AND.
     X      ABS(NOAREA(3)-NOAREA(1)).GT.0 ) THEN
C         EMOS-277: extra (+1)*2 for memory allocation (see issue)
C         ! 2* to consider also vector interpolations (such as vo/d > u/v)
          TMP = (ABS(NOAREA(4)-NOAREA(2))/NOGRID(1) + 1 + 1)
     X        * (ABS(NOAREA(3)-NOAREA(1))/NOGRID(2) + 1 + 1)
          TMP = 2*TMP
          IF (ESTIMA>TMP) ESTIMA = TMP
        ENDIF
        CALL INTLOG(JP_DEBUG,'ESTIMA: Regular lat/long = ',ESTIMA)
      ENDIF

      IF(NILOCAL.EQ.4) THEN
        IF(NOGRID(1).GT.0.AND.NOGRID(1).GT.0) THEN
          ESTIMA = (JP360/NOGRID(1) + 1) * (JP180 / NOGRID(2) + 1)*2
        ELSE
          ESTIMA = 36099382
        ENDIF
        CALL INTLOG(JP_DEBUG,'ESTIMA: OCEAN field = ',ESTIMA)
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9.   Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      CALL INTLOG(JP_DEBUG,
     X  'ESTIMA: number of points in output field = ',ESTIMA)
C
      RETURN
      END
