C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C
      INTEGER FUNCTION JGGLAT(KLAT, PGAUSS)
C
C---->
C**** *JGGLAT* - Routine to compute Gaussian abscissae
C
C     PURPOSE
C     _______
C
C     This routine computes Gaussian lines of latitude for a given
C     truncation.
C
C     INTERFACE
C     _________
C
C     IERR = JGGLAT(KLAT, PGAUSS)
C
C     Input parameters
C     ________________
C
C     KLAT       - This is the number of lines of latitude from pole to
C                  pole.
C
C     Output parameters
C     ________________
C
C     PGAUSS     - The Gaussian lines of latitude for truncation
C                  KLAT / 2.
C
C     Return value
C     ____________
C
C     The error indicator (INTEGER).
C
C     Error and Warning Return Values
C     _______________________________
C
C     1  The Newton iteration failed to converge when generating
C        Gaussian lines of latitude.
C
C     MARSINT problem - Please report this to Meteorological
C     Applications.
C
C     Common block usage
C     __________________
C
C     None
C
C     EXTERNALS
C     _________
C
C     JBESS    - This routine is used to generate initial
C                  approximations to the Gaussian latitudes.
C
C     METHOD
C     ______
C
C     JBESS is used to provide zeros of the Bessel function J0,
C     which are used as starting approximations to the Gaussian
C     latitudes. Newton iteration is used to generate the latitudes
C     from these approximations.
C
C     REFERENCE
C     _________
C
C     None
C
C     COMMENTS
C     ________
C
C
C     This routine is adapted from that in the old Marsint library.
C     The interface and the variable names have been modified.
C
C     Program contains sections 0 to 2 and 9
C
C     AUTHOR
C     ______
C
C     K. Fielding      *ECMWF*      Oct 1993
C
C     MODIFICATIONS
C     _____________
C
C     None
C
C----<
C     _______________________________________________________
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
         IMPLICIT NONE
C
#include "jparams.h"
#include "parim.h"
C
C     Dummy arguments
      INTEGER KLAT
C
      REAL PGAUSS (*)
C
C     Local variables
C
      INTEGER ITRUNC, IERR
C
C     Loop control variables
C
      INTEGER JLAT, JITER, JLN
C
      REAL ZRADDEG, ZCON, ZLAT, ZROOT
      REAL ZKM1, ZKM2, ZLN, ZFUNC, ZDERIV, ZMOVE
C
C     Function externals
C
      INTEGER JBESS
C
C     _______________________________________________________
C
C*    Section 1. Set constants and get initial approximation.
C     _______________________________________________________
C
  100 CONTINUE
C
      ZFUNC  = 0.
      JGGLAT = 0
C
      ZRADDEG = 180.0 / PPI
C
      ZCON = (PPONE - (PPTWO / PPI) ** 2) * PPQUART
C
      ZLAT = KLAT
      ITRUNC = KLAT / 2
C
      IERR = JBESS (ITRUNC, PGAUSS)
C
C     _______________________________________________________
C
C*    Section 2. Compute abscissae
C     _______________________________________________________
C
  200 CONTINUE
C
      DO 240 JLAT = 1, ITRUNC
C
C     First approximation for ZROOT
C
         ZROOT = COS (PGAUSS (JLAT) /
     1      SQRT ( (ZLAT + PPHALF) ** 2 + ZCON) )
C
C     Perfrom loop of Newton iterations
C
         DO 220 JITER =1, JPMAXITER
C
            ZKM2 = PPONE
            ZKM1 = ZROOT
C
C     Compute Legendre polynomial
C
            DO 210 JLN = 2, KLAT
C
               ZLN = JLN
C
               ZFUNC = ( (PPTWO * ZLN - PPONE) * ZROOT * ZKM1 -
     1            (ZLN - PPONE) * ZKM2) / ZLN
C
               ZKM2 = ZKM1
               ZKM1 = ZFUNC
C
  210       CONTINUE
C
C     Perform Newton iteration
C
C
            ZDERIV = (ZLAT * (ZKM2 - ZROOT * ZFUNC) ) /
     1         (PPONE - ZROOT ** 2)
C
            ZMOVE = ZFUNC / ZDERIV
C
            ZROOT = ZROOT - ZMOVE
C
C     If ABS (ZMOVE) < PPEPSA leave iteration loop because sufficient
C     accuracy has been achieved.
C
            IF (ABS (ZMOVE) .LE. PPEPSA) GOTO 230
C
  220    CONTINUE
C
C     Routine fails if no convergence after JPMAXITER iterations.
C
         JGGLAT = 1
         GO TO 900
C
  230    CONTINUE
C
C*    Set North and South values using symmetry.
C
         PGAUSS (JLAT) = ASIN (ZROOT) * ZRADDEG
         PGAUSS (KLAT + 1 - JLAT) = - PGAUSS (JLAT)
C
  240 CONTINUE
C
C
      IF (KLAT .NE. ITRUNC * 2) PGAUSS (ITRUNC + 1) = PPZERO
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine.
C     _______________________________________________________
C
  900 CONTINUE
C
C
      RETURN
      END

