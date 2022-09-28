C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGGLAT(KLAT, PGAUSS, KPR, KERR)
C
C---->
C**** *IGGLAT*
C
C     PURPOSE
C     _______
C
C     Compute Gaussian lines of latitude for a given truncation.
C
C
C     INTERFACE
C     _________
C
C     IERR = IGGLAT(KLAT, PGAUSS, KPR, KERR)
C
C
C     Input parameters
C     ________________
C
C     KLAT - The number of lines of latitude from pole to pole.
C
C     KPR  - Debug print switch:
C            0, No debugging output.
C            1, Produce debugging output.
C
C     KERR - Error control flag.
C            -ve, No error message. Return error code.
C            0  , Hard failure with error message.
C            +ve, Print error message. Return error code.
C
C
C     Output parameters
C     ________________
C
C     PGAUSS - The Gaussian lines of latitude for truncation KLAT/2
C
C
C     Return value
C     ____________
C
C     0     - OK
C     24701 - The calculation of Gaussian lines of latitude failed.
C
C
C     Common block usage
C     __________________
C
C     None
C
C
C     EXTERNALS
C     _________
C
C     INTLOG(R) - Logs messages.
C     IGBESS    - This routine is used to generate initial
C                 approximations to the Gaussian latitudes.
C
C
C     METHOD
C     ______
C
C     IGBESS is used to provide zeros of the Bessel function J0,
C     which are used as starting approximations to the Gaussian
C     latitudes. Newton iteration is used to generate the latitudes
C     from these approximations.
C
C     Calculated latitudes are stored internally and reused if a
C     later call asks for the same calculation.
C
C
C     REFERENCE
C     _________
C
C     None
C
C
C     COMMENTS
C     ________
C
C     This routine is adapted from that in the old Marsint library.
C     The interface and the variable names have been modified.
C
C
C     AUTHOR
C     ______
C
C     K. Fielding      *ECMWF*      Oct 1993
C
C
C     MODIFICATIONS
C     _____________
C
C     J.Chambers        ECMWF       Aug 1998
C     Add reuse of calculated latitudes.
C
C----<
C     -------------------------------------------------------
C*    Section 0. Definition of variables.
C     -------------------------------------------------------
C
      IMPLICIT NONE
C
#include "jparams.h"
#include "parim.h"
C
C     Function arguments
C
      INTEGER KLAT, KPR, KERR
      REAL PGAUSS (*)
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 24700)
C     -------------------------------------------------------
C     The convergence criteria is machine dependent
C     -------------------------------------------------------
C
      REAL PACRCY
#ifdef REAL_8
      PARAMETER (PACRCY = 1.0E-14)
#else
      PARAMETER (PACRCY = 1.0E-7)
#endif
C
C     Local variables
C
      LOGICAL LDEBUG
      INTEGER ITRUNC, IERR
      INTEGER JLAT, JITER, JLN, LOOP, JOLDLAT
      REAL ZRADDEG, ZCON, ZLAT, ZROOT
      REAL ZKM1, ZKM2, ZLN, ZFUNC, ZDERIV, ZMOVE
      REAL ROLDGS
      DIMENSION ROLDGS(JPGTRUNC*2)
C
      SAVE JOLDLAT, ROLDGS
C
C     Externals
C
      INTEGER IGBESS
C
C     -------------------------------------------------------
C*    Section 1. Set constants and get initial approximation.
C     -------------------------------------------------------
C
  100 CONTINUE
C
      ZFUNC  = 0.
      IGGLAT = 0
      LDEBUG = ( KPR.GE.1 )
C
      IF( LDEBUG ) THEN
        CALL INTLOG(JP_DEBUG,'IGGLAT: Section 1.',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X  'IGGLAT: No.lines lat from pole to pole = ',KLAT)
      ENDIF
C
C     Using previously calculated values if truncation is the same.
C
      IF( KLAT.EQ.JOLDLAT ) THEN
        IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X    'IGGLAT: Using previously calculated values',JPQUIET)
        DO LOOP = 1, KLAT
          PGAUSS(LOOP) = ROLDGS(LOOP)
        ENDDO
        GOTO 900
      ENDIF
C
      JOLDLAT = KLAT
C
      ZRADDEG = 180.0/PPI
C
      ZCON   = (PPONE - (PPTWO / PPI) ** 2) * PPQUART
C
      ZLAT   = KLAT
      ITRUNC = KLAT/2
C
      IERR = IGBESS(ITRUNC, PGAUSS, KPR, KERR)
C
      IF( IERR.GT.0 ) THEN
         IGGLAT = IERR
         GOTO 900
      ENDIF
C
C     -------------------------------------------------------
C*    Section 2. Compute abscissae
C     -------------------------------------------------------
C
  200 CONTINUE
C
      IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'IGGLAT: Section 2.',JPQUIET)
C
      DO 240 JLAT = 1, ITRUNC
C
C       First approximation for ZROOT
C
        ZROOT = COS(PGAUSS(JLAT) / SQRT( (ZLAT+PPHALF)**2 + ZCON) )
C
C       Perform loop of Newton iterations
C
        DO 220 JITER = 1, JPMAXITER
C
          ZKM2 = PPONE
          ZKM1 = ZROOT
C
C         Compute Legendre polynomial
C
          DO 210 JLN = 2, KLAT
C
            ZLN = JLN
C
            ZFUNC = ( (PPTWO * ZLN - PPONE) * ZROOT * ZKM1 -
     1              (ZLN - PPONE) * ZKM2) / ZLN
C
            ZKM2 = ZKM1
            ZKM1 = ZFUNC
C
  210     CONTINUE
C
C         Perform Newton iteration
C
          ZDERIV = (ZLAT * (ZKM2 - ZROOT * ZFUNC) ) /
     1             (PPONE - ZROOT ** 2)
C
          ZMOVE = ZFUNC / ZDERIV
          ZROOT = ZROOT - ZMOVE
C
C         Leave iteration loop when sufficient accuracy achieved.
C
          IF( ABS(ZMOVE).LE.PACRCY ) GOTO 230
C
  220   CONTINUE
C
C       Routine fails if no convergence after JPMAXITER iterations.
C
        IGGLAT = JPROUTINE + 1
C
        IF( KERR.GE.0 ) CALL INTLOG(JP_ERROR,
     X    'IGGLAT: Calculation of Gaussian lats failed.',JPQUIET)
C
        IF( KERR.EQ.0 ) CALL INTLOG(JP_FATAL,
     X    'IGGLAT: interpolation failed.',IGGLAT)
C
        GOTO 900
C
  230   CONTINUE
C
C*      Set North and South values using symmetry.
C
        PGAUSS(JLAT) = ASIN(ZROOT) * ZRADDEG
        PGAUSS(KLAT + 1 - JLAT) = - PGAUSS(JLAT)
C
  240 CONTINUE
C
      IF( KLAT.NE.(ITRUNC*2) ) PGAUSS(ITRUNC + 1) = PPZERO
C
C     Store calculated values for re-use
C
      DO LOOP = 1, KLAT
        ROLDGS(LOOP) = PGAUSS(LOOP)
      ENDDO
C
C     -------------------------------------------------------
C*    Section 9. Return to calling routine.
C     -------------------------------------------------------
C
  900 CONTINUE
C
      IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'IGGLAT: Section 9.',JPQUIET)
C
      RETURN
      END
