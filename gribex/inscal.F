C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

       SUBROUTINE INSCAL (PDATA,KDATA,KLEN,PREF,PSCALE,NBPV)
C
C---->
C**** INSCAL - Vectorise calculation of increments.
C
C     Purpose.
C     --------
C
C     Vectorise calculation of increments.
C
C**   Interface.
C     ----------
C
C     CALL INSCAL (PDATA,KDATA,KLEN,PREF,PSCALE,NBPV)
C
C
C     Input Parameters.
C     -----------------
C
C     PDATA      - Array of floating point values.
C     KLEN       - Number of values to be converted.
C     PREF       - Reference value.
C     PSCALE     - Scale factor.
C     NBPV       - Number of bits per packed value
C
C     Output Parameters.
C     -----------------
C
C     KDATA      - Array of integer increments
C
C     Method.
C     -------
C
C     The reference value is subtracted from each value,
C     and the result is then divided by the scale factor.
C
C     Externals.
C     ----------
C
C     None.
C
C     Reference.
C     ----------
C
C     WMO Manual on Codes re GRIB representation.
C
C     Comments.
C     --------
C
C     PDATA and KDATA are really the same array. This routine
C     is just a device to force vectorisation on the Cray,
C     without the necessity of using another array.
C
C
C     Author.
C     -------
C
C     J. Hennessy     ECMWF     25.06.91
C
C     Modifications.
C     _____________
C
C     J.D.Chambers          ECMWF     21.11.94
C     Use JNINT on VAX
C
C     J.D.Chambers          ECMWF     21.11.94
C     Check number of bits per value is not exceeded
C
C     P.J.Towers            ECMWF     18.07.02
C     Optimised for IBM Power4.
C         -Merged loops
C         -Replaced Nint(x) by Int(x+0.5)
C         -Replaced division by scale by multiplication by reciprocal
C
C     R. El Khatib / D. Paradis METEO-FRANCE 06.02.08
C         - (No conversion if the value exceeds the number of bits for
C         integers 
C
C----<
C     -----------------------------------------------------------------
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------
C
#ifdef CRAY
      IMPLICIT NONE
#endif
#include "grbcom.h"
#include "common/grprs.h"
C
      INTEGER KDATA(KLEN), NBPV, LOOP, MAXNBPV
      INTEGER INTEMAX
C
      REAL PDATA(KLEN), PREF, PSCALE
      REAL ZRECIP
      REAL ZI
      LOGICAL LPDEBUG
C
C     -----------------------------------------------------------------
C*    Section 1 . Calculation of increments.
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
      LPDEBUG = ( NDBG.GE.1 )
      MAXNBPV = 2**NBPV
      ZRECIP = 1.0 / PSCALE
      IF( LPDEBUG ) THEN
        WRITE(GRPRSM,*) 'INSCAL: MAXNBPV = ', MAXNBPV
        WRITE(GRPRSM,*) 'INSCAL: PSCALE = ', PSCALE
        WRITE(GRPRSM,*) 'INSCAL: ZRECIP = ', ZRECIP
        WRITE(GRPRSM,*) 'INSCAL: PREF = ', PREF
        WRITE(GRPRSM,*) 'INSCAL: KLEN = ', KLEN
      ENDIF

#ifdef JBPW_64
      INTEMAX = 140737488355327
#else
      INTEMAX = 2147483647
#endif

      DO LOOP = 1,KLEN
#ifdef VAX
         KDATA(LOOP) = JNINT ( (PDATA(LOOP) - PREF) / PSCALE )
#else
         ZI =  (PDATA(LOOP) - PREF) * ZRECIP + 0.5
         IF (ABS(ZI) .GE. INTEMAX) THEN
            KDATA(LOOP) = MAXNBPV - 1
         ELSE
            KDATA(LOOP) =  INT ( ZI )
         ENDIF
#endif
         IF( KDATA(LOOP).GE.MAXNBPV ) KDATA(LOOP) = MAXNBPV - 1
         IF( KDATA(LOOP).LT.0 ) KDATA(LOOP) = 0
      ENDDO
C
C     -----------------------------------------------------------------
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END
