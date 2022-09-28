C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION GSCALE( PDATA, KTRUNC, KSTART, IPOWER, HOPT)
C
C---->
C**** GSCALE
C
C     Purpose.
C     --------
C
C     Pre- or post-scales spherical harmonic coefficents in GRIB scaling
C     and packing.
C
C
C**   Interface.
C     ----------
C
C     IRET = GSCALE(PDATA, KTRUNC, KSTART, IPOWER, HOPT)
C
C
C     Input Parameters.
C     -----------------
C
C     PDATA      - Array of floating point values.
C     KTRUNC     - Truncation of the spherical harmonic coefficients.
C     KSTART     - Number indicating truncation after which to start.
C     IPOWER     - Power to use when scaling coefficients(*1000).
C     HOPT       - Selects pre- or post scaling
C                  = 'I' for pre-scaling
C                  = 'O' for post-scaling
C
C
C     Output Parameters.
C     -----------------
C
C     PDATA      - Array with values changed.
C
C
C     Method.
C     -------
C
C                            <--    n    -->             (n = 0 to m)
C
C                            <KSTART>|
C
C     Given numbers:         cccccccc|cccccc    ^
C                              cccccc|cccccc    |
C                                cccc|cccccc    |
C                                  cc|cccccc    m        (m = 0 to KTRUNC)
C                                    |cccccc    |
C                                       cccc    |
C                                         cc    v
C
C
C      Operates on values :          |cccccc
C                                    |cccccc
C                                    |cccccc
C                                    |cccccc
C                                    |cccccc
C                                       cccc
C                                         cc
C
C
C
C     A scale factor is applied to the values.  The factor is
C     
C     n(n+1)^^p            for pre-scaling
C     1 / n(n+1)^^p        for post-scaling
C
C     where p = IPOWER/1000.
C
C
C     Externals.
C     ----------
C
C     None
C
C
C     Reference.
C     ----------
C
C     None.
C
C
C     Comments.
C     --------
C
C     None.
C
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      20:04:94
C
C
C     Modifications.
C     _____________
C
C     J.D.Chambers     ECMWF        Feb 1997
C     Allow for 64-bit pointers
C
C     J. Clochard, Meteo France, for ECMWF - January 1998.
C     Call to FLOAT replaced by REAL.
C
C     J.D.Chambers     ECMWF        July 2000
C     Run loops calculating power factors in SCALAR on VPPs to
C     eliminate differences due to vetorisation.
C
C     S.Curic     ECMWF        April 2005
C     Change value of JPMXTRC in order to handle truncation T2047
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------|
C
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Parameters
C
      INTEGER JPBYTES, JPMXTRC
#ifdef REAL_8
      PARAMETER (JPBYTES = 8)
#else
      PARAMETER (JPBYTES = 4)
#endif
      PARAMETER (JPMXTRC = 2048)
C
C     Subroutine arguments
C
      INTEGER KTRUNC, KSTART, IPOWER
      REAL PDATA
      DIMENSION PDATA(*)
      CHARACTER*1 HOPT
C
C     Local variables.
C
      INTEGER N, M, INDEX
      REAL FACTOR, POWER
      DIMENSION FACTOR(JPMXTRC)
C
C     Externals
C
C     -----------------------------------------------------------------|
C*    Section 1 . Check for valid truncation and start values.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      GSCALE = 0
      IF( (IPOWER.LT.-10000).OR.(IPOWER.GT.10000) ) THEN
        WRITE(GRPRSM,*) ' GSCALE - Invalid power given = ', IPOWER
        GSCALE = 16600
        GOTO 900
      ENDIF
C
      IF( (KTRUNC.LT.0).OR.(KTRUNC.GT.JPMXTRC) ) THEN
        WRITE(GRPRSM,*) ' GSCALE - cannot handle truncation = ', KTRUNC
        GSCALE = 16601
        GOTO 900
      ENDIF
C
      IF( (HOPT.NE.'I').AND.(HOPT.NE.'O') ) THEN
        WRITE(GRPRSM,*) ' GSCALE - Invalid option given = ', HOPT
        GSCALE = 16602
        GOTO 900
      ENDIF
C
      IF( KSTART.GT.KTRUNC ) THEN
        WRITE(GRPRSM,*) ' GSCALE - Invalid start/truncation given'
        WRITE(GRPRSM,*) ' GSCALE - KSTART, KTRUNC =', KSTART, KTRUNC
        GSCALE = 16603
        GOTO 900
      ENDIF
C
C     Setup scaling factors = n(n+1)^^p, for n = 1 to KTRUNC
C
      IF( IPOWER.EQ.0 ) GOTO 900
      POWER = REAL(IPOWER) / 1000.0
      FACTOR(1) = 1.0
C
C     'I' option -> multiply by the factors
C     'O' option -> divide by the factors
C
      IF( HOPT.EQ.'I' ) THEN
#ifdef FUJITSU
!OCL SCALAR
#endif
        DO N = 1 , KTRUNC
          IF( IPOWER.NE.1000 ) THEN
            FACTOR(N+1) = REAL(N * (N+1) )**POWER
          ELSE
            FACTOR(N+1) = REAL(N * (N + 1))
          ENDIF
        ENDDO
      ELSE
#ifdef FUJITSU
!OCL SCALAR
#endif
        DO N = 1 , KTRUNC
          IF( IPOWER.NE.1000 ) THEN
            FACTOR(N+1) = 1.0 / REAL(N * (N+1) )**POWER
          ELSE
            FACTOR(N+1) = 1.0 / REAL(N * (N + 1))
          ENDIF
        ENDDO
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2 . Scale the values.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Handle values 2 at a time (real and imaginary parts)
C
      INDEX = -1
      DO M = 0 , KSTART-1
        DO N = M , KTRUNC
          INDEX = INDEX + 2
          IF( N.GE.KSTART ) THEN
            PDATA(INDEX)   = PDATA(INDEX)   * FACTOR(N+1)
            PDATA(INDEX+1) = PDATA(INDEX+1) * FACTOR(N+1)
          ENDIF   
        ENDDO
      ENDDO
C
      DO M = KSTART , KTRUNC
        DO N = M , KTRUNC
          INDEX = INDEX + 2
          PDATA(INDEX)   = PDATA(INDEX)   * FACTOR(N+1)
          PDATA(INDEX+1) = PDATA(INDEX+1) * FACTOR(N+1)
        ENDDO
      ENDDO
C
C     -----------------------------------------------------------------|
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
