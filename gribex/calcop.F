C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION CALCOP( PSPEC, KTRUNC, KSUBSP)
C
C---->
C**** *CALCOP*  - linear fit of spectrum to (n*(n+1))**P
C
C     Purpose.
C     --------
C 
C     GRIB allows for "complex packing" of spectral coeficients.
C     This means that a sub-spectrum is not packed and, for the rest,
C     a multiplying factor is applied before packing.  The factor is
C     (n*(n+1))**p, where n is the total wave-number.
C
C     This routine returns an optimal P based on the field itself.
C     
C
C**   Interface.
C     ----------
C     
C     IP = CALCOP( PSPEC, KTRUNC, KSUBSP)
C
C
C     Input arguments.
C     ----------------
C
C     PSPEC   - spectral field
C     KTRUNC  - spectral truncation of field
C     KSUBSP  - truncation of "non-packed" sub-spectrum
C
C
C     Output arguments.
C     -----------------
C
C     The function return value is the integer value of P*1000.
C     The value is constrained to be in the range (-10000, +10000).
C                                
C
C     Method.
C     -------
C     
C     Consider F(n,m) = (n*(n+1))**(-P) * G(n,m), where F(n,m) is the
C     original spectral field and n the total wavenumber.  The aim
C     is to minimize G in a 1 norm with respect to P.  This can only
C     partially be achieved.
C
C     What we do is to to compute H(n), where
C         H(n) = max(F(n,m))    with respect to m.
C
C     We then perform a least square fit for the equation
C         log(H(n)) = beta0+beta1*log(n*(n+1)).
C
C     To ensure a better fit for the lower end of the spectrum, we
C     apply an (arbitrary) weighting function before fitting
C          W(n) = 1.0 / (n-KSUBSP+1)
C
C
C     Externals.
C     ----------
C
C     None.
C
C
C     Reference.
C     ----------
C     
C     Seber, G.A.F. (1979). Linear Regression Analyses.
C     John Wiley and Sons
C
C     ECMWF Research Department documentation of the IFS
C
C
C     Author.
C     -------
C
C     Mats Hamrud     ECMWF      5th May 1994
C
C
C     Modifications.
C     --------------
C
C     John Chambers   ECMWF      6th Sept 1994
C     Add original field truncation as a parameter.
C     Use GRIB ordering of spherical harmonic coefficients in input.
C
C     John Chambers   ECMWF      21st Nov 1994
C     Use double precision intrinsics on VAX
C
C     Lubia Vinhas    ECMWF      9th Oct 1996
C     Intrinsic function MAX in ALPHA platform needs
C
C----<
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Parameters
      INTEGER JPTMAX
      PARAMETER ( JPTMAX = 2047 )
C                  
C     JPTMAX is current maximum truncation value for input field
C
C     Subroutine arguments
      REAL PSPEC
      DIMENSION PSPEC(*)
      INTEGER KTRUNC, KSUBSP
C
C     Local arguments
      INTEGER ISMIN, ISMAX, INDEX, JN, JM
#ifdef CRAY
      REAL ZEPS, ZZ, ZXMW, ZYMW, ZWSUM, ZX, ZY, ZSUM1, ZSUM2
      REAL ZP
      REAL ZBETA1
      REAL ZW(0:(JPTMAX+1)), ZNORM(0:(JPTMAX+1))
#else
      DOUBLE PRECISION ZEPS, ZZ, ZXMW, ZYMW, ZWSUM, ZX, ZY, ZSUM1, ZSUM2
      DOUBLE PRECISION ZP
      DOUBLE PRECISION ZBETA1
      DOUBLE PRECISION ZW(0:(JPTMAX+1)), ZNORM(0:(JPTMAX+1))
#endif
C      
C     ------------------------------------------------------------------
C*    Section 1. Do initial setup and calulate the (arbitrary) weights.
C     ------------------------------------------------------------------
C
 100  CONTINUE
C
C     Check internal arrays can handle the truncation
C
      IF( KTRUNC.GT.JPTMAX ) THEN
        WRITE(GRPRSM,*)
     X    'CALCOP: Cannot use the given truncation ', KTRUNC
        WRITE(GRPRSM,*) 'CALCOP: Maximum handled is ', JPTMAX
        CALCOP = -99999
        RETURN
      ENDIF
C
      ISMIN = KSUBSP+1
      ISMAX = KTRUNC+1
      ZEPS  = 1.0E-15
C
C     Set up the weights.
#ifdef IBM
      ZZ = DBLE(ISMAX-ISMIN+1)
#else
      ZZ = REAL(ISMAX-ISMIN+1)
#endif
      DO 110 JN = ISMIN, ISMAX      
#ifdef IBM
        ZW(JN) = ZZ / DBLE(JN-ISMIN+1)
#else
        ZW(JN) = ZZ / REAL(JN-ISMIN+1)
#endif
 110  CONTINUE
C      
C     ------------------------------------------------------------------
C*    Section 2. Compute norms.
C     ------------------------------------------------------------------
C
 200  CONTINUE
C
C     Initialise array to zero.
      DO 210 JN = ISMIN, ISMAX
        ZNORM(JN) = 0.0
 210  CONTINUE
C
C     Form norms.
C     Handle values 2 at a time (real and imaginary parts)
C
C     First the rows affected by the subset
      INDEX = -1
      DO 230 JM = 0 , KSUBSP-1
        DO 220 JN = JM , KTRUNC
          INDEX = INDEX + 2
          IF ( JN .GE. KSUBSP ) THEN
#if defined (VAX) || defined (IBM) || defined (__alpha) || defined hpR64 || defined hpiaR64
            ZNORM(JN) = MAX(ZNORM(JN), DBLE(ABS(PSPEC(INDEX))))
            ZNORM(JN) = MAX(ZNORM(JN), DBLE(ABS(PSPEC(INDEX+1))))
#else
            ZNORM(JN) = MAX(ZNORM(JN), ABS(PSPEC(INDEX)))
            ZNORM(JN) = MAX(ZNORM(JN), ABS(PSPEC(INDEX+1)))
#endif
          ENDIF
  220   CONTINUE
  230 CONTINUE
C
C     Next the rows not affected by the subset
      DO 250 JM = KSUBSP , KTRUNC
        DO 240 JN = JM , KTRUNC
          INDEX = INDEX + 2
#if defined (VAX) || defined (IBM) || defined (__alpha) || defined hpR64 || defined hpiaR64
          ZNORM(JN) = DMAX1(ZNORM(JN), DBLE(ABS(PSPEC(INDEX))))
          ZNORM(JN) = DMAX1(ZNORM(JN), DBLE(ABS(PSPEC(INDEX+1))))
#else
          ZNORM(JN) = MAX(ZNORM(JN), ABS(PSPEC(INDEX)))
          ZNORM(JN) = MAX(ZNORM(JN), ABS(PSPEC(INDEX+1)))
#endif
  240   CONTINUE
  250 CONTINUE
C
C     Ensure the norms have a value which is not too small in case of
C     problems with math functions (e.g. LOG).
C
      DO 290 JN = ISMIN, ISMAX
#if defined (VAX) || defined (IBM) || defined hpR64 || defined hpiaR64
        ZNORM(JN) = DMAX1(ZNORM(JN), ZEPS)
#else
        ZNORM(JN) = MAX(ZNORM(JN), ZEPS)
#endif
        IF(ZNORM(JN).EQ.ZEPS) ZW(JN) = 100.*ZEPS
 290  CONTINUE     
C      
C     ------------------------------------------------------------------
C*    Section 3. Do linear fit to find the slope (ZBETA1).
C     ------------------------------------------------------------------
C
 300  CONTINUE
C
      ZXMW  = 0.0
      ZYMW  = 0.0
      ZWSUM = 0.0
C
C     Sum the weighted X and Ys.
      DO 310 JN = ISMIN, ISMAX
#ifdef IBM
        ZX    = LOG(DBLE(JN*(JN+1)))
#else
        ZX    = LOG(REAL(JN*(JN+1)))
#endif
        ZY    = LOG(ZNORM(JN))
        ZXMW  = ZXMW+ZX*ZW(JN)
        ZYMW  = ZYMW+ZY*ZW(JN)
        ZWSUM = ZWSUM+ZW(JN)
 310  CONTINUE
C
C     Form mean weighted X and Y.
      ZXMW  = ZXMW / ZWSUM
      ZYMW  = ZYMW / ZWSUM
      ZSUM1 = 0.0
      ZSUM2 = 0.0
C
C     Perform a least square fit for the equation
      DO 320 JN = ISMIN, ISMAX
#ifdef IBM
        ZX    = LOG(DBLE(JN*(JN+1)))
#else
        ZX    = LOG(REAL(JN*(JN+1)))
#endif
        ZY    = LOG(ZNORM(JN))
        ZSUM1 = ZSUM1+ZW(JN)*(ZY-ZYMW)*(ZX-ZXMW)
        ZSUM2 = ZSUM2+ZW(JN)*(ZX-ZXMW)**2
 320  CONTINUE
C 
C     Calculate the slope.
      ZBETA1 = ZSUM1 / ZSUM2     
C      
C     ------------------------------------------------------------------
C*    Section 9. Closedown.
C     ------------------------------------------------------------------
C
 900  CONTINUE
C
      ZP = -ZBETA1
#ifdef CRAY
      ZP = MAX(-9.999, MIN(9.999, ZP))
#endif
#if defined (VAX) || defined (IBM) || defined hpR64 || defined hpiaR64
      ZP = DMAX1(-9.999D0, DMIN1(9.999D0, ZP))
#endif
#if !defined(VAX) && !defined(CRAY) && !defined(IBM)
      ZP = MAX(-9.999D0, MIN(9.999D0, ZP))
#endif
C
#ifdef IBM
      CALCOP = IDINT( ZP * 1000.0 )
#else
      CALCOP = INT( ZP * 1000.0 )
#endif
C
      RETURN
      END
