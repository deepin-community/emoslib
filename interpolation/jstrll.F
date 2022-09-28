C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JSTRLL( PSHUP, KTRUNC, PNLAT, PBUILD, KLONO,
     X                    PLEG, PTRIGS, KMFAX, PZFA,   KRET)
C
C---->
C**** JSTRLL
C
C     PURPOSE
C     _______
C
C     This routine converts stretched spectral input fields to
C     standard lat/long grid fields.
C
C     INTERFACE
C     _________
C
C     CALL JSTRLL( PSHUP, KTRUNC, PNLAT, PBUILD, KLONO,
C    X              PLEG, PTRIGS, KMFAX, PZFA,   KRET)
C
C     Input parameters
C     ________________
C
C     PSHUP  - Spherical harmonics field, unpacked
C     KTRUNC - Truncation number of spherical harmonics field
C     PNLAT  - Start latitude (northernmost) for output field
C     PBUILD - Grid interval used to build the legendre coefficients
C     KLONO  - Number of longitude points along the line of latitude
C     PLEG   - Array used to hold legendre functions
C     PTRIGS - Initialized array of trig.functions (setup by JJSET99)
C     KMFAX  - Initialized array of prime factors (setup by JJSET99)
C
C     Output parameters
C     ________________
C
C     PZFA   - Output grid point field; contains one latitude row.
C     KRET   - Return status code, 0 = OK
C
C     Common block usage
C     __________________
C
C     JDCNDBG
C
C     Method
C     ______
C
C     None.
C
C     Externals
C     _________
C
C     FFT99   - Carries out FFT
C     INTLOG  - Output log message
C     INTLOGR - Output log message (with real value)
C     NMAKLL  - Make interpolation coefficients one latitude at a time
C
C     Reference
C     _________
C
C     E.C.M.W.F. Research Department technical memorandum no. 56
C                "The forecast and analysis post-processing package"
C                May 1982. J.Haseler.
C
C     Comments
C     ________
C
C     It handles transformation to a regular lat/long grid.
C
C     It is not for U and V fields (no correction is applied at the 
C     poles).
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers      ECMWF      July 1999
C
C     MODIFICATIONS
C     _____________
C
C     None.
C
C----<
C     _______________________________________________________
C
C
      IMPLICIT NONE
#include "jparams.h"
#include "parim.h"
#include "nifld.common"
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER ( JPROUTINE = 31300 )
      INTEGER JPN, JPS
      PARAMETER ( JPN = 1 )
      PARAMETER ( JPS = 2 )
C
C     Subroutine arguments
C
      COMPLEX PSHUP(*)
      INTEGER KTRUNC
      INTEGER KLONO, KRET
      INTEGER KMFAX(*)
      REAL PNLAT, PBUILD
      REAL PZFA(JPLONO+2), PLEG(*), PTRIGS(*)
C
C     Local variables
C
      INTEGER ILIM, IMLIM, ILN
      INTEGER ITAL, ITALA, ITALS, IMN, IMP
      INTEGER INORTH
      INTEGER JM, LOOP, JF
      INTEGER NERR
C
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IWORK
#endif
#endif
      REAL ZNLAT, MFACTOR
      REAL WORK
      DIMENSION WORK(1)
      POINTER ( IWORK, WORK )
      COMPLEX   ZDUM(JPTRNC + 1)
      COMPLEX   ZSUMS(JPTRNC + 1), ZSUMA(JPTRNC + 1)
      COMPLEX*16 CHOLD
C
      INTEGER ISIZE
      DATA ISIZE/0/
      SAVE ISIZE, IWORK
C
C     _______________________________________________________
C
C*    Section 1.    Initialization.
C     _______________________________________________________
C
  100 CONTINUE
C
C     First time through, dynamically allocate memory for workspace
C
      IF( ISIZE.EQ.0 ) THEN
        ISIZE =  2*JPFFT
        CALL JMEMHAN( 9, IWORK, ISIZE, 1, KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'JSTRLL: memory allocation error.',IWORK)
          KRET = JPROUTINE + 1
          GOTO 990
        ENDIF
      ENDIF
C
      IF( NDBG.GT.1 ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'JSTRLL: Spherical harmonic coeffs(first 20):',JPQUIET)
        DO NDBGLP = 1, 20
          CALL INTLOGR(JP_DEBUG,' ',PSHUP( NDBGLP ))
        ENDDO
        CALL INTLOG(JP_DEBUG,'JSTRLL: Input parameters:',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'JSTRLL: Spherical harmonic truncation = ', KTRUNC)
        CALL INTLOGR(JP_DEBUG,
     X    'JSTRLL: Current latitude = ', PNLAT)
        CALL INTLOGR(JP_DEBUG,
     X    'JSTRLL: Grid lat. interval(leg. file) = ', PBUILD)
        CALL INTLOG(JP_DEBUG,
     X    'JSTRLL: Number of long. pts per row = ', KLONO)
        CALL INTLOG(JP_DEBUG,
     X    'JSTRLL: Trig.functions (setup by JJSET99):',JPQUIET)
        DO NDBGLP = 1, 10
          CALL INTLOGR(JP_DEBUG,' ',PTRIGS( NDBGLP ))
        ENDDO
        CALL INTLOG(JP_DEBUG,
     X    'JSTRLL: Prime factors (setup by JJSET99):',JPQUIET)
        DO NDBGLP = 1, 10
          CALL INTLOG(JP_DEBUG,' ',KMFAX( NDBGLP ))
        ENDDO
      ENDIF
C
C     If it is a stretched field, calculate the true latitude and
C     the map factor.
C
      IF( RISTRET.NE.0 ) THEN
        CALL STRLAT(PNLAT, RISTRET, 1, ZNLAT, MFACTOR)
      ELSE
        ZNLAT = PNLAT
      ENDIF
C
      ILIM   = KTRUNC + 1
      IMLIM  = KTRUNC + 1
      INORTH = -1
      ILN    = KLONO + 2
C
C     _______________________________________________________
C
C*    Section 2.   Calculate fourier coefficients
C     _______________________________________________________
C
 200  CONTINUE
C
C     Generate the legendre coefficients 'on the fly'
C
      IF( ZNLAT.GE.0 ) THEN
        CALL NMAKLL( KTRUNC, PBUILD, ZNLAT, 1, PLEG, NERR)
      ELSE
        CALL NMAKLL( KTRUNC, PBUILD, -ZNLAT, 1, PLEG, NERR)
      ENDIF
C
C     Clear array.
C
      DO JF = 1, JPLONO+2
        PZFA(JF) = 0.0
      ENDDO
C
C     Fill slots which are used
C
      IMN = 0
      IMP = 0
C
      DO JM = 1, IMLIM
        ITAL = ILIM - JM + 1
        DO LOOP = 1, ITAL
          ZDUM(LOOP) = PLEG(IMP + LOOP)*PSHUP(IMN + LOOP)
        ENDDO
C
        IMP = IMP + ITAL + 1
        IMN = IMN + ITAL
        ITALS = (ITAL + 1)/2
        ITALA = ITAL/2
        CHOLD = (0.0D0, 0.0D0)
        DO LOOP = 1, 2*ITALS, 2
          CHOLD = CHOLD + ZDUM(LOOP)
        ENDDO
        ZSUMS(JM) = CHOLD
        CHOLD = (0.0D0, 0.0D0)
        DO LOOP = 2, 2*ITALA, 2
          CHOLD = CHOLD + ZDUM(LOOP)
        ENDDO
        ZSUMA(JM) = CHOLD
C
      ENDDO
C
C     For the southern hemisphere row, the legendre functions are
C     the complex conjugates of the corresponding northern row -
C     hence the juggling with the signs in the next loop.
C
C     Note that PZFA is REAL, but the coefficients being calculated
C     are COMPLEX.  There are pairs of values for each coefficient
C     (real and imaginary parts) and pairs of values for each
C     latitude (north and south).
C
      IF( ZNLAT.GE.0 ) THEN
        DO JM = 1, IMLIM
          PZFA(2*JM -1) = REAL(ZSUMS(JM))  + REAL(ZSUMA(JM))
          PZFA(2*JM   ) = AIMAG(ZSUMS(JM)) + AIMAG(ZSUMA(JM))
        ENDDO
      ELSE
        DO JM = 1, IMLIM
          PZFA(2*JM -1) = REAL(ZSUMS(JM))  - REAL(ZSUMA(JM))
          PZFA(2*JM   ) = AIMAG(ZSUMS(JM)) - AIMAG(ZSUMA(JM))
        ENDDO
      ENDIF
C
C     _______________________________________________________
C
C*    Section 3.    Fast fourier transform
C     _______________________________________________________
C
 300  CONTINUE
C
      CALL FFT99(PZFA,WORK,PTRIGS,KMFAX,1,JPLONO+2,KLONO,1,1)
C   
C     Apply map factor (squared) to vorticity or divergence
C   
      IF( (NIPARAM.EQ.138).OR.(NIPARAM.EQ.155) ) THEN
        MFACTOR = MFACTOR*MFACTOR
        DO LOOP = 1, KLONO
          PZFA(LOOP) = MFACTOR*PZFA(LOOP)
        ENDDO
      ENDIF
C
      IF( NDBG.GT.1 ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'JSTRLL: Values calculated by FFT:',JPQUIET)
        DO NDBGLP = 1, 20
          CALL INTLOGR(JP_DEBUG,' ', PZFA( 1 + (NDBGLP-1)*2 ))
          CALL INTLOGR(JP_DEBUG,' ', PZFA( NDBGLP*2 ))
        ENDDO
      ENDIF
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
C
 900  CONTINUE
C
      KRET = 0
C
 990  CONTINUE
      RETURN
      END
