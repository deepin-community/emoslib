#ifdef IBM
@PROCESS HOT(NOVECTOR) NOSTRICT
#endif
C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JSYMLL( PSHUP, KTRUNC, PSTART, PBUILD, PINTVL, KLUNIT,
     X                   KLATO,KLONO,PLEG,PTRIGS,KMFAX,PZFA,KRET)
C
C---->
C**** JSYMLL
C
C     PURPOSE
C     _______
C
C     This routine converts spectral input fields to standard
C     lat/long grid fields.
C
C     INTERFACE
C     _________
C
C     CALL JSYMLL( PSHUP, KTRUNC, PSTART, PBUILD, PINTVL, KLUNIT,
C    X             KLATO,KLONO,PLEG,PTRIGS,KMFAX,PZFA,KRET)
C
C     Input parameters
C     ________________
C
C     PSHUP    - Spherical harmonics field, unpacked
C     KTRUNC   - Truncation number of spherical harmonics field
C     PSTART   - Start latitude (northernmost) for output field
C                (must be positive - see comments below)
C     PBUILD   - Grid interval used to build the legendre coefficients
C                file
C     PINTVL   - Grid interval in degrees
C     KLUNIT   - stream number of the legendre function file
C     KLATO    - Number of latitude points in output field
C     KLONO    - Number of longitude points in output field
C     PLEG     - Array used to hold legendre functions
C     PTRIGS   - Initialized array of trig.functions (setup by JJSET99)
C     KMFAX    - Initialized array of prime factors (setup by JJSET99)
C
C     Output parameters
C     ________________
C
C     PZFA    - Output grid point field; contains upto 32 each
C               of North and South latitude rows symmetrically.
C     KRET     - Return status code
C                0 = OK
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
C     JREADLL - Reads the legendre functions for a latitude
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
C     This is a redesign, based on SPECGP.F
C
C     It handles transformation to a regular lat/long grid.
C     The generated grid is symmetrical about the equator, so
C     PSTART must be positive.
C
C     It is not for U and V fields (no correction is applied at the
C     poles).
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers      *ECMWF*      Nov 1993
C
C     MODIFICATIONS
C     _____________
C
C     J.D.Chambers     ECMWF        Feb 1997
C     Allow for 64-bit pointers
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
      INTEGER JPROUTINE
      PARAMETER ( JPROUTINE = 31300 )
C
C     Subroutine arguments
      COMPLEX   PSHUP
      DIMENSION PSHUP(*)
      INTEGER   KTRUNC
      REAL      PSTART, PBUILD, PINTVL
      INTEGER   KLUNIT, KLATO, KLONO, KMFAX, KRET
      REAL PLEG, PTRIGS, PZFA
      DIMENSION PZFA(JPLONO+2, 64)
      DIMENSION KMFAX(*), PLEG(*), PTRIGS(*)
C
C     Local variables
      REAL ZLAT
      INTEGER ILIM, IMLIM, ILN
      INTEGER ITAL, ITALA, ITALS, IMN, IMP
      INTEGER INORTH, ISOUTH
      INTEGER JM, J, JNEXTLAT, JF
      INTEGER NERR
      INTEGER*8 JDCLOOP
      INTEGER*8 IOFF
      !Integer Itimer,timer1,timer280,timerFFT
      !Integer timer246,timer244,time
C
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IWORK
#endif
#endif
      REAL WORK
      DIMENSION WORK(1)
      POINTER ( IWORK, WORK )
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
      IOFF = 0
cs      timer1=Itimer(0)
C
C     First time through, dynamically allocate memory for workspace
C
      IF( ISIZE.EQ.0 ) THEN
        ISIZE =  2*JPFFT*64
        CALL JMEMHAN( 9, IWORK, ISIZE, 1, KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'JSYMLL: memory allocation error.',IWORK)
          KRET = JPROUTINE + 1
          GOTO 990
        ENDIF
      ENDIF
C
      IF ( NDBG .GT. 1) THEN
        CALL INTLOG(JP_DEBUG,
     X    'JSYMLL: Spherical harmonic coeffs(first 20):',JPQUIET)
        DO 101 NDBGLP = 1, 20
          CALL INTLOGR(JP_DEBUG,' ',PSHUP( NDBGLP ))
  101   CONTINUE
        CALL INTLOG(JP_DEBUG,'JSYMLL: Input parameters:',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'JSYMLL: Spherical harmonic truncation = ', KTRUNC)
        CALL INTLOGR(JP_DEBUG,
     X    'JSYMLL: Start latitude(northernmost) = ', PSTART)
        CALL INTLOGR(JP_DEBUG,
     X    'JSYMLL: Grid lat. interval(leg. file) = ', PBUILD)
        CALL INTLOGR(JP_DEBUG,
     X    'JSYMLL: Grid lat. interval(degrees) = ', PINTVL)
        CALL INTLOG(JP_DEBUG,
     X    'JSYMLL: Stream number of leg. file = ', KLUNIT)
        CALL INTLOG(JP_DEBUG,
     X    'JSYMLL: Number of lat. rows in output = ', KLATO)
        CALL INTLOG(JP_DEBUG,
     X    'JSYMLL: Number of long. pts per row = ', KLONO)
        CALL INTLOG(JP_DEBUG,
     X    'JSYMLL: Trig.functions (setup by JJSET99):',JPQUIET)
        DO 102 NDBGLP = 1, 10
          CALL INTLOGR(JP_DEBUG,' ',PTRIGS( NDBGLP ))
  102   CONTINUE
        CALL INTLOG(JP_DEBUG,
     X    'JSYMLL: Prime factors (setup by JJSET99):',JPQUIET)
        DO 103 NDBGLP = 1, 10
          CALL INTLOG(JP_DEBUG,' ',KMFAX( NDBGLP ))
  103   CONTINUE
      ENDIF
      ILIM   = KTRUNC + 1
      IMLIM  = KTRUNC + 1
      INORTH = -1
      ILN    = KLONO + 2
C
C     _______________________________________________________
C
C*    Section 2.    Main loop through latitude rows to
C*                  calculate fourier coefficients
C     _______________________________________________________
C
 200  CONTINUE
C     timer280=Itimer(0)
C
C     For each latitude, the north and corresponding south latitude row
C     are calculated at the same time from the same legendre functions.
C
      DO 280 JNEXTLAT = 1, KLATO
        ZLAT  = ( PSTART - (PINTVL * REAL(JNEXTLAT - 1)) )
C
        IF ( NDBG .GT. 1)
     X    CALL INTLOGR(JP_DEBUG,'JSYMLL: Next latitude = ', ZLAT)
C
C       If required, generate the coefficients 'on the fly'
C
        IF( LON_FLY ) THEN
          CALL NMAKLL( KTRUNC, PBUILD, ZLAT, 1, PLEG, NERR)
          IOFF = 0
        ELSE IF(LFILEIO ) THEN
          CALL JREADLL( KLUNIT, KTRUNC, PBUILD, ZLAT, PLEG, NERR)
          IF ( NERR .NE. 0 ) THEN
            CALL INTLOG(JP_ERROR,'JSYMLL: JREADLL error',NERR)
            KRET = JPROUTINE + 2
            GOTO 990
          ENDIF
        ELSE
          IOFF = NINT( (90.0 - ZLAT)/PBUILD )
          IOFF = IOFF *(KTRUNC+1)
          IOFF = IOFF *(KTRUNC+4)
          IOFF = IOFF/2
        ENDIF
C
C       Clear unused slots in array.
C       Note there are two slots in the array - one for north latitude
C       and one for the corresponding south latitude.
        INORTH = INORTH + 2
        ISOUTH = INORTH + 1
        DO 241 JF = 2*IMLIM + 1, ILN
          PZFA(JF, INORTH) = 0.0
          PZFA(JF, ISOUTH) = 0.0
 241    CONTINUE
C
C       Now fill slots which are used
        IMN = 0
        IMP = 0
C
C       timer244=Itimer(0)
        DO JM = 1, IMLIM
          ITAL = ILIM - JM + 1
          ITALS = (ITAL + 1)/2
          ITALA = ITAL/2
          CHOLD=(0.0D0, 0.0D0)
          DO J = 1, 2*ITALS, 2
            IF( LFILEIO ) THEN
              CHOLD=CHOLD+PLEG(IMP+J)*PSHUP(IMN+J)
            ELSE
              JDCLOOP = IMP + J + IOFF
              CHOLD=CHOLD+PLEG(JDCLOOP)*PSHUP(IMN+J)
            ENDIF
          ENDDO
          ZSUMS(JM)=CHOLD
          CHOLD = (0.0D0, 0.0D0)
          DO J = 2, 2*ITALA, 2
            IF( LFILEIO ) THEN
              CHOLD=CHOLD+PLEG(IMP+J)*PSHUP(IMN+J)
            ELSE
              JDCLOOP = IMP + J + IOFF
              CHOLD=CHOLD+PLEG(JDCLOOP)*PSHUP(IMN+J)
            ENDIF
          ENDDO
          ZSUMA(JM)=CHOLD
          IMP = IMP + ITAL + 1
          IMN = IMN + ITAL
        ENDDO
C       timer244=Itimer(timer244)
C
C       For the southern hemisphere row, the legendre functions are
C       the complex conjugates of the corresponding northern row -
C       hence the juggling with the signs in the next loop.
C
C       Note that PZFA is REAL, but the coefficients being calculated
C       are COMPLEX.  There are pairs of values for each coefficient
C       (real and imaginary parts) and pairs of values for each
C       latitude (north and south).
C
C       timer246=Itimer(0)
        DO 246 JM = 1, IMLIM
          PZFA(2*JM -1, INORTH) = REAL(ZSUMS(JM))  + REAL(ZSUMA(JM))
          PZFA(2*JM   , INORTH) = AIMAG(ZSUMS(JM)) + AIMAG(ZSUMA(JM))
          PZFA(2*JM -1, ISOUTH) = REAL(ZSUMS(JM))  - REAL(ZSUMA(JM))
          PZFA(2*JM   , ISOUTH) = AIMAG(ZSUMS(JM)) - AIMAG(ZSUMA(JM))
 246    CONTINUE
C       timer246=Itimer(timer246)
C
C*    End of main loop through latitude rows.
C
 280  CONTINUE
C     timer280=Itimer(timer280)
C     print*,'JSYMLL: legendre functions klat=',KLATO,' time:',timer280,
C    X    ' seconds loop 246:',timer246,' loop 244:',timer244
C
C     _______________________________________________________
C
C*    Section 3.    Fast fourier transform
C     _______________________________________________________
C
 300  CONTINUE
C     timerFFT=Itimer(0)
C
      IF ( NDBG .GT. 1) CALL INTLOG(JP_DEBUG,
     X  'JSYMLL: FFT, no.of rows (N and S) = ', ISOUTH)
C
      CALL FFT99(PZFA,WORK,PTRIGS,KMFAX,1,JPLONO+2,KLONO,ISOUTH,1)
C
      IF ( NDBG .GT. 1) THEN
        CALL INTLOG(JP_DEBUG,
     X    'JSYMLL: Values calculated by FFT:',JPQUIET)
        DO 301 NDBGLP = 1, 20
        CALL INTLOGR(JP_DEBUG,' ', PZFA( 1, NDBGLP ))
        CALL INTLOGR(JP_DEBUG,' ', PZFA( 2, NDBGLP ))
  301   CONTINUE
      ENDIF
C     timerFFT=Itimer(timerFFT)
C     timer1=Itimer(timer1)
C     print*,'JSYMLL: legendre functions Total:',timer1,' sec first:',
C    X    timerMEM,' FFT99: ',timerFFT
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
