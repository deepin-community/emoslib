C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JWINDLL( PSHUP, KTRUNC, PSTART, PBUILD, PINTVL, KLUNIT,
     X                    KLATO,KLONO,PLEG,PTRIGS,KMFAX,PZFA,KRET)
C
C---->
C**** JWINDLL
C
C     Purpose
C     -------
C     This routine converts spectral input fields to standard
C     lat/long grid fields for wind components U and V.
C
C
C     Interface
C     ---------
C     CALL JWINDLL( PSHUP, KTRUNC, PSTART, PBUILD, PINTVL, KLUNIT,
C    X              KLATO,KLONO,PLEG,PTRIGS,KMFAX,PZFA,KRET)
C
C
C     Input parameters
C     ----------------
C     PSHUP  - Spherical harmonics field, unpacked
C     KTRUNC - Truncation number of spherical harmonics field
C     PSTART - Start latitude (northernmost) for output field
C              (must be positive - see comments below)
C     PBUILD - Grid interval used to build the legendre coefficients file
C     PINTVL - Grid latitude interval in degrees
C     KLUNIT - stream number of the legendre function file
C     KLATO  - Number of latitude points in output field
C     KLONO  - Number of longitude points in output field
C     PLEG   - Array used to hold legendre functions
C     PTRIGS - Initialized array of trig.functions (setup by JJSET99)
C     KMFAX  - Initialized array of prime factors (setup by JJSET99)
C
C
C     Output parameters
C     -----------------
C     PZFA - Output grid point field; contains upto 32 each of
C            North and South latitude rows symmetrically.
C     KRET - Return status code
C            0 = OK
C
C
C     Common block usage
C     ------------------
C     JDCNDBG
C
C
C     Method
C     ------
C     None.
C
C
C     Externals
C     ---------
C     JREADLL - Reads the legendre functions for a latitude
C     JSPPOLE - Applies correction at North or South pole
C     FFT99   - Carries out FFT
C     INTLOG  - Output log message
C     INTLOGR - Output log message (with real value)
C     GETENV  - Pick up contents of an environment variable
C     JUVPOLE - Use gaussian grid to calculate U and V at the poles
C
C
C     Reference
C     ---------
C     E.C.M.W.F. Research Department technical memorandum no. 56
C                "The forecast and analysis post-processing package"
C                May 1982. J.Haseler.
C
C
C     Comments
C     --------
C     This is a redesign, based on SPECGP.F
C
C     It handles transformation to a regular lat/long grid.
C     The generated grid is symmetrical about the equator, so
C     PSTART must be positive.
C
C     It is only for U and V fields (correction is applied at the
C     poles and a scale factor is applied according to latitude).
C
C
C     Author
C     ------
C     J.D.Chambers      *ECMWF*      Nov 1993
C
C
C     Modifications
C     -------------
C     J.D.Chambers     ECMWF        Feb 1997
C     Allow for 64-bit pointers
C
C     J.D.Chambers     ECMWF        October 2002
C     Add handling for U/V spectral (as opposed to Ucos(theta)/Vcos(theta))
C
C
C----<
C     -----------------------------------------------------------------|
C
C
      IMPLICIT NONE
#include "jparams.h"
#include "parim.h"
#include "nifld.common"
C
C     Subroutine arguments
C
      COMPLEX   PSHUP
      DIMENSION PSHUP(*)
      INTEGER   KTRUNC
      REAL      PSTART, PBUILD, PINTVL
      INTEGER   KLUNIT, KLATO, KLONO, KMFAX, KRET
      REAL PLEG, PTRIGS, PZFA
      DIMENSION PZFA(JPLONO + 2, 64)
      DIMENSION KMFAX(*), PLEG(*), PTRIGS(*)
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER ( JPROUTINE = 31400 )
C
C     Local variables
C
      REAL ZPIBY2, ZDEGR, ZLAT
      INTEGER ILIM, IMLIM, ILN
      INTEGER ITAL, ITALA, ITALS, IMN, IMP
      INTEGER INORTH, ISOUTH
      INTEGER JM, J245, JNEXTLAT
      INTEGER NERR
      INTEGER*8 IOFF
      INTEGER*8 JDCLOOP
      REAL ZF
      REAL ZCOSI
C
      DIMENSION ZF(2*JPFFT)
C
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IWORK
#endif
#endif
      REAL WORK
      DIMENSION WORK(1)
      POINTER ( IWORK, WORK )
      COMPLEX   ZDUM(JPTRNC + 1)
      COMPLEX   ZSUMS(JPTRNC + 1), ZSUMA(JPTRNC + 1)
      COMPLEX*16 CHOLD
      INTEGER LOOP
C
      LOGICAL LXFIRST, LPLAINU
      INTEGER ISIZE, IBLANK
      CHARACTER*10 PLAINUV
C
      DATA LXFIRST/.TRUE./, LPLAINU/.FALSE./
      DATA ISIZE/0/
      SAVE ISIZE, IWORK, LXFIRST, LPLAINU
C
C     Statement function
C
      LOGICAL GPOLE
      REAL ANGLE
      GPOLE(ANGLE) = ( ABS(90.0 - ABS(ANGLE) ) .LT. 1.0E-3 )
C                  = .TRUE. if LAT is 90.0 or -90.0
C
C     -----------------------------------------------------------------|
C*    Section 1.    Initialization.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
      IOFF = 0
C
C     Check environment variable to see if wind components are plain
C     U and V, or U*cos(theta) and V*cos(theta) (the default).
C
      IF( LXFIRST ) THEN
C
        LXFIRST = .FALSE.
        CALL GETENV('INTERP_PLAIN_UV', PLAINUV)
        IBLANK = INDEX(PLAINUV, ' ')
C
        IF( (IBLANK.EQ.3).AND.(PLAINUV(1:2).EQ.'ON') )  LPLAINU =.TRUE.
        IF( (IBLANK.EQ.3).AND.(PLAINUV(1:2).EQ.'on') )  LPLAINU =.TRUE.
        IF( (IBLANK.EQ.4).AND.(PLAINUV(1:3).EQ.'YES') ) LPLAINU =.TRUE.
        IF( (IBLANK.EQ.4).AND.(PLAINUV(1:3).EQ.'yes') ) LPLAINU =.TRUE.
C
        IF( (IBLANK.EQ.3).AND.(PLAINUV(1:2).EQ.'NO') )  LPLAINU =.FALSE.
        IF( (IBLANK.EQ.3).AND.(PLAINUV(1:2).EQ.'no') )  LPLAINU =.FALSE.
        IF( (IBLANK.EQ.4).AND.(PLAINUV(1:3).EQ.'OFF') ) LPLAINU =.FALSE.
        IF( (IBLANK.EQ.4).AND.(PLAINUV(1:3).EQ.'off') ) LPLAINU =.FALSE.
C
C       First time through, dynamically allocate memory for workspace
C
        ISIZE =  2*JPFFT*64
        CALL JMEMHAN( 9, IWORK, ISIZE, 1, KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'JWINDLL: memory allocation error',IWORK)
          KRET = JPROUTINE + 1
          GOTO 990
        ENDIF
      ENDIF
C
      IF( NDBG.GT.1 ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'JWINDLL: Spherical harmonic coeffs(first 20):',JPQUIET)
        DO NDBGLP = 1, 20
          CALL INTLOGR(JP_DEBUG,' ',PSHUP( NDBGLP ))
        ENDDO
        CALL INTLOG(JP_DEBUG,
     X    'JWINDLL: Input parameters:',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'JWINDLL: Spherical harmonic truncation = ', KTRUNC)
        CALL INTLOGR(JP_DEBUG,
     X    'JWINDLL: Start latitude(northernmost) = ', PSTART)
        CALL INTLOGR(JP_DEBUG,
     X    'JWINDLL: Grid lat. interval(leg. file) = ', PBUILD)
        CALL INTLOGR(JP_DEBUG,
     X    'JWINDLL: Grid lat. interval(degrees) = ', PINTVL)
        CALL INTLOG(JP_DEBUG,
     X    'JWINDLL: Stream number of leg. file = ', KLUNIT)
        CALL INTLOG(JP_DEBUG,
     X    'JWINDLL: Number of lat. rows in output = ', KLATO)
        CALL INTLOG(JP_DEBUG,
     X    'JWINDLL: Number of long. pts per row = ', KLONO)
        CALL INTLOG(JP_DEBUG,
     X    'JWINDLL: Trig.functions (setup by JJSET99):',JPQUIET)
        DO NDBGLP = 1, 10
          CALL INTLOGR(JP_DEBUG,' ',PTRIGS( NDBGLP ))
        ENDDO
        CALL INTLOG(JP_DEBUG,
     X    'JWINDLL: Prime factors (setup by JJSET99):',JPQUIET)
        DO NDBGLP = 1, 10
          CALL INTLOG(JP_DEBUG,' ',KMFAX( NDBGLP ))
        ENDDO
      ENDIF
C
      ZPIBY2 = PPI / 2.0
      ZDEGR  = PPI / 180.0
      ILIM   = KTRUNC + 1
      IMLIM  = KTRUNC + 1
      INORTH = -1
      ILN    = KLONO + 2
C
C     -----------------------------------------------------------------|
C*    Section 2.    Main loop through latitude rows to
C*                  calculate fourier coefficients
C     -----------------------------------------------------------------|
C
 200  CONTINUE
C
C     For each latitude, the north and corresponding south latitude row
C     are calculated at the same time from the same legendre functions.
C
      DO 280 JNEXTLAT = 1, KLATO
        ZLAT  = ( PSTART - (PINTVL * REAL(JNEXTLAT - 1)) )
C
        IF( NDBG.GT.1 )
     X    CALL INTLOGR(JP_DEBUG, 'JWINDLL: Next latitude = ', ZLAT)
C
C       If required, generate the coefficients 'on the fly'
C
        IF( LON_FLY ) THEN
          CALL NMAKLL( KTRUNC, PBUILD, ZLAT, 1, PLEG, NERR)
          IOFF = 0
        ELSE IF( LFILEIO ) THEN
          CALL JREADLL( KLUNIT, KTRUNC, PBUILD, ZLAT, PLEG, NERR)
          IF ( NERR .NE. 0 ) THEN
            CALL INTLOG(JP_ERROR,'JWINDLL: JREADLL error',NERR)
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
C     Clear unused slots in array.
C     Note there are two slots in the array - one for north latitude
C     and one for the corresponding south latitude.
        INORTH = INORTH + 2
        ISOUTH = INORTH + 1
        DO LOOP = 1, ILN
          ZF( LOOP) = 0.0
          PZFA( LOOP, INORTH) = 0.0
          PZFA( LOOP, ISOUTH) = 0.0
        ENDDO
C
C       For spectral Ucos(theta), Vcos(theta), treat the poles as a
C       special case.
C
        IF( GPOLE(ZLAT).AND.(.NOT.LPLAINU) ) THEN
C
          IF( NDBG.GT.1 ) CALL INTLOG(JP_DEBUG,
     X      'JWINDLL: Pole special for spectral Ucos(t), Vcos(t)',
     X      JPQUIET)
C
          CALL JSPPOLE( PSHUP, 1, KTRUNC, .TRUE., ZF)
          DO LOOP = 1, ILN
            PZFA( LOOP, INORTH) = ZF( LOOP)
          ENDDO
C
          CALL JSPPOLE( PSHUP, 0, KTRUNC, .TRUE., ZF)
          DO LOOP = 1, ILN
            PZFA( LOOP, ISOUTH) = ZF( LOOP)
          ENDDO
C
        ELSE
C
C       Otherwise not at pole, or spectral U and V
C
C         Fill slots which are used
          IMN = 0
          IMP = 0
C
          DO 247 JM = 1, IMLIM
            ITAL = ILIM - JM + 1
            DO 245 J245 = 1, ITAL
              IF( LFILEIO ) THEN
                ZDUM(J245) = PLEG(IMP + J245)*PSHUP(IMN + J245)
              ELSE
                JDCLOOP = IMP + J245 + IOFF
                ZDUM(J245) = PLEG(JDCLOOP)*PSHUP(IMN + J245)
              ENDIF
 245        CONTINUE
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
 247      CONTINUE
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
          DO JM = 1, IMLIM
            PZFA(2*JM -1, INORTH) = REAL(ZSUMS(JM))  + REAL(ZSUMA(JM))
            PZFA(2*JM   , INORTH) = AIMAG(ZSUMS(JM)) + AIMAG(ZSUMA(JM))
            PZFA(2*JM -1, ISOUTH) = REAL(ZSUMS(JM))  - REAL(ZSUMA(JM))
            PZFA(2*JM   , ISOUTH) = AIMAG(ZSUMS(JM)) - AIMAG(ZSUMA(JM))
          ENDDO
        ENDIF
C
C*    End of main loop through latitude rows.
C
 280  CONTINUE
C
C     -----------------------------------------------------------------|
C*    Section 3.    Fast fourier transform
C     -----------------------------------------------------------------|
C
 300  CONTINUE
C
      IF( NDBG.GT.1 ) CALL INTLOG(JP_DEBUG,
     X  'JWINDLL: FFT, no.of rows (N and S) = ', ISOUTH)
C
      CALL FFT99(PZFA,WORK,PTRIGS,KMFAX,1,JPLONO+2,KLONO,ISOUTH,1)
C
      IF( NDBG.GT.1 ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'JWINDLL: Values calculated by FFT:',JPQUIET)
        DO NDBGLP = 1, 20
          CALL INTLOGR(JP_DEBUG,' ',PZFA( 1, NDBGLP ))
          CALL INTLOGR(JP_DEBUG,' ',PZFA( 2, NDBGLP ))
        ENDDO
      ENDIF
C
C     For spectral U, V generate the poles from a reduced gaussian line
C     of latitude.
C
      IF( GPOLE(PSTART).AND.(LPLAINU) ) THEN
C
        IF( NDBG.GT.1 ) CALL INTLOG(JP_DEBUG,
     X    'JWINDLL: Poles generated from gaussian for spectral U, V',
     X    JPQUIET)
C
        CALL JUVPOLE(PSHUP, KTRUNC, PZFA, KLONO, KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'JWINDLL: pole wind create error',IWORK)
          KRET = JPROUTINE + 1
          GOTO 990
        ENDIF
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 4.    For spectral Ucos(theta), Vcos(theta), apply scale
C                   factor to all latitudes except poles.
C     -----------------------------------------------------------------|
C
 400  CONTINUE
C
      IF( NDBG.GT.0 ) THEN
        IF( LPLAINU ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'JWINDLL: Do not apply scale to latitudes',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,
     X      'JWINDLL: Apply scale to latitudes',JPQUIET)
        ENDIF
      ENDIF
C
      IF( .NOT.LPLAINU ) THEN
        INORTH = -1
        DO JNEXTLAT = 1, KLATO
          ZLAT  = ( PSTART - (PINTVL * REAL(JNEXTLAT - 1)) )
          INORTH = INORTH + 2
          ISOUTH = INORTH + 1
          IF( .NOT.GPOLE(ZLAT) ) THEN
            ZCOSI = 1.0 / COS( ZLAT * ZDEGR )
            CALL EMOSLIB_SSCAL( KLONO, ZCOSI, PZFA( 2, INORTH), 1)
            CALL EMOSLIB_SSCAL( KLONO, ZCOSI, PZFA( 2, ISOUTH), 1)
          ENDIF
        ENDDO
C
      ENDIF
C     -----------------------------------------------------------------|
C*    Section 9. Return to calling routine. Format statements
C     -----------------------------------------------------------------|
C
 900  CONTINUE
C
      KRET = 0
C
 990  CONTINUE
      RETURN
      END
