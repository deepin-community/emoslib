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

      SUBROUTINE JSYMGG( PSHUP,KTRUNC,KSTART,KLUNIT,KLATO,KLONO,
     X                    PLAT,PLEG,PTRIGS,KMFAX,LUSEFFTW,PZFA,KRET)
C
C---->
C**** JSYMGG
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
C     CALL JSYMGG( PSHUP, KTRUNC, KSTART, KLUNIT, KLATO, KLONO,
C    X             PLAT, PLEG, PTRIGS, KMFAX, PZFA, KRET)
C
C     Input parameters
C     ________________
C
C     PSHUP    - Spherical harmonics field, unpacked
C     KTRUNC   - Truncation number of spherical harmonics field
C     KSTART   - Number of start latitude row (northernmost) for output
C                field (must be positive - see comments below)
C     KLUNIT   - stream number of the legendre function file
C     KLATO    - Number of latitude rows in output field
C     KLONO    - Number of longitude points in output field
C     PLAT     - Array of gaussian latitudes
C     PLEG     - Array used to hold legendre functions
C     PTRIGS   - Initialized array of trig.functions (setup by JJSET99)
C     KMFAX    - Initialized array of prime factors (setup by JJSET99)
C     LUSEFFTW - Use FFTW instead of FFT99
C
C     Output parameters
C     ________________
C
C     PZFA     - Output grid point field; contains up to 32 each of
C                North and South latitude rows symmetrically.
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
C     JREADGG - Reads the legendre functions for a latitude
C     FFT99   - Carries out FFT
C     INTLOG  - Output log message
C     INTLOGR - Output log message (with real value)
C     NMAKGG  - Make interpolation coefficients one latitude at a time
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
C     It handles transformation to a gaussian grid.
C     The generated grid is symmetrical about the equator, so
C     KSTART must be positive.
C
C     It is not for U and V fields (no correction is applied at the
C     poles).
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers      *ECMWF*      Jan 1994
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
      PARAMETER ( JPROUTINE = 31200 )
C
C     Subroutine arguments
      COMPLEX   PSHUP
      DIMENSION PSHUP(*)
      INTEGER   KTRUNC
      INTEGER   KSTART
      INTEGER   KLUNIT, KLATO, KLONO, KMFAX, KRET
      REAL      PLEG, PTRIGS, PZFA
      LOGICAL   LUSEFFTW
      DIMENSION PZFA(JPLONO + 2, 64)
      DIMENSION KMFAX(*), PLEG(*), PTRIGS(*)
      REAL      PLAT
      DIMENSION PLAT(*)
C
C     Local variables
      INTEGER   ILIM, IMLIM, ILN
      INTEGER   ITAL, ITALA, ITALS, IMN, IMP
      INTEGER   INORTH, ISOUTH
      INTEGER   JM, J, JNEXTLAT, JF
      INTEGER   NERR
      INTEGER*8 JDCLOOP
      INTEGER*8 IOFF
C
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IWORK
#endif
#endif
#ifdef HAVE_FFTW
      INTEGER*8 PLAN
      INTEGER   N(64), IDIST, ODIST
      REAL      X1, XN
#include "fftw3.f"
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
C
C     First time through, dynamically allocate memory for workspace
C
      IF( ISIZE.EQ.0 .AND. .NOT. LUSEFFTW ) THEN
        ISIZE =  2*JPFFT*64
        CALL JMEMHAN( 9, IWORK, ISIZE, 1, KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'JSYMGG: memory allocation error.',IWORK)
          KRET = JPROUTINE + 1
          GOTO 990
        ENDIF
      ENDIF
C
      IF ( NDBG .GT. 1) THEN
        CALL INTLOG(JP_DEBUG,
     X    'JSYMGG: Spherical harmonic coeffs(first 20):',JPQUIET)
        DO NDBGLP = 1, 20
          CALL INTLOGR(JP_DEBUG,' ',PSHUP( NDBGLP ))
        ENDDO
        CALL INTLOG(JP_DEBUG,'JSYMGG: Input parameters:',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'JSYMGG: Spherical harmonic truncation = ', KTRUNC)
        CALL INTLOG(JP_DEBUG,
     X    'JSYMGG: Start latitude(northernmost) = ', KSTART)
        CALL INTLOG(JP_DEBUG,
     X    'JSYMGG: Stream number of leg. file = ', KLUNIT)
        CALL INTLOG(JP_DEBUG,
     X    'JSYMGG: Number of lat. rows in output = ', KLATO)
        CALL INTLOG(JP_DEBUG,
     X    'JSYMGG: Number of long. pts per row = ', KLONO)
        IF( .NOT. LUSEFFTW ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'JSYMGG: Trig.functions (setup by JJSET99):',JPQUIET)
          DO NDBGLP = 1, 10
            CALL INTLOGR(JP_DEBUG,' ',PTRIGS( NDBGLP ))
          ENDDO
          CALL INTLOG(JP_DEBUG,
     X      'JSYMGG: Prime factors (setup by JJSET99):',JPQUIET)
          DO NDBGLP = 1, 10
            CALL INTLOG(JP_DEBUG,' ',KMFAX( NDBGLP ))
          ENDDO
        ENDIF
      ENDIF
C
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
C
C     For each latitude, the north and corresponding south latitude row
C     are calculated at the same time from the same legendre functions.
C
      DO JNEXTLAT = KSTART, KSTART+KLATO-1
C
        IF ( NDBG .GT. 1)
     X    CALL INTLOG(JP_DEBUG,'JSYMGG: Next latitude = ', JNEXTLAT)
C
C       If required, generate the coefficients 'on the fly'
C
        IF( LON_FLY ) THEN
          CALL NMAKGG( KTRUNC, JNEXTLAT, PLAT, 1, PLEG, NERR)
          IOFF = 0
        ELSE IF( LFILEIO ) THEN
          CALL JREADGG( KLUNIT, KTRUNC, JNEXTLAT, PLEG, NERR)
          IF ( NERR .NE. 0 ) THEN
            CALL INTLOG(JP_ERROR,'JSYMGG: JREADGG error',NERR)
            KRET = JPROUTINE + 2
            GOTO 990
          ENDIF
        ELSE
C         IOFF = (JNEXTLAT-1)*(KTRUNC+1)*(KTRUNC+4)/2
          IOFF = (JNEXTLAT-1)
          IOFF = (KTRUNC+1) * IOFF
          IOFF = (KTRUNC+4) * IOFF
          IOFF = IOFF/2
        ENDIF
C
C       Clear unused slots in array.
C       Note there are two slots in the array - one for north latitude
C       and one for the corresponding south latitude.
C
        INORTH = INORTH + 2
        ISOUTH = INORTH + 1
        DO JF = 2*IMLIM + 1, ILN
          PZFA(JF, INORTH) = 0.0
          PZFA(JF, ISOUTH) = 0.0
        ENDDO
C
C       Now fill slots which are used
C
        IMN = 0
        IMP = 0
C
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
C
C*    End of main loop through latitude rows.
C
      ENDDO
C
C     _______________________________________________________
C
C*    Section 3.    Fast fourier transform
C     _______________________________________________________
C
 300  CONTINUE
C
      IF ( NDBG .GT. 1) CALL INTLOG(JP_DEBUG,
     X  'JSYMGG: FFT, no.of rows (N and S) = ',ISOUTH)
C
#ifdef HAVE_FFTW
      IF( LUSEFFTW ) THEN
        N    = 0
        N(1) = KLONO
        IDIST = (JPLONO+2)/2
        ODIST = (JPLONO+2)
#ifdef REAL_8
C       double precision fftw
        CALL DFFTW_PLAN_MANY_DFT_C2R( PLAN, 1, N, ISOUTH,
     X         PZFA, 0, 1, IDIST,  ! in,  inembed, istride, idist
     X         PZFA, 0, 1, ODIST,  ! out, onembed, ostride, odist
     X         FFTW_ESTIMATE + FFTW_UNALIGNED + FFTW_NO_SIMD +
     X         FFTW_DESTROY_INPUT )
        CALL DFFTW_EXECUTE( PLAN )
        CALL DFFTW_DESTROY_PLAN( PLAN )
#else
C       single precision fftw
        CALL SFFTW_PLAN_MANY_DFT_C2R( PLAN, 1, N, ISOUTH,
     X         PZFA, 0, 1, IDIST,
     X         PZFA, 0, 1, ODIST,
     X         FFTW_ESTIMATE + FFTW_UNALIGNED + FFTW_NO_SIMD )
        CALL SFFTW_EXECUTE( PLAN )
        CALL SFFTW_DESTROY_PLAN( PLAN )
#endif
C
C       shift right and enforce periodicity (see FFT99 documentation):
C         from [ x1, x2, ..., xN-1, xN,   0,  0,  (0...) ]
C         to   [ xN, x1, x2, ...,   xN-1, xN, x1, (0...) ]
        DO J=1, ISOUTH
          X1 = PZFA(   1,   J )
          XN = PZFA( KLONO, J )
          PZFA( 2:KLONO+1, J ) = PZFA(1:KLONO,J)
          PZFA(     1,     J ) = XN
          PZFA(  KLONO+2,  J ) = X1
        ENDDO
      ELSE
        CALL FFT99(PZFA,WORK,PTRIGS,KMFAX,1,JPLONO+2,KLONO,ISOUTH,1)
      ENDIF
#else
      CALL FFT99(PZFA,WORK,PTRIGS,KMFAX,1,JPLONO+2,KLONO,ISOUTH,1)
#endif
C
      IF ( NDBG .GT. 1) THEN
        CALL INTLOG(JP_DEBUG,
     X    'JSYMGG: Values calculated by FFT:',JPQUIET)
        DO NDBGLP = 1, 20
          CALL INTLOGR(JP_DEBUG,' ',PZFA( 1, NDBGLP ))
          CALL INTLOGR(JP_DEBUG,' ',PZFA( 2, NDBGLP ))
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
