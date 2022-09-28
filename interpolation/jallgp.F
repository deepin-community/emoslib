C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JALLGP( PSHUP, KTRUNC, PNORTH, PSOUTH, PWEST, PEAST,
     X                    PLATINC, PLONINC, POUTF, KRET)
C
C---->
C**** JALLGP
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
C     CALL JALLGP( PSHUP, KTRUNC, PNORTH, PSOUTH, PWEST, PEAST,
C    X                    PLATINC, PLONINC, POUTF, KRET)
C
C     Input parameters
C     ________________
C
C     PSHUP    - Spherical harmonics field, unpacked
C     KTRUNC   - Truncation number of spherical harmonics field
C     PNORTH   - Northernmost latitude for output field (degrees)
C     PSOUTH   - Southernmost latitude for output field (degrees)
C     PWEST    - Westernmost longitude for output field (degrees)
C     PEAST    - Easternmost longitude for output field (degrees)
C     PLATINC  - Grid interval between latitudes in degrees
C     PLONINC  - Grid interval between longitudes in degrees
C
C     Output parameters
C     ________________
C
C     POUTF    - Output grid point field
C     KRET     - Return status code
C                0 = OK
C
C     Common block usage
C     __________________
C
C     JDCNDBG, JDCFLAGS
C
C
C     Method
C     ______
C
C     None.
C
C     Externals
C     _________
C
C     JOPNLL   - Opens the legendre functions file for reading line
C                by line.
C     JOPNLLF  - Opens the legendre functions file and reads the whole
C                of it into dynamically allocated memory.
C     JOPNLLSM - Opens the legendre functions file and memory maps it.
C
C     JJSET99  - Sets up FFT
C     JSYMLL   - Generate symmetric grid from spherical harmonics
C     JWINDLL  - Generate symmetric grid from spherical harmonics(wind)
C     JMOVLL   - Move points of symmetric grid to array
C     JMVULL   - Move points of unsymmetric grid to array
C     JDEBUG   - Checks environment variable to switch on/off debug
C     INTLOG   - Logs output messages
C     INTLOGR  - Logs output messages
C     GETENV   - Get value of an environment variable
C     GETCONF  - Decides how to handle legendre coefficients
#ifdef SHAREDMEMORY
C     SMREAD   - Gets interpolation coefficients in a shared memory array
#endif
C
C
C     Reference
C     _________
C
C     E.C.M.W.F. Research Department technical memorandum no. 56
C                "The forecast and analysis post-processing package"
C                May 1982. J.Haseler.
C
C
C     Comments
C     ________
C
C     This is a redesign, based on SPECGP.F
C
C     It handles transformations to a regular lat/long grid assuming.
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
C
C     J.D.Chambers     ECMWF        Feb 1997
C     Allow for 64-bit pointers
C
C----<
C     _______________________________________________________
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
      IMPLICIT NONE
#include "jparams.h"
#include "parim.h"
#include "nifld.common"
C
C     Parameters
C
      INTEGER JPROUTINE
      REAL PEPS
      PARAMETER ( JPROUTINE = 30000 )
      PARAMETER ( PEPS = 1E-6 )
C                   |
C                    `-> value used to compare equality of calculated
C                        latitude with area limits
C
C     Subroutine arguments
C
      COMPLEX   PSHUP
      DIMENSION PSHUP(*)
      INTEGER   KTRUNC
      REAL      PNORTH, PSOUTH, PWEST, PEAST, PLATINC, PLONINC
      REAL      POUTF
      DIMENSION POUTF(*)
      INTEGER   KRET
C
C     Local variables
C
      REAL PSTART, PNORMX, PDIFF
      REAL ZDEGR, ZBUILD
      INTEGER ILON, IOLON, ITOUP1, IJPWR
      INTEGER ILUNIT, ISTART
      INTEGER NEXT, NCALLS, NLEFT, NROWS, NFLAG, NCASE, NUMLON
C
      INTEGER IMFAX
      REAL ATRIGS
      DIMENSION IMFAX(10),ATRIGS(JPLONO*2)
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IALEG, IZFA
#endif
#endif
      REAL ALEG
      DIMENSION ALEG(1)
      POINTER ( IALEG, ALEG )
      REAL ZFA
      DIMENSION ZFA(1)
      POINTER ( IZFA, ZFA )
C
C     Integer Itimer,timer1
      INTEGER ISIZE, ISIZE2
      DATA ISIZE/0/, ISIZE2/0/
      SAVE ISIZE, ISIZE2, IZFA
C
      SAVE ILUNIT, IMFAX, ATRIGS, IALEG
C
      LOGICAL LDEBUG, LFIRST
      DATA LFIRST/.TRUE./
      SAVE LDEBUG, LFIRST
C
C     Externals
C
#ifdef SHAREDMEMORY
      INTEGER SHAREDLL, SHAREDDT
      EXTERNAL SHAREDLL, SHAREDDT
#endif
C
C     _______________________________________________________
C
C*    Section 1.    Initialization.
C     _______________________________________________________
C
  100 CONTINUE
C
      CALL JDEBUG( )
      LDEBUG = NDBG.GT.0
C
C     Decide how to handle legendre coefficients
C
      CALL GETCONF()
C
C
C       First time through, dynamically allocate memory for workspace
C
      IF( LFIRST ) THEN
        LFIRST = .FALSE.
        ISIZE =  (JPLONO+2)*64
        CALL JMEMHAN( 10, IZFA, ISIZE, 1, KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'JALLGP: memory allocation error.',IZFA)
          KRET = JPROUTINE + 1
          GOTO 900
        ENDIF
      ENDIF
C
      IF( LDEBUG ) THEN
        CALL INTLOG(JP_DEBUG,'JALLGP: Input parameters:', JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'JALLGP: Spherical harmonic coeffs(first 20):', JPQUIET)
        DO NDBGLP = 1, 20
          CALL INTLOGR(JP_DEBUG,' ', PSHUP( NDBGLP ))
        ENDDO
        CALL INTLOG(JP_DEBUG,
     X    'JALLGP: Spherical harmonic truncation = ', KTRUNC)
        CALL INTLOGR(JP_DEBUG,
     X    'JALLGP: Northern latitude for output = ', PNORTH)
        CALL INTLOGR(JP_DEBUG,
     X    'JALLGP: Southern latitude for output = ', PSOUTH)
        CALL INTLOGR(JP_DEBUG,
     X    'JALLGP: Western longitude for output = ', PWEST)
        CALL INTLOGR(JP_DEBUG,
     X    'JALLGP: Eastern longitude for output = ', PEAST)
        CALL INTLOGR(JP_DEBUG,
     X    'JALLGP: Latitude grid interval (deg) = ', PLATINC)
        CALL INTLOGR(JP_DEBUG,
     X    'JALLGP: Longitude grid interval (deg) = ', PLONINC)
      ENDIF
C
C     Open the appropriate legendre function file.
C
      IF( LFILEIO ) THEN
        ISIZE2 = JPK
        CALL JMEMHAN( 14, IALEG, ISIZE2, 1, KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'JALLGP: memory allocate error.',IALEG)
          KRET = JPROUTINE + 2
          GOTO 900
        ENDIF
        CALL JOPNLL( ALEG, PLATINC, KTRUNC, ILUNIT, ZBUILD, KRET)
C
      ELSE IF( LMEMORY ) THEN
        CALL JOPNLLF( IALEG, PLATINC, KTRUNC, ILUNIT, ZBUILD, KRET)
C
      ELSE IF( LMAPPED ) THEN
        CALL JOPNLLSM( IALEG, PLATINC, KTRUNC, ILUNIT, ZBUILD, KRET)
C
#ifdef SHAREDMEMORY
      ELSE IF( LSHARED ) THEN
        IF( LDEBUG ) THEN
           CALL INTLOG(JP_DEBUG,'JALLGP: call SHAREDLL:', JPQUIET)
        ENDIF

        KRET=SHAREDLL(IALEG,KTRUNC,PLATINC)

c        print*,' jallgp.F KRET=',KRET
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X     'JALLGP: SHARED Failed to get legendre coeff.',KRET)
          KRET = JPROUTINE + 3
          GOTO 900
        ENDIF
        GOTO 102
#endif
      ELSE
        ISIZE2 =  (KTRUNC+1)*(KTRUNC+2)
        CALL JMEMHAN( 14, IALEG, ISIZE2, 1, KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'JALLGP: memory allocate error.',IALEG)
          KRET = JPROUTINE + 2
          GOTO 900
        ENDIF
      ENDIF
C
      IF( KRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'JALLGP: Failed to get legendre coefficients',KRET)
        KRET = JPROUTINE + 3
        GOTO 900
      ENDIF
C
  102 CONTINUE
C
      ZDEGR  = PPI / 180.0
      IOLON  = NINT( 360.0 / PLATINC )
      ILON   = NINT( 360.0 / PLONINC )
      ITOUP1 = KTRUNC
C
C     For calculation purposes, ILON, the number of longitude points,
C     has to be greater than 2*(output truncation) to ensure that the
C     fourier transform is exact (see Reference, page 10).
C     The following loop increases ILON*IJPWR until ILON=>2*ITOUP1.
C     Later, when filling the output array POUTF, the longitude points
C     have to be taken selectively to avoid the intermediate generated
C     points, picking up values only at the required longitudes.
C
      IJPWR = 1
 110  CONTINUE
        IF( ILON.GT.2*ITOUP1 ) GOTO 120
        IJPWR = IJPWR*2
        ILON  = ILON*2
        GOTO 110
 120  CONTINUE
C
C     Initialise FFT constants
C
      IF( LDEBUG ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'JALLGP: Initialise FFT constants with ', ILON)
        CALL INTLOG(JP_DEBUG,
     X    'JALLGP: longitude points.', JPQUIET)
      ENDIF
C
      CALL JJSET99( ATRIGS, IMFAX, ILON, KRET)
      IF( KRET.NE.0 ) GOTO 900
C
C     _______________________________________________________
C
C*    Section 2.    Work out geometry of the request
C     _______________________________________________________
C
 200  CONTINUE
C
C     Symmetrical about equator?
C
      NCASE  = 1
      NFLAG  = 3
      PNORMX = PNORTH
      PDIFF  = 0.0
      NROWS  = NINT( PNORTH/PLATINC )  + 1
      IF( ( ABS((NROWS-1)*PLATINC)-PNORTH).GT.PEPS ) NROWS = NROWS - 1
C
C     Northern hemisphere only ?
C
      IF( PSOUTH.GT.0.0 ) THEN
        NCASE = 2
        NFLAG = 1
        PNORMX = PNORTH
        PDIFF = PNORTH - PSOUTH
        NROWS = NINT( PDIFF/PLATINC )  + 1
        IF( ( ABS((NROWS-1)*PLATINC)-PNORTH).GT.PEPS ) NROWS = NROWS - 1
        IF( LDEBUG )
     X    CALL INTLOG(JP_DEBUG,'JALLGP: North hemisphere only.',JPQUIET)
        GOTO 300
      ENDIF
C
C     Southern hemisphere only ?
C
      IF( PNORTH.LT.0.0 ) THEN
        NCASE = 3
        NFLAG = 2
        PNORMX = ABS(PSOUTH)
        PDIFF  = PNORMX + PNORTH
        NROWS =  NINT( PDIFF/PLATINC )  + 1
        IF( ( ABS((NROWS-1)*PLATINC)-ABS(PSOUTH)).GT.PEPS )
     X    NROWS = NROWS - 1
C
        IF( LDEBUG )
     X    CALL INTLOG(JP_DEBUG,'JALLGP: South hemisphere only.',JPQUIET)
        GOTO 300
      ENDIF
C
C     Straddles equator ...
C     ... more north than south?
C
      IF( PNORTH.GT.-PSOUTH ) THEN
        NCASE = 4
        PNORMX = PNORTH
        PDIFF = PNORTH + PSOUTH
        NROWS =  NINT( PNORMX/PLATINC )  + 1
C
        IF( LDEBUG ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'JALLGP: Straddles equator.', JPQUIET)
          CALL INTLOG(JP_DEBUG,
     X      'JALLGP: More north than south.', JPQUIET)
        ENDIF
      ENDIF
C
C     ... more south than north?
C
      IF( PNORTH.LT.-PSOUTH ) THEN
        NCASE = 5
        PNORMX = ABS(PSOUTH)
        PDIFF  = PNORMX - PNORTH
        NROWS =  NINT( PNORMX/PLATINC )  + 1
        IF( ( ABS((NROWS-1)*PLATINC)-PNORMX).GT.PEPS ) NROWS = NROWS - 1
C
        IF( LDEBUG ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'JALLGP: Straddles equator.', JPQUIET)
          CALL INTLOG(JP_DEBUG,
     X      'JALLGP: More south than north.', JPQUIET)
        ENDIF
      ENDIF
C     _______________________________________________________
C
C*    Section 3.    Calculation.
C     _______________________________________________________
C
 300  CONTINUE
C
      NUMLON = NINT( (PEAST - PWEST)/PLONINC ) + 1
      ZBUILD = PLATINC
C
      IF( LDEBUG ) THEN
        CALL INTLOG(JP_DEBUG,'JALLGP: Number of longitudes = ', NUMLON)
        CALL INTLOGR(JP_DEBUG,
     X    'JALLGP: North latitude used for calc = ', PNORMX)
        CALL INTLOG(JP_DEBUG,'JALLGP: No.lats used in calc = ', NROWS)
      ENDIF
C
      NCALLS = NROWS / JP32
C
      IF( LWIND ) THEN
        IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X    'JALLGP: No.of 32 rows (calls to JWINDLL) = ', NCALLS)
      ELSE
        IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X    'JALLGP: No.of 32 rows (calls to JSYMLL) = ', NCALLS)
      ENDIF
C     _______________________________________________________
C
C*    Section 4.    Process 32 rows at a time.
C     _______________________________________________________
C
 400  CONTINUE
C
C     timer1=Itimer(0)
      DO 420 NEXT = 1, NCALLS
        PSTART = PNORMX - REAL( (NEXT -1)*JP32 ) *PLATINC
        ISTART = INT((PSTART*1000.0)+0.5)
        PSTART = ISTART/1000.0
C
        IF( LDEBUG )
     X    CALL INTLOGR(JP_DEBUG, 'JALLGP: Next latitude = ', PSTART)
C
C       Generate the next 32 rows (Northern and Southern hemisphere).
C
        IF( LWIND ) THEN
          CALL JWINDLL( PSHUP, KTRUNC, PSTART, ZBUILD, PLATINC, ILUNIT,
     X                 JP32,ILON,ALEG,ATRIGS,IMFAX,ZFA,KRET)
        ELSE
          CALL JSYMLL( PSHUP, KTRUNC, PSTART, ZBUILD, PLATINC, ILUNIT,
     X                 JP32,ILON,ALEG,ATRIGS,IMFAX,ZFA,KRET)
        ENDIF
        IF( KRET.NE.0 ) GOTO 900
C
C       If area straddles the equator and has different number of
C       north/south rows, only move a selection of rows to output array.
C
        IF( (NCASE.EQ.4).OR.(NCASE.EQ.5) ) THEN
          CALL JMVULL( ZFA, PNORTH, PSOUTH, PWEST, PLATINC, PLONINC,
     X                 PSTART, PDIFF, JP32, NUMLON, ILON, IJPWR, POUTF)
          PDIFF = PDIFF - PLATINC*JP32
          IF( PDIFF.LT.0 ) NCASE = NCASE - 2
C
C       Otherwise, move them all
C
        ELSE
          CALL JMOVLL( ZFA, PNORTH, PWEST, PLATINC, PLONINC, PSTART,
     X                 JP32, NUMLON, ILON, IJPWR, POUTF, NFLAG)
        ENDIF
C
 420  CONTINUE
C     timer1=Itimer(timer1)
C     print*,'jallgp.F legendre functions JSYMLL time:',timer1,' sec'

C
C     _______________________________________________________
C
C*    Section 5.    Process leftover rows nearest the equator.
C     _______________________________________________________
C
 500  CONTINUE
C
      NLEFT = MOD( NROWS, JP32 )
C
      IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X    'JALLGP: No.of leftover rows nearest equator = ', NLEFT)
C
C
      IF( NLEFT.NE.0 ) THEN
        PSTART = PNORMX - REAL( NCALLS*JP32 ) *PLATINC
C
      IF( LDEBUG )
     X  CALL INTLOGR(JP_DEBUG, 'JALLGP: Next latitude = ',PSTART)
C
        IF( LWIND ) THEN
          CALL JWINDLL( PSHUP, KTRUNC, PSTART, ZBUILD, PLATINC, ILUNIT,
     X                 NLEFT,ILON,ALEG,ATRIGS,IMFAX,ZFA,KRET)
        ELSE
          CALL JSYMLL( PSHUP, KTRUNC, PSTART, ZBUILD, PLATINC, ILUNIT,
     X                 NLEFT,ILON,ALEG,ATRIGS,IMFAX,ZFA,KRET)
        ENDIF
        IF( KRET.NE.0 ) GOTO 900
C
C       If area straddles the equator and has different number of
C       north/south rows, only move a selection of rows to output array.
C
        IF( (NCASE.EQ.4).OR.(NCASE.EQ.5) ) THEN
          CALL JMVULL( ZFA, PNORTH, PSOUTH, PWEST, PLATINC, PLONINC,
     X                 PSTART, 0.0, NLEFT, NUMLON, ILON, IJPWR, POUTF)
C
C       Otherwise, move them all
C
        ELSE
          CALL JMOVLL( ZFA, PNORTH, PWEST, PLATINC, PLONINC, PSTART,
     X                 NLEFT, NUMLON, ILON, IJPWR, POUTF, NFLAG)
        ENDIF
C
      ENDIF
C
      KRET = 0
#ifdef SHAREDMEMORY
      KRET = SHAREDDT(IALEG)
      IF( LDEBUG )
     X  CALL INTLOG(JP_DEBUG,'JALLGP: release_shared_file  = ', KRET)
      KRET = 0
#endif
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
 900  CONTINUE
C
      RETURN
      END
