C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JSLLGP( PSHUP, KTRUNC, PNORTH, PSOUTH, PWEST, PEAST,
     X                    PLATINC, PLONINC, POUTF, KRET)
C
C---->
C**** JSLLGP
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
C     CALL JSLLGP( PSHUP, KTRUNC, PNORTH, PSOUTH, PWEST, PEAST,
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
C     INTLOG   - Output log message
C     INTLOGR  - Output log message (with real value)
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
C     JDCNDBG
C
C     Method
C     ______
C
C     Generates one latitude at a time.
C
C     Externals
C     _________
C
C     JJSET99   - Sets up FFT
C     JSTRLL    - Generate latitude line from spherical harmonics
C     JSTRWLL   - Generate latitude line from spherical harmonics (wind)
C     JDEBUG    - Checks environment variable to switch on/off debug
C     INTLOG    - Logs output messages
C     INTLOGR   - Logs output messages
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
C     None
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
C     None
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
      PARAMETER ( JPROUTINE = 30000 )
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
      REAL DEG2RAD, ZBUILD
      INTEGER ILON, IOLON, ITOUP1, IJPWR, NEXT, NROWS, NUMLON
      INTEGER IOFFN, LONGIT, N360, NSTART, INEXT, LOOPCL
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
      INTEGER ISIZE, ISIZE2
      DATA ISIZE/0/, ISIZE2/0/
      SAVE ISIZE, ISIZE2, IZFA
C
      SAVE IMFAX, ATRIGS, IALEG
C
C     _______________________________________________________
C
C*    Section 1.    Initialization.
C     _______________________________________________________
C
  100 CONTINUE
C
      CALL JDEBUG( )
C
C     First time through, dynamically allocate memory for workspace
C
      IF( ISIZE.EQ.0 ) THEN
        ISIZE = (JPLONO+2) * 2
        CALL JMEMHAN( 10, IZFA, ISIZE, 1, KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'JSLLGP: memory allocation error.',IZFA)
          KRET = JPROUTINE + 1
          GOTO 900
        ENDIF
      ENDIF
C
      IF( NDBG.GT.0 ) THEN
        CALL INTLOG(JP_DEBUG,'JSLLGP: Input parameters:', JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'JSLLGP: Spherical harmonic coeffs(first 20):', JPQUIET)
        DO 101 NDBGLP = 1, 20
          CALL INTLOGR(JP_DEBUG,' ', PSHUP( NDBGLP ))
  101   CONTINUE
        CALL INTLOG(JP_DEBUG,
     X    'JSLLGP: Spherical harmonic truncation = ', KTRUNC)
        CALL INTLOGR(JP_DEBUG,
     X    'JSLLGP: Northern latitude for output = ', PNORTH)
        CALL INTLOGR(JP_DEBUG,
     X    'JSLLGP: Southern latitude for output = ', PSOUTH)
        CALL INTLOGR(JP_DEBUG,
     X    'JSLLGP: Western longitude for output = ', PWEST)
        CALL INTLOGR(JP_DEBUG,
     X    'JSLLGP: Eastern longitude for output = ', PEAST)
        CALL INTLOGR(JP_DEBUG,
     X    'JSLLGP: Latitude grid interval (deg) = ', PLATINC)
        CALL INTLOGR(JP_DEBUG,
     X    'JSLLGP: Longitude grid interval (deg) = ', PLONINC)
      ENDIF
C
C     Coefficients are to generated 'on the fly',
C
      ISIZE2 =  (KTRUNC+1)*(KTRUNC+2)
      CALL JMEMHAN( 14, IALEG, ISIZE2, 1, KRET)
      IF( KRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'JSLLGP: memory allocate error.',IALEG)
        KRET = JPROUTINE + 2
        GOTO 900
      ENDIF
C
      DEG2RAD = PPI / 180.0
      IOLON   = NINT( 360.0 / PLATINC )
      ILON    = NINT( 360.0 / PLONINC )
      ITOUP1  = KTRUNC
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
      IF( NDBG.GT.0 ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'JSLLGP: Initialise FFT constants with ', ILON)
        CALL INTLOG(JP_DEBUG,
     X    'JSLLGP: longitude points.', JPQUIET)
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
      PNORMX = PNORTH
      PDIFF  = PNORTH - PSOUTH
      NROWS = NINT( PDIFF/PLATINC )  + 1
      IF( ((NROWS-1)*PLATINC).GT.PDIFF ) NROWS = NROWS - 1
C
      NUMLON = NINT( (PEAST - PWEST)/PLONINC ) + 1
      ZBUILD = PLATINC
C
      IF( NDBG.GT.0 ) THEN
        CALL INTLOG(JP_DEBUG,'JSLLGP: Number of longitudes = ', NUMLON)
        CALL INTLOGR(JP_DEBUG,
     X    'JSLLGP: North latitude used for calc = ', PNORMX)
        CALL INTLOG(JP_DEBUG,'JSLLGP: No.lats used in calc = ', NROWS)
      ENDIF
C     _______________________________________________________
C
C*    Section 4.    Process one row at a time.
C     _______________________________________________________
C
 400  CONTINUE
C
      DO NEXT = 1, NROWS
        PSTART = PNORMX - REAL(NEXT-1)*PLATINC
C
        IF( NDBG.GT.0 )
     X    CALL INTLOGR(JP_DEBUG, 'JSLLGP: Next latitude = ', PSTART)
C
C       Generate the next row
C
        IF( LWIND ) THEN
          CALL JSTRWLL(PSHUP, KTRUNC, PSTART, ZBUILD, ILON,
     X                ALEG, ATRIGS, IMFAX, ZFA, KRET)
        ELSE
          CALL JSTRLL(PSHUP, KTRUNC, PSTART, ZBUILD, ILON,
     X                ALEG, ATRIGS, IMFAX, ZFA, KRET)
        ENDIF
        IF( KRET.NE.0 ) GOTO 900
C
C       Find start offset in storage array
C
        IOFFN  = NINT( (PNORTH - PSTART)/PLATINC ) + 1
        LONGIT = NINT( (360.0 + PWEST)/PLONINC )
        N360 = NINT( 360.0/PLONINC )
        NSTART = MOD( LONGIT, N360)*IJPWR
C
        DO LOOPCL = 1, NUMLON
          INEXT = 2 + MOD( NSTART + (LOOPCL-1)*IJPWR , ILON)
          POUTF( LOOPCL + (IOFFN-1)*NUMLON ) = ZFA( INEXT)
        ENDDO
C
      ENDDO
C
      KRET = 0
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
