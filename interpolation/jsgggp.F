C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JSGGGP( PSHUP, KTRUNC, PNORTH, PSOUTH, PWEST, PEAST,
     X                    KNUM, HTYPE, KPTS, POUTF, LUV, KRET)
C
C---->
C**** JSGGGP
C
C     PURPOSE
C     _______
C
C     This routine converts streched spectral fields to a gaussian
C     grid field.
C
C     INTERFACE
C     _________
C
C     CALL JSGGGP( PSHUP, KTRUNC, PNORTH, PSOUTH, PWEST, PEAST,
C    X                    KNUM, HTYPE, KPTS, POUTF, LUV, KRET)
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
C     KNUM     - Gaussian grid number
C     HTYPE    - Gaussian grid type
C                = 'R' for reduced ("quasi-regular"),
C                = 'O' for reduced/octahedral,
C                = 'F' for full,
C                = 'U' for user-defined reduced gaussian grid
C     KPTS     - Array specifying number of points at each line of
C                latitude if HTYPE = 'U'.
C                (Must be big enough for both hemispheres, but only
C                 needs to give Northern hemisphere values from pole
C                 to latitude nearest the Equator.)
C     LUV      - Code indicating whether or not the field is a wind
C                component field;
C                = 1 if U or V field.
C
C     Output parameters
C     ________________
C
C     KPTS     - Array specifying number of points at each line of
C                latitude; user defined values if HTYPE = 'U'.
C                (Must be big enough for both hemispheres)
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
C     Converts stretched 'latitude' to real latitude and then
C     interpolates along the real latitude line.
C
C
C     Externals
C     _________
C
C     JGETGG   - Pick up the definition of the gaussian grid.
C     JJSET99  - Sets up FFT
C     JNORSGG  - Determines which latitude row in a gaussian grid is
C                north or south of a given latitude
C     JSYMGG   - Generate symmetric grid from spherical harmonics
C     JMKOFGG  - Builds offsets into storage array
C     JDEBUG   - Checks environment variable to switch on/off debug
C     INTLOG   - Logs output messages
C     INTLOGR  - Logs output messages
C
C
C     Reference
C     _________
C
C     None
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
C
C     None
C
C----<
C     _______________________________________________________
C
C
      IMPLICIT NONE
#include "jparams.h"
#include "parim.h"
#include "current.h"
#include "nifld.common"
#include "nofld.common"
#include "grfixed.h"
C
C     Subroutine arguments
C
      COMPLEX   PSHUP
      DIMENSION PSHUP(*)
      INTEGER   KTRUNC, KNUM, KPTS, KRET
      DIMENSION KPTS(*)
      CHARACTER*1 HTYPE
      REAL      PNORTH, PSOUTH, PWEST, PEAST
      REAL      POUTF
      DIMENSION POUTF(*)
      INTEGER   LUV
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER ( JPROUTINE = 31600 )
      REAL DEG2RAD
      PARAMETER ( DEG2RAD = PPI/180.0 )
C
C     Local variables
C
      INTEGER ILON, ILONOLD, ITOUP1, IJPWR, NUMPTS
      DIMENSION IJPWR(JPMAX)
      INTEGER ILUNIT
      INTEGER NXTROW
      REAL RINTVL, ZCOSI, ZLAT
      REAL ZFACTOR(JPMAX)
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
      DIMENSION ZFA(2)
      POINTER ( IZFA, ZFA )
      INTEGER ISIZE
      DATA ISIZE/0/
      SAVE ISIZE, IZFA
C
      INTEGER J108, NNORTH, NSOUTH, NFLAG
      INTEGER NOFSET, LOOP
      DIMENSION NOFSET(JPMAX)
      INTEGER INORTH, ISOUTH, IMAGE
      REAL ALAT
      DIMENSION ALAT(JPMAX)
C
      SAVE ILUNIT
C
      LOGICAL LDEBUG, LFIRST
      DATA LFIRST/.TRUE./
      SAVE LDEBUG, LFIRST
C
      INTEGER  JNORSGG
      EXTERNAL JNORSGG
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
C
C     First time through, dynamically allocate memory for workspace
C
      IF( LFIRST ) THEN
        LFIRST = .FALSE.
        ISIZE =  (JPLONO+2)*64
        CALL JMEMHAN( 10, IZFA, ISIZE, 1, KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'JSGGGP: memory allocation error.',IZFA)
          KRET = JPROUTINE + 1
          GOTO 990
        ENDIF
      ENDIF
C
      IF( LDEBUG ) THEN
        CALL INTLOG(JP_DEBUG,'JSGGGP: Input parameters:',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'JSGGGP: Spherical harmonic truncation = ', KTRUNC)
        CALL INTLOGR(JP_DEBUG,
     X    'JSGGGP: Northern latitude for output = ', PNORTH)
        CALL INTLOGR(JP_DEBUG,
     X    'JSGGGP: Southern latitude for output = ', PSOUTH)
        CALL INTLOGR(JP_DEBUG,
     X    'JSGGGP: Western longitude for output = ', PWEST)
        CALL INTLOGR(JP_DEBUG,
     X    'JSGGGP: Eastern longitude for output = ', PEAST)
        CALL INTLOG(JP_DEBUG,
     X    'JSGGGP: Gaussian grid number = ', KNUM)
C
        IF( HTYPE.EQ.'R' .OR. HTYPE.EQ.'r' ) CALL INTLOG(JP_DEBUG,
     X    'JSGGGP: Grid type is Reduced.',JPQUIET)
        IF( HTYPE.EQ.'O' .OR. HTYPE.EQ.'o' ) CALL INTLOG(JP_DEBUG,
     X    'JSGGGP: Grid type is Reduced/octahedral.',JPQUIET)
        IF( HTYPE.EQ.'F' .OR. HTYPE.EQ.'f' ) CALL INTLOG(JP_DEBUG,
     X    'JSGGGP: Grid type is Full.',JPQUIET)
        IF( HTYPE.EQ.'U' .OR. HTYPE.EQ.'u' ) CALL INTLOG(JP_DEBUG,
     X    'JSGGGP: Grid type is User.',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'JSGGGP: Field code( = 1 for U or V) = ', LUV)
C
        IF( HTYPE.EQ.'u' .OR. HTYPE.EQ.'U' ) THEN
          CALL INTLOG(JP_DEBUG,'JSGGGP: User-supplied values.',JPQUIET)
          CALL INTLOG(JP_DEBUG,'JSGGGP: No. pts at each line:',JPQUIET)
          DO NDBGLP = 1, 20
            CALL INTLOG(JP_DEBUG,' ',KPTS( NDBGLP ))
          ENDDO
        ENDIF
      ENDIF
C
C     Pick up the file containing the definition of the gaussian grid.
C
      IF( (NOGAUSO.NE.KNUM).OR.(HOGAUST.NE.HTYPE) ) THEN
        CALL JGETGG( KNUM, HTYPE, ALAT, KPTS, KRET)
        IF( KRET.NE.0 ) GOTO 990
        NOGAUSO = KNUM
        HOGAUST = HTYPE
      ELSE
        DO LOOP = 1, KNUM*2
          ALAT(LOOP) = ROGAUSS(LOOP)
          KPTS(LOOP) = NOLPTS(LOOP)
        ENDDO
      ENDIF
C
C     Calculate the true latitudes and the map factor.
C
      DO LOOP = 1, KNUM*2
        CALL STRLAT(ALAT(LOOP), RISTRET, 0, ALAT(LOOP), ZFACTOR(LOOP))
      ENDDO
C
C     Coefficients are to generated 'on the fly'
C
      LON_FLY = .TRUE.
      LFILEIO = .FALSE.
      LMEMORY = .FALSE.
      LSHARED = .FALSE.
      LMAPPED = .FALSE.
      ISIZE =  (KTRUNC+1)*(KTRUNC+2)
      CALL JMEMHAN( 14, IALEG, ISIZE, 1, KRET)
      IF( KRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'JSGGGP: memory allocate error.',IALEG)
        KRET = JPROUTINE + 2
        GOTO 990
      ENDIF
C
      IF( KRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'JSGGGP: Failed to open legendre function file',JPQUIET)
        KRET = JPROUTINE + 3
        GOTO 990
      ENDIF
C
      NNORTH = JNORSGG( PNORTH, ALAT, KNUM, 1)
      NSOUTH = JNORSGG( PSOUTH, ALAT, KNUM, 0)
      IF( LDEBUG ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'JSGGGP: Gaussian latitudes in requested area:',JPQUIET)
        CALL INTLOG(JP_DEBUG,'JSGGGP: NORTH = ',NNORTH)
        CALL INTLOG(JP_DEBUG,'JSGGGP: SOUTH = ',NSOUTH)
      ENDIF
C
C
C     For calculation purposes, the number of longitude points for each
C     latitude row has to be greater than 2*(output truncation) to
C     ensure the fourier transform is exact (see Reference, page 10).
C
C     The following loop calculates the power of 2 to be used as a
C     multipier for each row to ensure there are enough points.
C     Later, when filling the output array POUTF, the longitude points
C     have to be taken selectively to avoid the intermediate generated
C     points, picking up values only at the required longitudes.
C
      ITOUP1 = KTRUNC
      DO 108 J108 = 1, KNUM
        IMAGE = 2*KNUM - J108 + 1
        ILON        = KPTS(J108)
        IJPWR(J108) = 1
 104    CONTINUE
          IF(ILON .GT. 2*ITOUP1) GOTO 106
          IJPWR(J108) = IJPWR(J108)*2
          ILON        = ILON*2
          GOTO 104
 106    CONTINUE
        IJPWR(IMAGE) = IJPWR(J108)
 108  CONTINUE
C
C     _______________________________________________________
C
C*    Section 2.    Work out geometry of the request
C     _______________________________________________________
C
 200  CONTINUE
C
C     Latitude rows are generated from row INORTH to ISOUTH.
C     Selection is carried out for actual requested rows when the
C     calculated values are moved to the output array.
C
      INORTH = NNORTH
      ISOUTH = NSOUTH
C
C     Setup the storage offset for the given latitude range
C
 220  CONTINUE
C
      IF( LDEBUG ) THEN
        CALL INTLOG(JP_DEBUG,'JSGGGP: INORTH = ', INORTH)
        CALL INTLOG(JP_DEBUG,'JSGGGP: ISOUTH = ', ISOUTH)
      ENDIF
      CALL JMKOFGG(HTYPE,NNORTH,NSOUTH,PWEST,PEAST,KNUM,KPTS,NOFSET)
C
C     _______________________________________________________
C
C*    Section 3.    Process one row at a time.
C     _______________________________________________________
C
 300  CONTINUE
C
      IF( LDEBUG )
     X  CALL INTLOG(JP_DEBUG,'JSGGGP: Process latitude rows.',JPQUIET)
C
      ILONOLD = 0
C
      DO NXTROW = INORTH, ISOUTH
C
C       Initialise FFT constants for the latitude
C
        ILON = KPTS(NXTROW) * IJPWR(NXTROW)
C
        IF( LDEBUG ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'JSGGGP: Initialise FFT consts for the group.',JPQUIET)
          CALL INTLOG(JP_DEBUG,
     X      'JSGGGP: Latitude number = ', NXTROW)
          CALL INTLOG(JP_DEBUG,
     X      'JSGGGP: No. of calculated long.pts = ', ILON)
        ENDIF
C
        IF( ILON.NE.ILONOLD ) THEN
          ILONOLD = ILON
          CALL JJSET99( ATRIGS, IMFAX, ILON, KRET)
          IF( KRET.NE.0 ) GOTO 990
        ENDIF
C
C       Calculate the points along the line of latitude
C
        CALL JSYMGG( PSHUP, KTRUNC, NXTROW, ILUNIT, 1, ILON,
     X               ALAT, ALEG, ATRIGS, IMFAX, .FALSE., ZFA, KRET)
        IF( KRET.NE.0 ) GOTO 990
C
C       If it is a wind component field, apply latitude correction
C
        IF( LUV.EQ.1 ) THEN
          ZLAT   = ALAT(NXTROW)
          ZCOSI  = ZFACTOR(NXTROW) / ABS( COS(ZLAT*DEG2RAD) )
          CALL EMOSLIB_SSCAL( ILON, ZCOSI, ZFA(2), 1)
        ENDIF
C
C       Store away the generated latitudes
C
        IF( HTYPE.EQ.'R' .OR. HTYPE.EQ.'r' .OR.
     X      HTYPE.EQ.'O' .OR. HTYPE.EQ.'o' .OR.
     X      HTYPE.EQ.'U' .OR. HTYPE.EQ.'u' ) THEN
          NUMPTS = KPTS(NXTROW)
        ELSE
          RINTVL = (PEAST - PWEST)*FLOAT( KPTS(NXTROW) ) / 360.0
          NUMPTS = NINT( RINTVL ) + 1
        ENDIF
C
C       Move latitude line to output array
C
        NFLAG = 1
        CALL JMOVGG( ZFA, PWEST, KNUM, NXTROW, 1, NUMPTS,
     X    ILON, IJPWR(NXTROW), NOFSET, POUTF, NFLAG)
C
      ENDDO
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
 930  CONTINUE
      KRET = 0
C
 990  CONTINUE
C
      IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'JSGGGP: Return code = ',KRET)
C
      RETURN
      END
