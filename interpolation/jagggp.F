C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JAGGGP( PSHUP, KTRUNC, PNORTH, PSOUTH, PWEST, PEAST,
     X                    KNUM, HTYPE, KPTS, POUTF, LUV, KRET)
C
C---->
C**** JAGGGP
C
C     PURPOSE
C     _______
C
C     This routine converts spectral input fields to a gaussian
C     grid field.
C
C     INTERFACE
C     _________
C
C     CALL JAGGGP( PSHUP, KTRUNC, PNORTH, PSOUTH, PWEST, PEAST,
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
C     JDCNDBG, JDCFLAGS
C
C     Method
C     ______
C
C     None.
C
C
C     Externals
C     _________
C
C     JGETGG   - Pick up the definition of the gaussian grid.
C     JOPNGG   - Opens the legendre functions file for reading line
C                by line.
C     JOPNGGF  - Opens the legendre functions file and reads the whole
C                of it into dynamically allocated memory.
C     JOPNGGSM - Opens the legendre functions file and memory maps it.
C     JJSET99  - Sets up FFT
C     JNORSGG  - Determines which latitude row in a gaussian grid is
C                north or south of a given latitude
C     JSYMGG   - Generate symmetric grid from spherical harmonics
C     JWSCAL   - Apply latitude correction to wind component field
C     JMVUGG   - Moves grid point data to output array
C     JMKOFGG  - Builds offsets into storage array
C     JDEBUG   - Checks environment variable to switch on/off debug
C     INTLOG   - Logs output messages
C     INTLOGR  - Logs output messages
C     GETCONF  - Decides how to handle legendre coefficients
C     GETENV   - Get value of an environment variable
c     FORCED_FFTW - determine if FFTW should be used, instead of the
C                usual FFT99 (as per the environment)
#ifdef SHAREDMEMORY
C     SMREAD   - Gets interpolation coefficients in a shared memory array
#endif
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
C     It handles transformations to a gaussian grid.
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
#include "current.h"
#include "nifld.common"
#include "nofld.common"
#include "grfixed.h"
C
C     Subroutine arguments
C
      COMPLEX   PSHUP
      DIMENSION PSHUP(*)
      INTEGER   KTRUNC, KNUM, KPTS, KRET, KRET2
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
C
C     Local variables
C
      INTEGER ILON, ITOUP1, IJPWR, NUMPTS
      DIMENSION IJPWR(JPMAX)
      INTEGER ILUNIT
      INTEGER NXTROW
      REAL RINTVL, GNUM
      LOGICAL LUSEFFTW
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
      INTEGER ISIZE
      DATA ISIZE/0/
      SAVE ISIZE, IZFA
C
      CHARACTER*10 PLAINUV
      CHARACTER*6  HUSEFFT
C
      INTEGER NGROUP
      INTEGER FILENM, J, NNORTH, NSOUTH, NCASE, NFLAG
      INTEGER NOFSET, LOOP
      DIMENSION NOFSET(JPMAX)
      INTEGER INORTH, ISOUTH, IMAGE, NFIRST, NDIFF
      DATA FILENM/67/
      REAL ALAT
      DIMENSION ALAT(JPMAX)
C
      SAVE ILUNIT, IALEG
C
      LOGICAL LDEBUG, LFIRST, LPLAINU
      DATA LFIRST/.TRUE./, LPLAINU/.FALSE./
      SAVE LDEBUG, LFIRST, LPLAINU
      INTEGER IBLANK
C
C     External
C
      INTEGER  JNORSGG
      EXTERNAL JNORSGG
#ifdef SHAREDMEMORY
      INTEGER  SHAREDGG, SHAREDDT
      EXTERNAL SHAREDGG, SHAREDDT
#endif
      LOGICAL FORCED_FFTW
      EXTERNAL FORCED_FFTW
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
      IF( LFIRST ) THEN
C
        LFIRST = .FALSE.
C
C       Check environment variable to see if wind components are plain
C       U and V, or U*cos(theta) and V*cos(theta) (the default).
C
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
C
C       First time through, dynamically allocate memory for workspace
C
        ISIZE =  (JPLONO+2)*64
        CALL JMEMHAN( 10, IZFA, ISIZE, 1, KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'JAGGGP: memory allocation error.',IZFA)
          KRET = JPROUTINE + 1
          GOTO 990
        ENDIF
      ENDIF
C
      IF( LDEBUG ) THEN
        CALL INTLOG(JP_DEBUG,'JAGGGP: Input parameters:',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'JAGGGP: Spherical harmonic truncation = ', KTRUNC)
        CALL INTLOGR(JP_DEBUG,
     X    'JAGGGP: Northern latitude for output = ', PNORTH)
        CALL INTLOGR(JP_DEBUG,
     X    'JAGGGP: Southern latitude for output = ', PSOUTH)
        CALL INTLOGR(JP_DEBUG,
     X    'JAGGGP: Western longitude for output = ', PWEST)
        CALL INTLOGR(JP_DEBUG,
     X    'JAGGGP: Eastern longitude for output = ', PEAST)
        CALL INTLOG(JP_DEBUG,
     X    'JAGGGP: Gaussian grid number = ', KNUM)
C
        IF( HTYPE.EQ.'R' .OR. HTYPE.EQ.'r' ) CALL INTLOG(JP_DEBUG,
     X    'JAGGGP: Grid type is Reduced.',JPQUIET)
        IF( HTYPE.EQ.'O' .OR. HTYPE.EQ.'o' ) CALL INTLOG(JP_DEBUG,
     X    'JAGGGP: Grid type is Reduced/octahedral.',JPQUIET)
        IF( HTYPE.EQ.'F' .OR. HTYPE.EQ.'f' ) CALL INTLOG(JP_DEBUG,
     X    'JAGGGP: Grid type is Full.',JPQUIET)
        IF( HTYPE.EQ.'U' .OR. HTYPE.EQ.'u' ) CALL INTLOG(JP_DEBUG,
     X    'JAGGGP: Grid type is User.',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'JAGGGP: Field code( = 1 for U or V) = ', LUV)
C
        IF( HTYPE.EQ.'u' .OR. HTYPE.EQ.'U' ) THEN
          CALL INTLOG(JP_DEBUG,'JAGGGP: User-supplied values.',JPQUIET)
          CALL INTLOG(JP_DEBUG,'JAGGGP: No. pts at each line:',JPQUIET)
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
C     If coefficients are not to generated 'on the fly',
C     open the appropriate legendre function file.
C
      IF( LFILEIO ) THEN
        ISIZE = JPK
        CALL JMEMHAN( 14, IALEG, ISIZE, 1, KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'JAGGGP: memory allocate error',IALEG)
          KRET = JPROUTINE + 2
          GOTO 990
        ENDIF
        CALL JOPNGG(ALEG,KNUM,KTRUNC,ILUNIT,ALAT,KRET)
      ELSE IF( LMEMORY ) THEN
        CALL JOPNGGF(IALEG,KNUM,KTRUNC,ILUNIT,ALAT,KRET)
      ELSE IF( LMAPPED ) THEN
        CALL JOPNGGSM(IALEG,KNUM,KTRUNC,ILUNIT,ALAT,KRET)
#ifdef SHAREDMEMORY
      ELSE IF( LSHARED ) THEN
        GNUM = REAL(KNUM)
        CALL INTLOG(JP_DEBUG,'JAGGGP: call SHAREDGG', JPQUIET)
        KRET = SHAREDGG(IALEG,KTRUNC,KNUM)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'JAGGGP: SHARED MEMORY Failed to get legendre coeff.',KRET)
          KRET = JPROUTINE + 3
          GOTO 990
        ENDIF
        GOTO 102
#endif
      ELSE
        ISIZE =  (KTRUNC+1)*(KTRUNC+2)
        CALL JMEMHAN( 14, IALEG, ISIZE, 1, KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'JAGGGP: memory allocate error.',IALEG)
          KRET = JPROUTINE + 2
          GOTO 990
        ENDIF
      ENDIF
C
      IF( KRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'JAGGGP: Failed to get legendre coefficients',KRET)
        KRET = JPROUTINE + 3
        GOTO 990
      ENDIF
C
  102 CONTINUE
C
C     Determine which latitude rows in the gaussian grid are in the
C     requested area
C
      NNORTH = JNORSGG( PNORTH, ALAT, KNUM, 1)
      NSOUTH = JNORSGG( PSOUTH, ALAT, KNUM, 0)
      IF( LDEBUG ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'JAGGGP: Gaussian latitudes in requested area:',JPQUIET)
        CALL INTLOG(JP_DEBUG,'JAGGGP: NORTH = ',NNORTH)
        CALL INTLOG(JP_DEBUG,'JAGGGP: SOUTH = ',NSOUTH)
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
      DO J = 1, KNUM
        IMAGE = 2*KNUM - J + 1
        ILON     = KPTS(J)
        IJPWR(J) = 1
 104    CONTINUE
          IF(ILON .GT. 2*ITOUP1) GOTO 106
          IJPWR(J) = IJPWR(J)*2
          ILON        = ILON*2
          GOTO 104
 106    CONTINUE
        IJPWR(IMAGE) = IJPWR(J)
      ENDDO
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
C     Start with assumption that the area is symmetrical about equator.
C
      NCASE  = 1
      NFLAG  = 3
      INORTH = NNORTH
      ISOUTH = KNUM
      NDIFF  = 0
C
C     Northern hemisphere only ?
C
      IF ( PSOUTH .GE. 0.0 ) THEN
        NCASE  = 2
        NFLAG  = 1
        INORTH = NNORTH
        ISOUTH = NSOUTH
        IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X    'JAGGGP: North hemisphere only.',JPQUIET)
        GOTO 220
      ENDIF
C
C     Southern hemisphere only ?
C
      IF ( PNORTH .LE. 0.0 ) THEN
        NCASE  = 3
        NFLAG  = 2
        INORTH = 2*KNUM - NSOUTH + 1
        ISOUTH = 2*KNUM - NNORTH + 1
        IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X    'JAGGGP: South hemisphere only.',JPQUIET)
        GOTO 220
      ENDIF
C
C     Straddles equator ...
C     ... more north than south?
C
      IF ( PNORTH .GT. -PSOUTH ) THEN
        NCASE  = 4
        INORTH = NNORTH
        ISOUTH = KNUM
        IF( LDEBUG ) THEN
          CALL INTLOG(JP_DEBUG,'JAGGGP: Straddles equator.',JPQUIET)
          CALL INTLOG(JP_DEBUG,'JAGGGP: More north than south',JPQUIET)
        ENDIF
        GOTO 220
      ENDIF
C
C     ... more south than north?
C
      IF ( PNORTH .LT. -PSOUTH ) THEN
        NCASE  = 5
        INORTH = 2*KNUM - NSOUTH + 1
        ISOUTH = KNUM
        NDIFF  = NNORTH - INORTH
        IF( LDEBUG ) THEN
          CALL INTLOG(JP_DEBUG,'JAGGGP: Straddles equator.',JPQUIET)
          CALL INTLOG(JP_DEBUG,'JAGGGP: More south than north',JPQUIET)
        ENDIF
      ENDIF
C
C     Setup the storage offset for the given latitude range
C
 220  CONTINUE
C
      IF( LDEBUG ) THEN
        CALL INTLOG(JP_DEBUG,'JAGGGP: INORTH = ', INORTH)
        CALL INTLOG(JP_DEBUG,'JAGGGP: ISOUTH = ', ISOUTH)
      ENDIF
      CALL JMKOFGG(HTYPE,NNORTH,NSOUTH,PWEST,PEAST,KNUM,KPTS,NOFSET)
C
C     _______________________________________________________
C
C*    Section 3.    Check which version of FFT to use
C     _______________________________________________________
C
 300  CONTINUE
C
      LUSEFFTW = FORCED_FFTW(LOCTAHEDRAL)
      HUSEFFT = 'FFT99'
#ifdef HAVE_FFTW
      IF( LDEBUG .AND. .NOT. LUSEFFTW ) CALL INTLOG(JP_DEBUG,
     X  'JAGGGP: Check which version of FFT to use.',JPQUIET)
      DO LOOP = 1, KNUM !*2 (only half of KPTS/IJPWR needs to be checked)
        IF( LUSEFFTW ) EXIT
        ILON = KPTS(LOOP) * IJPWR(LOOP)
        CALL JJSET99( ATRIGS, IMFAX, ILON, KRET )
        LUSEFFTW = ( KRET .NE. 0 )
      ENDDO
#ifdef REAL_8
      IF( LUSEFFTW ) HUSEFFT = 'DFFTW'
#else
      IF( LUSEFFTW ) HUSEFFT = 'SFFTW'
#endif
#endif
      IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X  'JAGGGP: Will use '//HUSEFFT,JPQUIET)
C
C     _______________________________________________________
C
C*    Section 4.    Process all latitude rows
C     _______________________________________________________
C
 400  CONTINUE
C
      IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X  'JAGGGP: Process latitude rows.',JPQUIET)
C
      NGROUP = 0
C
      DO NXTROW = INORTH, ISOUTH
C
C       Build up a group of lines of the same length which can all
C       be transformed at the same time.
        NGROUP = NGROUP + 1
C
C       Initialise FFT constants for the group
C
        IF ( NGROUP .EQ. 1 ) THEN
          NFIRST = NXTROW
          ILON = KPTS(NFIRST) * IJPWR(NFIRST)
C
          IF( LDEBUG ) THEN
            CALL INTLOG(JP_DEBUG,
     X        'JAGGGP: Initialise FFT consts for the group.',JPQUIET)
            CALL INTLOG(JP_DEBUG,
     X        'JAGGGP: Latitude number = ', NFIRST)
            CALL INTLOG(JP_DEBUG,
     X        'JAGGGP: No. of calculated long.pts = ', ILON)
          ENDIF
C
          IF( .NOT. LUSEFFTW ) THEN
            CALL JJSET99( ATRIGS, IMFAX, ILON, KRET)
            IF( KRET .NE. 0 ) GOTO 990
          ENDIF
        ENDIF
C
C       If the current line is the last to be processed,
C       or the next line is not suitable for the current group,
C       or 32 lines are in the group already ...
C
        IF (     ( NXTROW .EQ. ISOUTH)
     X      .OR. ( KPTS(NFIRST) .NE. KPTS(NXTROW+1) )
     X      .OR. ( NGROUP .EQ. JP32 ) ) THEN
C
C         Calculate the points along the line of latitude
C
          IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X      'JAGGGP: Number in group = ', NGROUP)
C
          CALL JSYMGG( PSHUP, KTRUNC, NFIRST, ILUNIT, NGROUP, ILON,
     X                 ALAT, ALEG, ATRIGS, IMFAX, LUSEFFTW, ZFA, KRET )
          IF( KRET .NE. 0 ) GOTO 990
C
C         If it is a wind component field formed from spectral Ucos(theta)
C         and Vcos(theta) (ie not just spectral U and V), apply a
C         latitude correction
C
          IF( (LUV.EQ.1).AND.(.NOT.LPLAINU) )
     X      CALL JWSCAL(ZFA, NGROUP, ILON, NFIRST, ALAT)
C
C         Store away the generated latitudes
C
          IF( HTYPE.EQ.'R' .OR. HTYPE.EQ.'r' .OR.
     X        HTYPE.EQ.'O' .OR. HTYPE.EQ.'o' .OR.
     X        HTYPE.EQ.'U' .OR. HTYPE.EQ.'u' ) THEN
            NUMPTS = KPTS(NFIRST)
          ELSE
            RINTVL = (PEAST - PWEST)*FLOAT( KPTS(NFIRST) ) / 360.0
            NUMPTS = NINT( RINTVL ) + 1
          ENDIF
C
C         Is the area unequally distributed about the equator?
C
          IF ( NCASE .EQ. 4 .OR. NCASE .EQ. 5 ) THEN
            CALL JMVUGG( ZFA, NNORTH, NSOUTH, PWEST, KNUM, NFIRST,
     X                   NDIFF, NGROUP, NUMPTS, ILON,
     X                   IJPWR(NFIRST), NOFSET, POUTF)
            NDIFF  = MAX( 0, (NDIFF - NGROUP) )
C
C         No, it is all in one hemisphere or is symmetrical
C         about the equator
C
          ELSE
            CALL JMOVGG( ZFA, PWEST, KNUM, NFIRST, NGROUP, NUMPTS,
     X                   ILON, IJPWR(NFIRST), NOFSET, POUTF, NFLAG)
          ENDIF
C
C         Startup next group of latitudes
C
          NGROUP = 0
        ENDIF
C
      ENDDO
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
 990  CONTINUE
#ifdef SHAREDMEMORY
C     note: shareddt return value ignored
      KRET2 = SHAREDDT(IALEG)
      IF( LDEBUG )
     X  CALL INTLOG(JP_DEBUG,'JAGGGP: release_shared_file  = ', KRET2)
#endif

C
      IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'JAGGGP: Return code = ',KRET)
C
      RETURN
      END
