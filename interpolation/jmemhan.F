C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JMEMHAN( KFLAG, KZOUTBF, KSIZE, KOPT, KRET)
C
C---->
C**** JMEMHAN
C
C     PURPOSE
C     _______
C
C     This routine handles memory allocation for jintll.F and jintgg.F
C
C     INTERFACE
C     _________
C
C     CALL JMEMHAN( KFLAG, KZOUTBF, KSIZE, KOPT, KRET)
C
C     Input parameters
C     ________________
C
C     KFLAG   - Flag to select lat/long or gaussian allocation
C               = 1  for grid to grid interpolation (igalloc)
C               = 2  for grid to grid interpolation (igalloc)
C               = 3  for scratch space
C               = 4  for vorticity and divergence scratch space
C               = 5  for even more scratch space (!?)
C               = 6  for latitude/longitude interpolation coefficients
C                    on Fujitsu
C               = 7  for gaussian interpolation coefficients on Fujitsu
C               = 8  unused
C               = 9  for FFT work space
C               = 10 for output (partial) grid point field workspace
C               = 11 for work space for rotating fields (see intfap.F)
C               = 12 for raw land-sea mask (see iglsmst.F)
C               = 13 for 10 minute land-sea mask (see iglsm01.F)
C               = 14 for reading legendre coefficents line by line
C               = 15 more work space for rotating fields (see intfap.F)
C               = 16 more work space intfbu.F
C               = 17 more work space ??
C               = 18 for rotation of gaussian grids (ggrotat and tatorgg)
C               = 19 for dynamic allocation of znfeldi (intf)
C               = 20 to 22, coefficients for rotating spectral fields (jacobif)
C
C     KSIZE   - Size of memory required in words
C     KOPT    - Memory option
C               = 0 to deallocate
C               = 1 to allocate
C
C
C     Output parameters
C     ________________
C
C     KZOUTBF - Pointer to memory (if allocating)
C     KRET    - Return status code
C               0 = OK
C
C
C     Common block usage
C     __________________
C
C     JDCNDGB
C     JMEMCOM
C
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
C     JDEBUG    - Checks environment variable to switch on/off debug
C     JMALLOC   - Allocate heap space
C     JFREE     - Deallocate heap space
C     INTLOG    - Output log message
C     IAFREE    - Deallocate heap space used by grid-pt to grid-pt
C                 interpolation
C
C
C     Reference
C     _________
C
C     None.
C
C
C     Comments
C     ________
C
C     None.
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers      *ECMWF*      May 1994
C
C     MODIFICATIONS
C     _____________
C
C     J.D.Chambers      *ECMWF*      Jan 1995
C     Add scratch for vorticity and divergence (flag = 4)
C
C     J.D.Chambers      *ECMWF*      Jan 1995
C     Add space for interpolation coefficents (flag = 6)
C
C     J.D.Chambers      *ECMWF*      Sep 1996
C     Split space for interpolation coefficents (flag = 6 and 7)
C
C     J.D.Chambers     ECMWF        Feb 1997
C     Allow for 64-bit pointers
C
C     J.D.Chambers     ECMWF        Mar 1998
C     Allow memory flushing
C
C----<
C     -------------------------------------------------------
C*    Section 0. Definition of variables.
C     -------------------------------------------------------
C
      IMPLICIT NONE
#include "jparams.h"
#include "jparam2.h"
#include "parim.h"
C
C     Parameters
C
      INTEGER JPROUTINE, JPOPTMX
      PARAMETER ( JPROUTINE = 30600 )
      PARAMETER ( JPOPTMX = 22 )
C
C     Subroutine arguments
C
#ifdef POINTER_64
      INTEGER*8 KZOUTBF
#else
      INTEGER KZOUTBF
#endif
      INTEGER KSIZE, KOPT, KRET, KFLAG
C
C     Local variables
C
      LOGICAL LDEBUG, LDEBUG1
      INTEGER ITOTAL, LOOP
#ifdef POINTER_64
      INTEGER*8 IZOUTBF
#else
      INTEGER IZOUTBF
#endif
      INTEGER IEXIST
      DIMENSION IEXIST(JPOPTMX)
      DIMENSION IZOUTBF(JPOPTMX)
      DATA IEXIST/ JPOPTMX * -1/
      DATA IZOUTBF/ JPOPTMX * 0/
C
      SAVE IEXIST, IZOUTBF
C
C     Externals
C
#ifdef POINTER_64
      INTEGER*8 JMALLOC
#else
      INTEGER JMALLOC
#endif
      INTEGER IAFREE
      EXTERNAL JMALLOC, IAFREE
C
C     -------------------------------------------------------
C*    Section 1.    Initialisation.
C     -------------------------------------------------------
C
  100 CONTINUE
C
      KRET = 0
      CALL JDEBUG( )
      LDEBUG  = ( NDBG.GT.0 )
      LDEBUG1 = ( NDBG.EQ.1 )
C
C     Check that a  valid option has been chosen
C
      IF( ( KFLAG.LT.1 ).OR.( KFLAG.GT.JPOPTMX ) ) THEN
        CALL INTLOG(JP_ERROR,'JMEMHAN: Invalid flag = ', KFLAG)
        KRET = JPROUTINE + 1
        GOTO 900
      ENDIF
C
C     Display diagnostics if required
C
      IF( LDEBUG ) THEN
C
        CALL INTLOG(JP_DEBUG,'JMEMHAN: Input parameters:',JPQUIET)
        CALL INTLOG(JP_DEBUG, 'JMEMHAN: Memory type flag = ', KFLAG)
C
        IF( ( KFLAG.EQ.1 ).OR.( KFLAG.EQ.2 ) ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 1 = grid to grid interpolation', JPQUIET)
C
        IF( ( KFLAG.EQ.3 ).OR.( KFLAG.EQ.4 ).OR.( KFLAG.EQ.5 ) )
     X    CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 3->5 = scratch,',JPQUIET)
C
        IF( KFLAG.EQ.6 ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 6 = coeffs for spectral to lat/long interp',JPQUIET)
C
        IF( KFLAG.EQ.7 ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 7 = coefs for spectral to gaussian interp,',JPQUIET)
C
        IF( KFLAG.EQ.8 ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 8 = coefficients for rotating SH fields',JPQUIET)
C
        IF( KFLAG.EQ.9 ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 9 = FFT workspace,',JPQUIET)
C
        IF( KFLAG.EQ.10 ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 10 = output (partial) grid pt field,',JPQUIET)
C
        IF( KFLAG.EQ.11 ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 11 = work space for rotating fields,',JPQUIET)
C
        IF( KFLAG.EQ.12 ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 12 = raw land-sea mask,',JPQUIET)
C
        IF( KFLAG.EQ.13 ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 13 = 10 minute land-sea mask.',JPQUIET)
C
        IF( KFLAG.EQ.14 ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 14 = legendre coefficents line by line.',JPQUIET)
C
        IF( KFLAG.EQ.15 ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 15 = more work space for rotating fields.',JPQUIET)
C
        IF( KFLAG.EQ.16 ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 16 = more work space for intfbu.F.',JPQUIET)
C
        IF( KFLAG.EQ.17 ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 17 = more work space for ??.',JPQUIET)
C
        IF( KFLAG.EQ.18 ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 18 = space for rotating gaussian fields.',JPQUIET)
C
        IF( KFLAG.EQ.19 ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 19 = for dynamic allocation of znfeldi.',JPQUIET)
C
        IF( KFLAG.EQ.20 ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 20 = coefficients for rotating SH fields',JPQUIET)
C
        IF( KFLAG.EQ.21 ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 21 = coefficients for rotating SH fields',JPQUIET)
C
        IF( KFLAG.EQ.22 ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: 22 = coefficients for rotating SH fields',JPQUIET)
C
        CALL INTLOG(JP_DEBUG,'JMEMHAN: Memory reqd in words = ', KSIZE)
        CALL INTLOG(JP_DEBUG,'JMEMHAN: Option (1 = allocate) = ', KOPT)
      ENDIF
C
C     -------------------------------------------------------
C*    Section 2.    Allocate memory
C     -------------------------------------------------------
C
  200 CONTINUE
C
      IF( KOPT.EQ.1 ) THEN
C
        IF( LDEBUG ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'JMEMHAN: Requested allocation = ', KSIZE)
          CALL INTLOG(JP_DEBUG,
     X      'JMEMHAN: Previous allocation  = ', IEXIST(KFLAG))
        ENDIF
C
C       See if more memory required than already allocated
C
        IF( KSIZE.GT.IEXIST(KFLAG) ) THEN
C
C         Special cases (sigh)
C
C         If allocating memory for spectral to lat/long or gaussian
C         interpolation, first deallocate heap space used by grid-pt
C         to grid-pt interpolation.
C
          IF( ( KFLAG.EQ.1 ).OR.( KFLAG.EQ.2 ) ) THEN
            IF( LDEBUG ) THEN
              KRET = IAFREE(1,1)
            ELSE
              KRET = IAFREE(0,-1)
            ENDIF
          ENDIF
C
C         If allocating memory on VPP for spectral to grid interpolation
C         coefficients, first deallocate memory used by other type of
C         spectral to gaussian interpolation if this option has been
C         requested.
C
          IF( LFREECF ) THEN
            IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X        'JMEMHAN: Free coefficients option exercised', JPQUIET)
C
C           Spectral -> lat/long
C
            IF( KFLAG.EQ.6 ) THEN
              IF( IEXIST(7).NE.-1 ) THEN
                IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X            'JMEMHAN: Free spect->gaussn coeff memory',IZOUTBF(7))
                CALL JFREE( IZOUTBF(7) )
                IZOUTBF(7) = 0
                IEXIST(7) = -1
                NISIZE7 = 0
                YOLDGG = 'xxxxxxxxxxxxxxxxxxxx'
              ENDIF
            ENDIF
C
C           Spectral -> gaussian
C
            IF( KFLAG.EQ.7 ) THEN
              IF( IEXIST(6).NE.-1 ) THEN
                IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X            'JMEMHAN: Free spec->lat/lon coeff memory',IZOUTBF(6))
                CALL JFREE( IZOUTBF(6) )
                IZOUTBF(6) = 0
                IEXIST(6) = -1
                NISIZE6 = 0
                YOLDLL = 'xxxxxxxxxxxxxxxxxxxx'
              ENDIF
            ENDIF
          ENDIF
C
          IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X      'JMEMHAN: Request greater than previous allocation', KSIZE)
C
C         If memory already allocated, deallocate existing memory
C
          IF( IEXIST(KFLAG).GE.0 ) THEN
C
            IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X          'JMEMHAN: Deallocate existing memory',IEXIST(KFLAG))
            CALL JFREE( IZOUTBF(KFLAG) )
          ENDIF
C
C         Allocate heap memory
C
          IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X      'JMEMHAN: Allocate new memory ',KSIZE)
C
          IEXIST(KFLAG) = KSIZE
#if (defined REAL_8)
          IZOUTBF(KFLAG) = JMALLOC( IEXIST(KFLAG) * 8 )
#else
          IZOUTBF(KFLAG) = JMALLOC( IEXIST(KFLAG) * 4 )
#endif
#ifdef hpR64
          IZOUTBF(KFLAG) = IZOUTBF(KFLAG)/(1024*1024*1024*4)
#endif
          IF( IZOUTBF(KFLAG).EQ.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'JMEMHAN: Memory allocation failed',IZOUTBF(KFLAG))
            IEXIST(KFLAG) = -1
            KRET = JPROUTINE + 5
            GOTO 900
          ENDIF
        ENDIF
C
        IF( LFREECF ) THEN
C
C         Spectral -> lat/long
C
          IF( KFLAG.EQ.6 ) NISIZE6 = IEXIST(6)
C
C         Spectral -> gaussian
C
          IF( KFLAG.EQ.7 ) NISIZE7 = IEXIST(7)
C
        ENDIF
C
C     -------------------------------------------------------
C*    Section 3.    Deallocate memory
C     -------------------------------------------------------
C
  300 CONTINUE
C
      ELSE
C
        IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X    'JMEMHAN: Deallocate memory address = ',IZOUTBF(KFLAG))
C
        IF( IZOUTBF(KFLAG).GT.0 ) THEN
C
          CALL JFREE( IZOUTBF(KFLAG) )
          IZOUTBF(KFLAG) = 0
          IEXIST(KFLAG) = -1
C
        ELSE
          CALL INTLOG(JP_DEBUG,'JMEMHAN: Deallocation ignored',JPQUIET)
        ENDIF
      ENDIF
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine.
C     _______________________________________________________
C
 900  CONTINUE
      IF( KRET.EQ.0 ) KZOUTBF = IZOUTBF(KFLAG)
C
      IF( LDEBUG.AND.(.NOT.LDEBUG1) ) THEN
        ITOTAL = 0
        DO LOOP = 1, JPOPTMX
          IF( IEXIST(LOOP).GT.0 ) THEN
            CALL INTLOG(JP_DEBUG, 'JMEMHAN: For type ', LOOP)
#if (defined REAL_8)
            CALL INTLOG(JP_DEBUG,
     X        'JMEMHAN: bytes allocated = ', IEXIST(LOOP)*8 )
            ITOTAL = ITOTAL + (IEXIST(LOOP) * 8)
#else
            CALL INTLOG(JP_DEBUG,
     X        'JMEMHAN: bytes allocated = ', IEXIST(LOOP)*4 )
            ITOTAL = ITOTAL + (IEXIST(LOOP) * 4)
#endif
          ENDIF
        ENDDO
        CALL INTLOG(JP_DEBUG,'JMEMHAN: Total bytes allocated=', ITOTAL)
        CALL INTLOG(JP_DEBUG,'JMEMHAN: Return status code = ', KRET)
      ENDIF
C
      RETURN
      END
