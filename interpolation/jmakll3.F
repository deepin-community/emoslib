C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JMAKLL3( KUNIT, KTRUNC, PINTVL, PLAT, PLEG, KRET)
C
C---->
C**** JMAKLL
C
C     PURPOSE
C     _______
C
C     This routine creates legendre functions for latitude rows upto
C     the given latitude in a 'non-standard truncation/interval' file.
C
C     INTERFACE
C     _________
C
C     CALL JMAKLL( KUNIT, KTRUNC, PINTVL, PLAT, PLEG, KRET)
C
C     Input parameters
C     ________________
C
C     KUNIT   - Unit number for open file of legendre functions.
C     KTRUNC  - Truncation.
C     PINTVL  - Grid interval (degrees)
C     PLAT    - Latitude in degrees.
C
C     Output parameters
C     ________________
C
C     PLEG     - Array of legendre functions for the latitude.
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
C     Determine which functions already exist for the latitude rows
C     in the file, skip to the end and create necessary extra functions
C     and add to the file.
C
C     Externals
C     _________
C
C     PBSEEK    - Position the coefficients file
C     PBWRITE   - Write the coefficients file
C     INTLOG   - Output log message
C     INTLOGR  - Output log message (with real value)
C
C     Reference
C     _________
C
C     None.
C
C     Comments
C     ________
C
C     Only positive latitude values are allowed.
C
C     AUTHOR
C     ______
C
C     J.D.Chambers      *ECMWF*      Dec 1993
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
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
C*    Prefix conventions for variable names
C
C     Logical      L (but not LP), global or common.
C                  O, dummy argument
C                  G, local variable
C                  LP, parameter.
C     Character    C, global or common.
C                  H, dummy argument
C                  Y (but not YP), local variable
C                  YP, parameter.
C     Integer      M and N, global or common.
C                  K, dummy argument
C                  I, local variable
C                  J (but not JP), loop control
C                  JP, parameter.
C     REAL         A to F and Q to X, global or common.
C                  P (but not PP), dummy argument
C                  Z, local variable
C                  PP, parameter.
C
      IMPLICIT NONE
C
#include "jparams.h"
#include "parim.h"
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER ( JPROUTINE = 30500 )
C
C     Subroutine arguments
C
      INTEGER KUNIT, KTRUNC, KRET
      REAL PLEG, PINTVL, PLAT
      DIMENSION PLEG(*)
C
C     Local variables
C
      INTEGER NEWPOS, NSIZE, NRET, NROWCT, NBYTES
      REAL ZLATEST, ZLAT, ALAT, PIBY2, DEG2RAD
      INTEGER NEXT, FIRST, LAST
C
C     _______________________________________________________
C
C*    Section 1.    Initialization.
C     _______________________________________________________
C
 100  CONTINUE
C
      IF( NDBG.GT.1 ) THEN
        CALL INTLOG(JP_DEBUG,'JMAKLL3: Input parameters:',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'JMAKLL3: Unit number for leg.funcs file =', KUNIT)
        CALL INTLOG(JP_DEBUG,'JMAKLL3: Truncation = ', KTRUNC)
        CALL INTLOGR(JP_DEBUG,
     X    'JMAKLL3: Grid interval (degrees) = ', PINTVL)
        CALL INTLOGR(JP_DEBUG,
     X    'JMAKLL3: Latitude in degrees = ', PLAT)
      ENDIF
C
      IF( PLAT.LT.0.0 ) THEN
        ZLAT = 0.0
      ELSE
        ZLAT = PLAT
      ENDIF
C
C     Calculate the byte offset into the file where the legendre
C     functions for the latitude should be.
C
#ifdef REAL_8
      NSIZE   = 8 * (KTRUNC+1)*(KTRUNC+4)/2
#else
      NSIZE   = 4 * (KTRUNC+1)*(KTRUNC+4)/2
#endif
      NEWPOS = NINT( (90.0 - ZLAT)/PINTVL ) * NSIZE
C
C     Find the current file length
C
      CALL PBSEEK3( KUNIT, 0, 2, NRET)
      IF( NRET.LT.0 ) THEN
        CALL INTLOG(JP_ERROR,'JMAKLL3: PBSEEK error',NRET)
        KRET = JPROUTINE + 2
        GOTO 990
      ENDIF
C
C     Use the length to determine how many rows have already been
C     put into the file
C
      NROWCT = NRET/NSIZE
      ZLATEST  = 90.0 - REAL(NROWCT) * PINTVL
C
      IF( NDBG.GT.1 ) THen
        CALL INTLOG(JP_DEBUG,'JMAKLL3: File length on open = ', NRET)
        CALL INTLOG(JP_DEBUG,
     X    'JMAKLL3: Number of rows already in file = ', NROWCT)
        CALL INTLOGR(JP_DEBUG,
     X    'JMAKLL3: Next latitude to go in file = ', ZLATEST)
      ENDIF
C
C     _______________________________________________________
C
C*    Section 2.    Processing.
C     _______________________________________________________
C
C     Loop through latitudes not in the file yet
C
#ifdef REAL_8
      NBYTES = (KTRUNC+1)*(KTRUNC+4)/2  * 8
#else
      NBYTES = (KTRUNC+1)*(KTRUNC+4)/2  * 4
#endif
C
      IF( NDBG.GT.1 )
     X  CALL INTLOG(JP_DEBUG,'JMAKLL3: No. of bytes per row = ', NBYTES)
C
      PIBY2   = PPI / 2.0
      DEG2RAD = PPI / 180.0
C
      FIRST = NINT(ZLATEST/PINTVL)
      LAST  = NINT(ZLAT/PINTVL)
      DO NEXT = FIRST, LAST, -1
        ALAT = (NEXT*PINTVL) * DEG2RAD
C
        IF( NDBG.GT.1 ) CALL INTLOGR(JP_DEBUG,
     X    'JMAKLL3: Next latitude constructed = ', (NEXT*PINTVL))
C
        CALL JSPLEG1( PLEG, ALAT, KTRUNC)
        CALL PBWRITE3( KUNIT, PLEG, NBYTES, NRET)
        IF( NRET.LT.NSIZE ) THEN
          CALL INTLOG(JP_ERROR,'JMAKLL3: PBWRITE error.', NRET)
          CALL INTLOG(JP_ERROR,'JMAKLL3: bytes required = ', NSIZE)
          KRET = JPROUTINE + 3
          GOTO 990
        ENDIF
      ENDDO
C     _______________________________________________________
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
 900  CONTINUE
      KRET = 0
C
 990  CONTINUE
      RETURN
      END

