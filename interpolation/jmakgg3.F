C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JMAKGG3( KUNIT, KTRUNC, KROW, PLAT, PLEG, KRET)
C
C---->
C**** JMAKGG
C
C     PURPOSE
C     _______
C
C     This routine creates legendre functions for latitude rows upto
C     the given latitude in a gaussian grid file.
C
C     INTERFACE
C     _________
C
C     CALL JMAKGG( KUNIT, KTRUNC, KROW, PLAT, PLEG, KRET)
C
C     Input parameters
C     ________________
C
C     KUNIT   - Unit number for open file of legendre functions.
C     KTRUNC  - Truncation.
C     KROW    - Latitude row number in the file
C     PLAT    - Array of latitudes for the gaussian grid
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
C     None.
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
C
C     Reference
C     _________
C
C     None.
C
C     Comments
C     ________
C
C     Only positive row numbers are allowed.
C
C     AUTHOR
C     ______
C
C     J.D.Chambers      *ECMWF*      Jan 1994
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
#include "jparams.h"
#include "parim.h"
C
C     Parameters
      INTEGER JPROUTINE
      PARAMETER ( JPROUTINE = 30400 )
C
C     Subroutine arguments
      INTEGER KUNIT, KTRUNC, KROW, KRET
      REAL PLAT, PLEG
      DIMENSION PLAT(*)
      DIMENSION PLEG(*)
C
C     Local variables
      INTEGER NEWPOS, NSIZE, NRET, NROWCT, NLATIT
      REAL ALAT, PIBY2, DEG2RAD
C
C     _______________________________________________________
C
C*    Section 1.    Initialization.
C     _______________________________________________________
C
100   CONTINUE
      IF ( KROW .LT. 0.0 ) THEN
        KRET = JPROUTINE + 1
        CALL INTLOG(JP_ERROR,'JMAKGG3: negative row number given',KROW)
        GOTO 990
      ENDIF
C
C     Calculate the byte offset into the file where the legendre
C     functions for the latitude should be.
#ifdef REAL_8
      NSIZE   = 8 * (KTRUNC+1)*(KTRUNC+4)/2
#else
      NSIZE   = 4 * (KTRUNC+1)*(KTRUNC+4)/2
#endif
      NEWPOS = (KROW - 1) * NSIZE
C
C     Find the current file length
      CALL PBSEEK3( KUNIT, 0, 2, NRET)
      IF ( NRET .LT. 0 ) THEN
        CALL INTLOG(JP_ERROR,'JMAKGG3: PBSEEK error',  NRET)
        KRET = JPROUTINE + 2
        GOTO 990
      ENDIF
C
C     Use the length to determine how many rows have already been
C     put into the file
      NROWCT = NRET/NSIZE
C
C     _______________________________________________________
C
C*    Section 2.    Processing.
C     _______________________________________________________
C
C     Loop through latitudes not in the file yet
      PIBY2   = PPI / 2.0
      DEG2RAD = PPI / 180.0
C
      DO 210 NLATIT = NROWCT+1, KROW
        ALAT = PLAT(NLATIT) * DEG2RAD
        CALL JSPLEG1( PLEG, ALAT, KTRUNC)
        CALL PBWRITE3( KUNIT, PLEG, NSIZE, NRET)
        IF ( NRET .LT. NSIZE ) THEN
          CALL INTLOG(JP_ERROR,'JMAKGG3: PBWRITE error.',  NRET)
          CALL INTLOG(JP_ERROR,'JMAKGG3: bytes required = ',  NSIZE)
          KRET = JPROUTINE + 3
          GOTO 990
        ENDIF
  210 CONTINUE
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

