C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GETCONF( )
C
C---->
C**** GETCONF
C
C     PURPOSE
C     _______
C
C     This routine looks for environment variable CONFIG_INTERP to
C     decide how to handle legendre coefficients.
C
C     INTERFACE
C     _________
C
C     CALL GETCONF( )
C
C     Input parameters
C     ________________
C
C     None.
C
C     Output parameters
C     ________________
C
C     Sets flags in common block JDCFLAGS (see nifld.common):
C
C       LFILEIO, LMEMORY, LSHARED, LON_FLY, LMAPPED
C
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
C     Looks for environment variable CONFIG_INTERP to decide how to
C     handle legendre coefficients. The variable contains entries in
C     the format:
C
C       col 1
C         |
C         v
C         FILEIO   to read coefficents from a file one latitude at a time
C         MEMORY   to read all coefficents from a file into memory
C         SHARED   to read coefficents from a memory-mapped file
C         ON_FLY   to generate coefficents for each latitude 'on the fly'
C
C     Externals
C     _________
C
C     JDEBUG   - Checks environment variable to switch on/off debug
C     INTLOG   - Logs output messages
C     GETENV   - Get value of an environment variable
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
C     J.D.Chambers      ECMWF      Jan 2000
C
C
C     MODIFICATIONS
C     _____________
C
C     None.
C
C----<
C     _______________________________________________________
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
      IMPLICIT NONE
#include "nifld.common"
#include "parim.h"
#include "jparams.h"
C
C     Parameters
C
C     Local variables
C
      CHARACTER*20 CONFIG
      INTEGER IBLANK
C
      LOGICAL LDEBUG
      SAVE LDEBUG
C
C     -----------------------------------------------------------------|
C*    Section 1.    Initialization.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      IF( LIFIRST ) THEN
C
        LIFIRST = .FALSE.
C
        CALL JDEBUG( )
        LDEBUG = NDBG.GT.0
C
C       Read the configuration information
C
        CALL GETENV('CONFIG_INTERP', CONFIG)
        IBLANK = INDEX(CONFIG, ' ')
        IF( IBLANK.GT.6 ) THEN
C
          IF( CONFIG(1:6).EQ.'FILEIO' ) LFILEIO = .TRUE.
          IF( CONFIG(1:6).EQ.'MEMORY' ) LMEMORY = .TRUE.
          IF( CONFIG(1:6).EQ.'SHARED' ) LSHARED = .TRUE.
          IF( CONFIG(1:6).EQ.'ON_FLY' ) LON_FLY = .TRUE.
          IF( CONFIG(1:6).EQ.'MAPPED' ) LMAPPED = .TRUE.
          IF( LDEBUG ) THEN
            CALL INTLOG(JP_DEBUG,
     X      'GETCONF: Legendre coefficents via: '//CONFIG(1:6),JPQUIET)
          ENDIF
        ELSE
#if defined IBM_POWER4
           IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X      'GETCONF: Default legendre coeff handling: MAPPED',JPQUIET)
          LMAPPED = .TRUE.
#else
           IF( LDEBUG ) CALL INTLOG(JP_DEBUG,
     X      'GETCONF: Default legendre coeff handling: FILEIO',JPQUIET)
          LFILEIO = .TRUE.
#endif
        ENDIF
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------|
C
 900  CONTINUE
C
      RETURN
      END
