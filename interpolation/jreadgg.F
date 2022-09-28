C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JREADGG( KUNIT, KTRUNC, KROW, PLEG, KRET)
C
C---->
C**** JREADGG
C
C     PURPOSE
C     _______
C
C     This routine reads legendre functions for one latitude.
C
C     INTERFACE
C     _________
C
C     CALL JREADGG( KUNIT, KTRUNC, KROW, PLEG, KRET)
C
C     Input parameters
C     ________________
C
C     KUNIT   - Unit number for open file of legendre functions.
C     KTRUNC  - Truncation.
C     KROW    - Latitude row number in the file
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
C     Calculate offset of the legendre functions for the latitude row
C     in the file, skip to the row and read the functions.
C
C     Retries the read upto 3 times if necessary
C
C     Externals
C     _________
C
C     PBSEEK    - Position the coefficients file
C     PBREAD    - Read the coefficients file
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
      IMPLICIT NONE
#include "jparams.h"
#include "parim.h"
C
C     Subroutine arguments
C
      INTEGER KUNIT, KTRUNC, KROW, KRET
      REAL PLEG
      DIMENSION PLEG(*)
C
C     Parameters
      INTEGER JPROUTINE
      PARAMETER ( JPROUTINE = 31000 )
C
C     Local variables
C
      INTEGER NTRIES
      INTEGER   NRET, NSIZE
      INTEGER*8 FRET, FSIZE
      INTEGER*8 NEWPOS, OLDPOS

      DATA OLDPOS/-1/
      SAVE NEWPOS, OLDPOS
C
C     _______________________________________________________
C
C*    Section 1.    Initialization.
C     _______________________________________________________
C
100   CONTINUE
      IF ( KROW .LT. 0 ) THEN
        CALL INTLOG(JP_ERROR,'JREADGG: negative row number given',KROW)
        KRET = JPROUTINE + 1
        GOTO 990
      ENDIF
C
C     Calculate the byte offset into the file where the legendre
C     functions for the latitude should be.
#ifdef REAL_8
      NSIZE   = 8 * (KTRUNC+1)*(KTRUNC+4)/2
      FSIZE   = 8 * (KTRUNC+1)*(KTRUNC+4)/2
#else
      NSIZE   = 4 * (KTRUNC+1)*(KTRUNC+4)/2
      FSIZE   = 4 * (KTRUNC+1)*(KTRUNC+4)/2
#endif
cs      NEWPOS = (KROW - 1) * NSIZE
      NEWPOS = (KROW - 1) * FSIZE
C
C     _______________________________________________________
C
C*    Section 2.    Processing.
C     _______________________________________________________
C
C     Position file unless the previous read left it in the correct
C     position already
C
      NTRIES = 0
 210  CONTINUE
      NTRIES = NTRIES + 1
      IF ( NEWPOS .NE. OLDPOS ) THEN
        CALL PBSEEK64( KUNIT, NEWPOS, 0, FRET)
        IF ( FRET .NE. NEWPOS ) THEN
          CALL INTLOG(JP_ERROR,'JREADGG: PBSEEK64 error.',FRET)
          KRET = JPROUTINE + 2
          GOTO 990
        ENDIF
      ENDIF
C
C     Read the legendre coefficients
      CALL PBREAD( KUNIT, PLEG, NSIZE, NRET)
      IF ( NRET .NE. NSIZE ) THEN
        IF ( NTRIES .EQ. JPMXTRY ) THEN
          CALL INTLOG(JP_ERROR,'JREADGG: PBREAD error.',NRET)
          KRET = JPROUTINE + 3
          GOTO 990
        ELSE
          GOTO 210
        ENDIF
      ENDIF
C
C     Record the current file byte position
      OLDPOS = NEWPOS + NSIZE
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
