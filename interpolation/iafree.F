C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IAFREE (KPR, KERR)
C
C---->
C**** *IAFREE*
C
C     PURPOSE
C     _______
C
C     Deallocate heap space used by grid-pt to grid-pt interpolation.
C
C     INTERFACE
C     _________
C
C     IERR = IAFREE (KPR, KERR)
C
C     Input parameters
C     ________________
C
C     KPR        - The debug print switch.
C                  0  , No debugging output.
C                  1  , Produce debugging output.
C
C     KERR       - The error control flag.
C                  -ve, No error message. Return error code.
C                  0  , Hard failure with error message.
C                  +ve, Print error message. Return error code.
C
C     Output parameters
C     ________________
C
C     None
C
C     Return value
C     ____________
C
C     The error indicator (INTEGER).
C
C     Error and Warning Return Values
C     _______________________________
C
C     Cray error codes
C
C     -1     HPDEALLC block is already free (Warning only).
C
C     21201  HPDEALLC attempt to free block at address outside the
C            bounds of the heap.
C     21202  HPDEALLC attempt to free block at address which was not
C            the beginning of a block.
C     21203  HPDEALLC returning any other non zero code.
C
C     Errors 21201 and 21202 should not occur and any occurrence
C     of one of these errors should be reported to Meteorological
C     Applications.
C
C     Common block usage
C     __________________
C
C     memreq.h     - This file contains the memory request
C                    definition variables.
C
C     MADDR        - The base addresses of the currently allocated
C                    memory segments used and reset.
C     MREQUEST     - The sizes of the current memory requests used
C                    and reset.
C
C
C     EXTERNALS
C     _________
C
C     INTLOG    - Logs messages.
C
C     Cray
C
C     ABORTX    - Standard routine to kill task.
C     HPDEALLC  - Cray library routine to deallocate heap space.
C
C     Sun and SGI
C
C     JFREE     - "Unix" routine to deallocate heap space.
C
C     METHOD
C     ______
C
C     The required heap space is allocated.
C
C     REFERENCE
C     _________
C
C     None
C
C     COMMENTS
C     ________
C
C     None.
C
C     AUTHOR
C     ______
C
C     K. Fielding      *ECMWF*      May 1994
C
C     MODIFICATIONS
C     _____________
C
C     J.D.Chambers      ECMWF       Aug 1994
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
C     Implicit statement to force declarations
C
      IMPLICIT NONE
C
#include "parim.h"
#include "memreq.h"
#include "nifld.common"
#include "nofld.common"
C
C     Dummy arguments
C
      INTEGER KPR, KERR
C
C     Local variables
C
      INTEGER IABORT, IERROR
C
      INTEGER JLEVEL
C
      INTEGER JPROUTINE
C
      PARAMETER (JPROUTINE = 21200)
C
C     External functions
C
C     _______________________________________________________
C
C
C*    Section 1. Initialisation
C     _______________________________________________________
C
  100 CONTINUE
C
      IF (KPR .GE. 1) CALL INTLOG(JP_DEBUG,'IAFREE: Section 1.',JPQUIET)
C
      IAFREE = 0
C
      IF (KPR .GE. 1) CALL INTLOG(JP_DEBUG,
     X 'IAFREE: Free all heap space.',JPQUIET)
C
C     _______________________________________________________
C
C
C*    Section 2. Free acquired space.
C     _______________________________________________________
C
  200 CONTINUE
C
      IF (KPR .GE. 1) CALL INTLOG(JP_DEBUG,'IAFREE: Section 2.',JPQUIET)
C
      IABORT = 0
      IERROR = 0
C
      DO 210 JLEVEL = JPLEVEL, 1, -1
C
         IF (MREQUEST (JLEVEL) .GT. 0) THEN
C
#ifdef CRAY
            CALL HPDEALLC (MADDR (JLEVEL), IERROR, IABORT)
C
            IF (IERROR .EQ. -4) THEN
              IAFREE = - 1
              IF (KERR .GE. 0) CALL INTLOG(JP_ERROR,
     X          'IAFREE: Already free, the block at ', MADDR (JLEVEL))
C
            ELSE IF (IERROR .NE. 0) THEN
C
              IF (IERROR .EQ. -3) THEN
C
                IAFREE = JPROUTINE + 1
C
                IF (KERR .GE. 0) THEN
                  CALL INTLOG(JP_ERROR,
     X              'IAFREE: Heap deallocation failed.',IERROR)
                  CALL INTLOG(JP_ERROR,
     X              'IAFREE: Block outside heap.',MADDR(JLEVEL))
                ENDIF
              ELSE IF (IERROR .EQ. -5) THEN
C
                IAFREE = JPROUTINE + 2
C
                IF (KERR .GE. 0) THEN
                  CALL INTLOG(JP_ERROR,
     X              'IAFREE: Heap deallocation failed.',IERROR)
                  CALL INTLOG(JP_ERROR,
     X              'IAFREE: Not at start of block.',MADDR(JLEVEL))
                ENDIF
              ELSE
C
                IAFREE = JPROUTINE + 3
C
                IF (KERR .GE. 0) THEN
                  CALL INTLOG(JP_ERROR,
     X              'IAFREE: Heap deallocation failed.',IERROR)
                  CALL INTLOG(JP_ERROR,
     X              'IAFREE: Attempt to free block at: ',MADDR(JLEVEL))
                ENDIF
              ENDIF
C
              IF (KERR .EQ. 0) CALL ABORTX ('IAFREE')
              GO TO 900
            ENDIF
#else
            CALL JFREE (MADDR (JLEVEL) )
#endif
C
            MREQUEST (JLEVEL) = 0
            MADDR (JLEVEL) = 0
C
         ENDIF
C
  210 CONTINUE
C
C     _______________________________________________________
C
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
  900 CONTINUE
C
      IF (KPR .GE. 1) CALL INTLOG(JP_DEBUG,'IAFREE: Section 9.',JPQUIET)
C
      RETURN
      END
