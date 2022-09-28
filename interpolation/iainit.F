C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IAINIT (KPR, KERR)
C
C---->
C**** *IAINIT*
C
C     PURPOSE
C     _______
C
C     Initialise common decks for grid-pt to grid-pt interpolation.
C
C     INTERFACE
C     _________
C
C     IERR = IAINIT (KPR, KERR)
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
C     None
C
C     Common block usage
C     __________________
C
C     grfixed.h    - The include file contains all the array space
C                    for grid to grid interpolation.
C
C     MILLEN       - The array of latitude line lengths for a quasi
C                    regular Gaussian grid is set.
C
C     memreq.h     - This file contains the memory request
C                    definition variables.
C
C     MADDR        - The base addresses of the currently allocated
C                    memory segments are set.
C     MREQUEST     - The sizes of the current memory requests are set.
C
C
C     EXTERNALS
C     _________
C
C     INTLOG   - Logs messages.
C
C     METHOD
C     ______
C
C     Initialise common decks for grid-pt to grid-pt interpolation:
C                - memory access arrays,
C                - quasi regular definition.
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
C     K. Fielding      *ECMWF*      Apr 1994
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
#include "grfixed.h"
C
C     Dummy arguments
C
      INTEGER KPR, KERR
C
C     Local variables
C
      INTEGER JSET
C
      INTEGER JPROUTINE
C
      PARAMETER (JPROUTINE = 21100)
C
C     External functions
C
C     Transform definition variables that must be preserved
C
C
C     _______________________________________________________
C
C
C*    Section 1. Initialisation
C     _______________________________________________________
C
  100 CONTINUE
C
      IF (KPR .GE. 1) CALL INTLOG(JP_DEBUG,'IAINIT: Section 1.',JPQUIET)
C
      IAINIT = 0
C
      IF (KPR .GE. 1) CALL INTLOG(JP_DEBUG,
     X  'IAINIT : Grid-pt to grid-pt common decks initialised',JPQUIET)
C
C     _______________________________________________________
C
C
C*    Section 2. Initialise memory access arrays.
C     _______________________________________________________
C
  200 CONTINUE
C
      IF (KPR .GE. 1) CALL INTLOG(JP_DEBUG,'IAINIT: Section 2.',JPQUIET)
C
      DO 210 JSET = 1, JPLEVEL
         MREQUEST (JSET) = 0
         MADDR (JSET) = 0
  210 CONTINUE
C
C     _______________________________________________________
C
C
C*    Section 3. Initialise quasi regular definition
C     _______________________________________________________
C
  300 CONTINUE
C
      IF (KPR .GE. 1) CALL INTLOG(JP_DEBUG,'IAINIT: Section 3.',JPQUIET)
C
      DO 310 JSET = 1, JPLAT
         MILLEN (JSET) = 0
  310 CONTINUE
C
C     _______________________________________________________
C
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
  900 CONTINUE
C
      IF (KPR .GE. 1) CALL INTLOG(JP_DEBUG,'IAINIT: Section 9.',JPQUIET)
C
      RETURN
      END
