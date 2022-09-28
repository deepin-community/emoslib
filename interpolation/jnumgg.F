C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JNUMGG( KNUM, HTYPE, KPTS, KRET)
C
C---->
C**** JNUMGG
C
C     PURPOSE
C     _______
C
C     This routine returns the number of points in each latitude of
C     a gaussian grid.
C
C
C     INTERFACE
C     _________
C
C     CALL JNUMGG( KNUM, HTYPE, KPTS, KRET)
C
C
C     Input parameters
C     ________________
C
C     KNUM     - Gaussian grid number
C     HTYPE    - Gaussian grid type
C                = 'R' for reduced ("quasi-regular"),
C                = 'O' for reduced/octahedral,
C                = 'F' for full,
C                = 'U' for user-defined reduced gaussian grid
C
C
C     Output parameters
C     ________________
C
C     KPTS    - Array giving number of points at each gaussian latitude
C               (both hemispheres)
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
C     Simplified interface to JGETGG.
C
C
C     Externals
C     _________
C
C     JGETGG     - Get the gaussian grid definition.
C     INTLOG   - Output log message
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
C     J.D.Chambers      *ECMWF*      Feb 1994
C
C
C     MODIFICATIONS
C     _____________
C
C     None.
C
C----<
C
      IMPLICIT NONE
#include "jparams.h"
#include "parim.h"
C
C     Subroutine arguments
C
      INTEGER KNUM, KPTS, KRET
      CHARACTER*1 HTYPE
      DIMENSION KPTS(*)
C
C     Parameters
      INTEGER JPROUTINE
      PARAMETER ( JPROUTINE = 30700 )
C
C     Local variables
C
      REAL ZLAT
      DIMENSION ZLAT(JPMAX)
C     _______________________________________________________
C
C*    Section 1.    Get the information.
C     _______________________________________________________
C
  100 CONTINUE
C
      KRET = 0
C
C     Make sure local array is big enough for return values.
C
      IF( KNUM .GT. (JPMAX/2) ) THEN
        CALL INTLOG(JP_ERROR,
     X    'JNUMGG - Local array too small for return values.',JPMAX/2)
        KRET = JPROUTINE + 1
        GOTO 900
      ENDIF
C
      CALL JGETGG(KNUM, HTYPE, ZLAT, KPTS, KRET)
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
