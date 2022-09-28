C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE NMAKLL( KTRUNC, PINTVL, PLAT, KNUM, PLEG, KRET)
C
C---->
C**** NMAKLL
C
C     PURPOSE
C     _______
C
C     This routine creates legendre functions for latitude rows from
C     the given latitude.
C
C     INTERFACE
C     _________
C
C     CALL NMAKLL( KTRUNC, PINTVL, PLAT, KNUM, PLEG, KRET)
C
C     Input parameters
C     ________________
C
C     KTRUNC  - Truncation.
C     PINTVL  - Grid interval (degrees)
C     PLAT    - Start latitude in degrees.
C     KNUM    - Number of latitudes
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
C     See below.
C
C     Externals
C     _________
C
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
C     None at present.
C
C     AUTHOR
C     ______
C
C     J.D.Chambers      *ECMWF*      June 1999
C
C     MODIFICATIONS
C     _____________
C
C     None.
C
C----<
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
      INTEGER KTRUNC, KRET, KNUM
      REAL PLEG, PINTVL, PLAT
      DIMENSION PLEG(*)
C
C     Local variables
C
      REAL ZLAST, ZLAT, ALAT, DEG2RAD
      INTEGER NEXTRW, NEXT, FIRST, LAST
C
C     _______________________________________________________
C
C*    Section 1.    Initialization.
C     _______________________________________________________
C
 100  CONTINUE
C
      IF( NDBG.GT.1 ) THEN
        CALL INTLOG(JP_DEBUG,'NMAKLL: Input parameters:',JPQUIET)
        CALL INTLOG(JP_DEBUG,'NMAKLL: Truncation = ', KTRUNC)
        CALL INTLOG(JP_DEBUG,'NMAKLL: Number of latitudes = ', KNUM)
        CALL INTLOGR(JP_DEBUG,
     X    'NMAKLL: Grid interval (degrees) = ', PINTVL)
        CALL INTLOGR(JP_DEBUG,
     X    'NMAKLL: Latitude in degrees = ', PLAT)
      ENDIF
C
      IF( PLAT.LT.0.0 ) THEN
        ZLAT = 0.0
      ELSE
        ZLAT = PLAT
      ENDIF
C
C     _______________________________________________________
C
C*    Section 2.    Processing.
C     _______________________________________________________
C
      DEG2RAD = PPI / 180.0
C
      ZLAST = ZLAT - (KNUM-1)*PINTVL
      NEXTRW = 1
      FIRST = NINT(ZLAT/PINTVL)
      LAST  = NINT(ZLAST/PINTVL)
      DO NEXT = FIRST, LAST, -1
        ALAT = (NEXT*PINTVL) * DEG2RAD
C
        IF( NDBG.GT.1 ) CALL INTLOGR(JP_DEBUG,
     X    'NMAKLL: Next latitude constructed = ', (NEXT*PINTVL))
C
        CALL JSPLEG1( PLEG(NEXTRW), ALAT, KTRUNC)
        NEXTRW = NEXTRW + (KTRUNC+1)*(KTRUNC+4)/2
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
