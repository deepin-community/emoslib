C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE NMAKGG( KTRUNC, KNEXT, PLAT, KNUM, PLEG, KRET)
C
C---->
C**** NMAKGG
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
C     CALL NMAKGG( KTRUNC, KNEXT, PLAT, KNUM, PLEG, KRET)
C
C     Input parameters
C     ________________
C
C     KTRUNC  - Truncation.
C     KNEXT   - Index (in PLAT) of next latitude at which to start
C     PLAT    - Array of gaussian latitudes
C     KNUM    - Number of latitudes to produce
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
      INTEGER KTRUNC, KNEXT, KRET, KNUM
      REAL PLEG, PLAT
      DIMENSION PLEG(*), PLAT(*)
C
C     Local variables
C
      INTEGER NEXT, ILAT
      REAL ALAT, DEG2RAD
C
C     _______________________________________________________
C
C*    Section 1.    Initialization.
C     _______________________________________________________
C
 100  CONTINUE
C
      IF ( NDBG .GT. 1) THEN
        CALL INTLOG(JP_DEBUG,'NMAKGG: Input parameters:',JPQUIET)
        CALL INTLOG(JP_DEBUG,'NMAKGG: Truncation = ', KTRUNC)
        CALL INTLOG(JP_DEBUG,'NMAKGG: Number of latitudes = ', KNUM)
        CALL INTLOG(JP_DEBUG, 'NMAKGG: Next latitude = ', KNEXT)
        CALL INTLOGR(JP_DEBUG, 'NMAKGG: Lat.in degrees = ', PLAT(KNEXT))
      ENDIF
C
C     _______________________________________________________
C
C*    Section 2.    Processing.
C     _______________________________________________________
C
      DEG2RAD = PPI / 180.0
C
      NEXT = 1
      DO ILAT = 1, KNUM
        ALAT = PLAT(KNEXT+ILAT-1) * DEG2RAD
C
        IF ( NDBG .GT. 1) CALL INTLOGR(JP_DEBUG,
     X    'NMAKGG: Next latitude constructed = ', ALAT)
C
        CALL JSPLEG1( PLEG(NEXT), ALAT, KTRUNC)
        NEXT = NEXT + (KTRUNC+1)*(KTRUNC+4)/2
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
