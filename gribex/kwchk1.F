C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE KWCHK1(KSEC1,KRET)
C
C---->
C**** KWCHK1
C
C     Purpose.
C     --------
C
C     Check parameters for Washington ensemble products Section 1
C     of GRIB code against valid values for GRIB Code Edition 1.
C
C**   Interface.
C     ----------
C
C     CALL KWCHK1(KSEC1,KRET)
C
C
C     Input Parameters.
C     -----------------
C
C     KSEC1      - Array containing parameters for section
C                  1 of GRIB code.
C
C     Output Parameters.
C     ------------------
C
C     KRET       - Return code.
C                  0   , No error encountered.
C                  1   , Error in GRIB code parameter.
C
C     Method.
C     -------
C
C     Values checked against Washington ensemble product usage.
C
C
C     Externals.
C     ----------
C
C     None.
C
C
C     Reference.
C     ----------
C
C     WMO Manual on Codes for GRIB code.
C
C
C     Comments.
C     ---------
C
C     Routine contains Sections 0 to 2 and Section 9.
C
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      09.10.95
C
C     Modifications.
C     --------------
C
C     None.
C
C----<
C     ----------------------------------------------------------------
C*    Section 0 . Definition of variables. Data statements.
C     ----------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Subroutine arguments
      INTEGER KSEC1, KRET
      DIMENSION KSEC1(*)
C
C     Local variables
C
#ifdef CRAY
      INTEGER MMASK
#endif
      INTEGER IVERSN, IV0001, IV0011
      DATA IV0001/808464433/, IV0011/808464689/
C
C     IV0001 = right-justified ascii '0001'
C     IV0011 = right-justified ascii '0011'
C
C     ----------------------------------------------------------------
C*    Section 1 . Set initial values.
C     ----------------------------------------------------------------
C
  100 CONTINUE
C
C     Reset return code.
      KRET = 0
C
C     ----------------------------------------------------------------
C*    Section 2 . Check usage.
C     ----------------------------------------------------------------
C
  200 CONTINUE
C
C     Ensemble forecast data
      IF(KSEC1(37).NE.1) THEN
        WRITE(GRPRSM,*)
     X      'KWCHK1: Invalid Ensemble f/c data flag = ',KSEC1(37)
        KRET = 1
        GOTO 900
      ENDIF
C
C     Class.
      IF( KSEC1(38).NE.1 ) THEN
        WRITE(GRPRSM,*) 'KWCHK1: Invalid class code = ',KSEC1(38)
        KRET = 1
        GOTO 900
      ENDIF
C
C     Type.
      IF( (KSEC1(39).NE.10) .AND. (KSEC1(39).NE.11) ) THEN
        WRITE(GRPRSM,*) 'KWCHK1: Invalid type code = ',KSEC1(39)
        KRET = 1
        GOTO 900
      ENDIF
C
C     Stream.
      IF( KSEC1(40).NE.1051 ) THEN
        WRITE(GRPRSM,*) 'KWCHK1: Invalid stream code = ',KSEC1(40)
        KRET = 1
        GOTO 900
      ENDIF
C
C     Version number.
      IVERSN = KSEC1(41)
#if (!defined rs6000)
#ifdef CRAY
C
C     Mask least significant 32 bits for integer comparison
      MMASK = MASK(96)
      IVERSN = AND( IVERSN, MMASK)
#endif
      IF( (IVERSN.NE.IV0001) .AND. (IVERSN.NE.IV0011) ) THEN
        WRITE(GRPRSM,*) 'KWCHK1: Invalid version number = ', IVERSN
        KRET = 1
        GOTO 900
      ENDIF
#endif
C
C     Number of perturbed forecast.
      IF( KSEC1(39).EQ.10 ) THEN
        IF( KSEC1(42).NE.0 ) THEN
          WRITE(GRPRSM,*)
     X      'KWCHK1: Invalid control f/c number = ',KSEC1(42)
          KRET = 1
          GOTO 900
        ENDIF
C
      ELSE
        IF( (KSEC1(42).LT.0) .OR. (KSEC1(42).GT.11) ) THEN
          WRITE(GRPRSM,*)
     X      'KWCHK1: Invalid perturbed f/c number = ',KSEC1(42)
          KRET = 1
          GOTO 900
        ENDIF
      ENDIF
C
C     Total number of forecasts in ensemble.
      IF( (KSEC1(43).LT.0) .OR. (KSEC1(43).GT.11) ) THEN
        WRITE(GRPRSM,*)
     X      'KWCHK1: Invalid total f/cs in ensemble = ',KSEC1(43)
        KRET = 1
        GOTO 900
      ENDIF
C
C     ----------------------------------------------------------------
C*    Section 9 . Return to calling routine. Format statements.
C     ----------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END
