C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JDEBUG( )
C
C---->
C**** JDEBUG
C
C     PURPOSE
C     _______
C
C     This routine checks whether the debug flag is to be turned on.
C
C
C     INTERFACE
C     _________
C
C     CALL JDEBUG( )
C
C     Input parameters
C     ________________
C
C     None
C
C
C     Output parameters
C     ________________
C
C     None
C
C
C     Common block usage
C     __________________
C
C     JDCNDBG - set flag NDBG.
C     JDCSPGP - set flag LFREECF, etc.
C
C
C     Method
C     ______
C
C     On first call, checks whether or not the environment 
C     variable JDCNDBG exists.
C
C
C     Externals
C     _________
C
C     GETENV    - Checks value of an environment variable.
C     INTLOGD   - Switchs on/off the printing off interpoaltion messages
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
C     None.
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers      *ECMWF*      Mar 1994
C
C
C     MODIFICATIONS
C     _____________
C
C     J.D.Chambers      *ECMWF*      5 Sep 1995
C     Add call to intlogd.
C
C     J.D.Chambers      *ECMWF*      19 Mar 1998
C     Add setting of LFREECF, etc
C
C----<
C
      IMPLICIT NONE
#include "jparams.h"
#include "jparam2.h"
C
C     Local variables
C
      INTEGER NFIRST
      DATA NFIRST/0/
      CHARACTER*12 YFLAG
      SAVE NFIRST
C
C     _______________________________________________________
C
C*    Section 1.    Initialization.
C     _______________________________________________________
C
  100 CONTINUE
C
      IF( NFIRST.EQ.0 ) THEN
        NFIRST = 1
C
C       Set debug level to control which messages are displayed
C
        CALL GETENV('JDCNDBG', YFLAG)
        IF( YFLAG(1:1).EQ.' ' ) THEN
           NDBG = 0
           CALL GETENV('EMOSLIB_DEBUG', YFLAG)
        ENDIF
        IF( YFLAG(1:1).EQ.'1' ) NDBG = 1
        IF( YFLAG(1:1).EQ.'2' ) NDBG = 2
        IF( YFLAG(1:1).EQ.'3' ) NDBG = 3
        IF( NDBG.EQ.0 ) THEN
          CALL INTLOGD(0)
        ELSE
          CALL INTLOGD(1)
        ENDIF
C
C       Initialise the flags controlling handling of spectral -> grid
C       interpolation coefficients memory unless already set by user
C
        IF( NFREECF.NE.11041967 ) THEN
          LFREECF = .FALSE.
          NFREECF = 11041967
        ENDIF
C
C       Clear the sizes of the memory areas used for coefficients in
C       spectral -> grid interpolations
C
        NISIZE6 = 0
        NISIZE7 = 0
C
      ENDIF
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine.
C     _______________________________________________________
C
 900  CONTINUE
C
      RETURN
      END
