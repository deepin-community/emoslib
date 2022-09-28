C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE INTLOGD(KONOFF)
C
C---->
C**** INTLOGD
C
C     PURPOSE
C     _______
C
C     This routine toggles the debug flag LDEBUG.
C
C
C     INTERFACE
C     _________
C
C     CALL INTLOG(KONOFF)
C
C
C     Input parameters
C     ________________
C
C     KONOFF   - value to set in the debug flag LDEBUG.
C                = 0 for 'off'.
C                = 1 for 'on'.
C
C
C     Output parameters
C     ________________
C
C     None.
C
C
C     Common block usage
C     __________________
C
C     LDEBUG in /INTLOGC/ controls display of message
C                 = 0 for no display
C                 = 1 to display
C
C
C     Method
C     ______
C
C     Toggle flag.
C
C
C     Externals
C     _________
C
C     None.
C
C
C     Comments
C     ________
C
C     LDEBUG is toggled by a call to INTLOGD.
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers      *ECMWF*      Jul 1995
C
C
C     MODIFICATIONS
C     _____________
C
C     None.
C
C
C----<
C     _______________________________________________________
C
C     Subroutine arguments.
      INTEGER KONOFF
#include "intlog.h"
C
      LDEBUG =  (KONOFF.EQ.1)
C
      RETURN
      END
