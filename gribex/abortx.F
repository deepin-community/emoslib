C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ABORTX (HNAME)
C
C---->
C**** ABORTX - Terminates execution of program.
C
C     Purpose.
C     --------
C
C     Terminates execution of program.
C
C**   Interface.
C     ----------
C
C     CALL ABORTX (HNAME)
C
C
C     Input Parameters.
C     -----------------
C
C     HNAME      - Name of calling routine.
C
C     Output Parameters.
C     ------------------
C
C     None.
C
C     Method.
C     -------
C
C     Prints message and terminates.
C
C     Externals.
C     ----------
C
#if defined(VAX) || defined(__PGI)
C     EXIT (VAX/PGI Fortran)
#elif (defined IBM) && (!defined rs6000)
C     SYSABN (IBM)
#elif defined(CRAY)
C     ABORT (Cray)
#elif defined(sun) || defined (sgi) || (defined rs6000)
C     ABORT (SUN/SGI)
#elif defined(CYBER)
C     ABORT (Cyber)
#elif defined(__uxp__)
C     ABORT (Fujitsu)
#elif defined(__hpux)
C     JABORT (C)
#endif
C
C     Reference.
C     ----------
C
C     None.
C
C     Comments.
C     ---------
C
C     None.
C
C     Author.
C     -------
C
C     J. Hennessy      ECMWF      13.11.91
C
C     Modifications.
C     --------------
C
C     None.
C
C----<
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
      CHARACTER*(*) HNAME
C
C     ------------------------------------------------------------------
C*    Section 1 . Print message and terminate.
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
      WRITE(GRPRSM,9001) HNAME
C
#if defined(VAX) || defined(__PGI)
      CALL EXIT (-1)
#elif (defined IBM) && (!defined rs6000)
      CALL SYSABN (1)
#elif defined(CRAY)
      CALL ABORT
#elif defined(sun) || defined (sgi) || (defined rs6000)
      CALL ABORT
#elif defined(CYBER)
      CALL ABORT ('US',0,' ')
#elif defined(__uxp__)
      CALL ABORT
#elif defined(__hpux)
      CALL JABORT
#endif
C     Ensure termination and non-zero return if external doesn't
C     (STOP is intrinsic)
      STOP 1
C
C     ------------------------------------------------------------------
C*    Section 9 . Format statements.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
 9001 FORMAT (1H ,'ABORTX : Routine ',A,' has requested program',
     C               ' termination.')
C
      RETURN
C
      END

