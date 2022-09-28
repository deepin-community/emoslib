C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C


      SUBROUTINE SETPAR (KBIT,KNEG,KPR)
C
C---->
C**** SETPAR - Set number of bits in word. Set maximum negative integer.
C
C     Purpose.
C     --------
C
C     Set number of bits in word. Set maximum negative integer.
C
C**   Interface.
C     ----------
C
C     CALL SETPAR (KBIT,KNEG,KPR)
C
C
C     Input Parameters.
C     -----------------
C
C     KPR - Debug print switch.
C           > 0 , print out.
C           0   , No print out.
C
C     Output Parameters.
C     ------------------
C
C     KBIT       - Number of bits in computer word.
C
C     KNEG       - Maximum negative integer.
C
C     Method.
C     -------
C
C     Values are assigned according to hardware platform.
C
C     Externals.
C     ----------
C
C     None.
C
C     Reference.
C     ----------
C
C     None.
C
C     Comments.
C     ---------
C
C     None
C
C     Author.
C     -------
C
C     J. Hennessy      ECMWF      28.10.91
C
C     Modifications.
C     --------------
C
C     None.
C
C     ------------------------------------------------------------------
C----<
C
C*    Section 0 . Definition of variables. Data statements.
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
      INTEGER KBIT
      INTEGER KNEG
      INTEGER KPR
C
C     ------------------------------------------------------------------
C*    Section 1 . Assign values.
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
      IF( KPR.GE.1 )  WRITE(GRPRSM,*) ' SETPAR : Section 1.'
C
#ifdef INTEGER_8
      KBIT = 64
      KNEG = -9223372036854775807
#elif (defined VAX)
      KBIT = 32
      KNEG = -2147483648
#else
      KBIT = 32
      KNEG = -2147483647
#endif
C     ------------------------------------------------------------------
C*    Section 9 . Return to calling routine.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
      IF( KPR.GE.1 ) THEN
        WRITE(GRPRSM,*) ' SETPAR : Section 9.'
        WRITE(GRPRSM,*) '          Output values set -'
        WRITE(GRPRSM,*) 'SETPAR: KBIT = ', KBIT
        WRITE(GRPRSM,*) 'SETPAR: KNEG = ', KNEG
      ENDIF
C
      RETURN
      END
