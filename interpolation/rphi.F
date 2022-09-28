C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

       SUBROUTINE RPHI( DATA, NM, WORK, SLON)
C
C---->
C**** RPHI
C
C     Purpose
C     -------
C
C     Rotates spectral coefficients by longitude.
C
C
C     Interface
C     ---------
C
C     CALL RPHI(DATA,NM,WORK,SLON)
C
C     Input
C     -----
C
C     DATA   = Input spectral field of REAL*8s of size (NM+1)*(NM+2).
C     NM     = Triangular truncation (e.g. 106)
C     WORK   = Work space of size at least 2*(NM+1).
C     SLON   = Rotation angle (REAL*8).
C              (degrees, positive => rotate west to east). 
C
C     Output
C     ------
C
C     Transformed fields are returned in data.
C
C
C     Method
C     ------
C
C     See reference below.
C
C
C     Externals
C     ---------
C
C     None.
C
C
C     Author
C     ------
C
C     R.McGrath and P.Lynch    HIRLAM
C
C
C     Reference.
C     ----------
C
C     "Spectral synthesis on rotated and regular grids"
C     by P.Lynch and R.McGrath (Irish Meteorological Service).
C
C
C     Modifications
C     -------------
C
C     J.D.Chambers       ECMWF     October 1995
C     Reformat and put into clearcase.
C
C ------------------------------------------------------------------
C----<
C
      IMPLICIT NONE
C
#include "jparams.h"
C
C     Function arguments
      COMPLEX*16 DATA
      COMPLEX*16 WORK
      DIMENSION DATA(*), WORK(*)
      REAL*8 SLON
      INTEGER NM
C
C     Parameters
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 26000 )
C
C     Local variables
      COMPLEX*16 CC
      INTEGER IPMN, NS, M, J
      REAL*8 RAD, XLON
C
C ------------------------------------------------------------------
C*    Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
C
      IPMN = ((NM+1)*(NM+2))/2
      RAD  = 180.0/PPI
      XLON = -SLON/RAD
C
C ------------------------------------------------------------------
C*    Section 2.   Transform the fields.
C ------------------------------------------------------------------
C
  200 CONTINUE
C
      NS = 1
      CC = (1.0,0.0)
C
      DO 215 M = 1,NM+1
        DO 210 J = NS,NS+NM-M+1
          WORK(J) = CC
  210   CONTINUE
C
        NS = J
        CC = CEXP(CMPLX(0.0,REAL(XLON*M)))
  215 CONTINUE
C
C     Transform fields in output array.
        DO 220 J = 1,IPMN
          DATA(J) = WORK(J)*DATA(J)
  220   CONTINUE
C
C ------------------------------------------------------------------
C*    Section 9.   Return.
C ------------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END
