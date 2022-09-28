C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE MAXMNI(KARRAY,KLEN,KMAX,KMIN)
C
C---->
C**** MAXMNI - Get maximum and minimum values.
C
C     Purpose.
C     --------
C
C     Get maximum and minimum values from an array of integer numbers.
C
C**   Interface.
C     ----------
C
C     CALL MAXMNI (KARRAY,KLEN,KMAX,KMIN)
C
C     Input Parameters.
C     -----------------
C
C     KARRAY - Array of numbers.
C     KLEN   - Last word of this array.
C
C     Output Parameters.
C     ------------------
C
C     KMAX   - Maximum value.
C     KMIN   - Minimum value.
C
C     Method.
C     -------
C
C     Intrinsic functions MAX and MIN are used.
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
C     Routine contains Sections 0 to 1 and Section 9.
C
C     Author.
C     -------
C
C     J. Clochard, Meteo France, for ECMWF - January 1998.
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
C     Subroutine arguments
C
      INTEGER KARRAY, KLEN, KMAX, KMIN
      DIMENSION KARRAY(*)
C
C     Local variables
C
      INTEGER JLOOP
C
C     ----------------------------------------------------------------
C*    Section 1 . Get maximum and minimum values.
C     ----------------------------------------------------------------
C
  100 CONTINUE
C
C     Set initial values.
C
      KMAX = KARRAY(1)
      KMIN = KARRAY(1)
C
C     Extract maximum and minimum values.
C
      DO 110 JLOOP = 2 , KLEN
        KMAX = MAX(KMAX,KARRAY(JLOOP))
        KMIN = MIN(KMIN,KARRAY(JLOOP))
  110 CONTINUE
C
C     ----------------------------------------------------------------
C*    Section 9. Return to calling routine.
C     ----------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
C
      END
