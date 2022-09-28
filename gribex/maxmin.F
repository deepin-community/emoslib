C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE MAXMIN(PARRAY,KLEN,PMAX,PMIN)
C
C---->
C**** MAXMIN - Get maximum and minimum values.
C
C     Purpose.
C     --------
C
C     Get maximum and minimum values from an array of
C     floating point numbers..
C
C**   Interface.
C     ----------
C
C     CALL MAXMIN(PARRAY,KLEN,PMAX,PMIN)
C
C     Input Parameters.
C     -----------------
C
C     PARRAY - Array of numbers.
C     KLEN   - Last word of this array.
C
C     Output Parameters.
C     ------------------
C
C     PMAX   - Maximum value.
C     PMIN   - Minimum value.
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
C     None.
C
C     Author.
C     -------
C
C     J. Hennessy      ECMWF      18:06:91
C
C     Modifications.
C     --------------
C
C     J. Clochard, Meteo France, for ECMWF - January 1998.
C     Start loop from index 2.
C     Inhibits use of DMAX1/DMIN1 on CRAY (SMP) systems.
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
      INTEGER KLEN
      REAL   PARRAY, PMAX, PMIN, VSMALL
      DIMENSION PARRAY(KLEN)
#ifndef __uxp__
C
C     Parameters
C
      REAL JPSMALL
#ifdef __alpha
#ifdef REAL_8
      PARAMETER (JPSMALL = 1E-307)
#else
      PARAMETER (JPSMALL = 1E-37)
#endif
#else
      PARAMETER (JPSMALL = 1E-38)
#endif
#endif
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
      PMAX = PARRAY(1)
      PMIN = PARRAY(1)
C
C     Extract maximum and minimum values.
C
      DO 110 JLOOP = 2 , KLEN
        PMAX = MAX(PMAX,PARRAY(JLOOP))
        PMIN = MIN(PMIN,PARRAY(JLOOP))
  110 CONTINUE
C
C     Adjust to an acceptable 'tiny' minimum
C
#ifdef __uxp__
      VSMALL = TINY(PMIN)
#else
      VSMALL = JPSMALL
#endif
      IF( (PMIN.NE.0.0).AND.(ABS(PMIN).LT.VSMALL) )
     X   PMIN = SIGN(VSMALL,PMIN)
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
