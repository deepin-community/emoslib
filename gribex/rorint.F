C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE RORINT(PDATA,KDATA,KLEN,HDIR)
C
C---->
C**** RORINT - Conversion of arrays between data types.
C
C     Purpose.
C     --------
C
C     Converts real arrays to integer and vice versa.
C
C**   Interface.
C     ----------
C
C     CALL RORINT(PDATA,KDATA,KLEN,HDIR)
C
C     Input Parameters.
C     -----------------
C
C     KDATA      - Array of integer increments
C                  Input for 'R' function.
C     PDATA      - Array of floating point values.
C                  Input for 'I' function.
C     KLEN       - Number of values to be converted.
C     HDIR       - 'R', convert integer to real.
C                  'I', convert real to integer.
C
C     Output Parameters.
C     -----------------
C
C     KDATA      - Array of integer increments
C                  Output for 'I' function.
C     PDATA      - Array of floating point values.
C                  Output for 'R' function.
C
C     Method.
C     -------
C
C     Data types converted.
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
C     --------
C
C     PDATA and KDATA are really the same array. This routine
C     is just a device to force vectorisation on the Cray,
C     without the necessity of using another array.
C
C
C     Author.
C     -------
C
C     J. Hennessy     ECMWF     27.09.91
C
C     Modifications.
C     _____________
C
C     J.D.Chambers          ECMWF     21.11.94
C     Use JNINT on VAX
C
C     J.D.Chambers    ECMWF     23.10.97
C     Use input and output arrays with same REAL and INTEGER size.
C
C----<
C     -----------------------------------------------------------------
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------
C
      IMPLICIT NONE
C
#if (defined REAL_BIGGER_THAN_INTEGER)
      INTEGER*8 KDATA
      REAL*8 PDATA
#else
      INTEGER KDATA
      REAL PDATA
#endif
      INTEGER KLEN
      DIMENSION PDATA(KLEN)
      DIMENSION KDATA(KLEN)
      CHARACTER*(1) HDIR
C
      INTEGER JLOOP
C
C     -----------------------------------------------------------------
C*    Section 1 . Conversion of data types.
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
C     Real to integer
C
      IF (HDIR.EQ.'I') THEN
        DO 110 JLOOP = 1, KLEN
#ifdef VAX
          KDATA(JLOOP) = JNINT(PDATA(JLOOP))
#else
          KDATA(JLOOP) = NINT(PDATA(JLOOP))
#endif
  110   CONTINUE
C
C     Integer to real
C
      ELSE
        DO 120 JLOOP = KLEN, 1, -1
#if (defined REAL_BIGGER_THAN_INTEGER)
          PDATA(JLOOP) = DBLE(KDATA(JLOOP))
#else
          PDATA(JLOOP) = REAL(KDATA(JLOOP))
#endif
  120   CONTINUE
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END
