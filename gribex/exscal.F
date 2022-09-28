C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE EXSCAL (PDATA,KDATA,KLEN,PREF,PSCALE,LALLPOS)
C
C---->
C**** EXSCAL - Vectorise calculation of values.
C
C     Purpose.
C     --------
C
C     Vectorise calculation of values.
C
C**   Interface.
C     ----------
C
C     CALL EXSCAL (PDATA,KDATA,KLEN,PREF,PSCALE,LALLPOS)
C
C
C     Input Parameters.
C     -----------------
C
C     KDATA   - Array of integer increments
C     KLEN    - Number of values to be converted.
C     PREF    - Reference value.
C     PSCALE  - Scale factor.
C     LALLPOS - .TRUE. if all packed value occupies full (integer)
C                      word, eg 32 bits on a 32-bit integer machine.
C
C     Output Parameters.
C     -----------------
C
C     PDATA   - Array of floating point values.
C
C     Method.
C     -------
C
C     The reference value is added to each increment,
C     and the result is then multiplied by the scale factor.
C
C     Externals.
C     ----------
C
C     None.
C
C     Reference.
C     ----------
C
C     WMO Manual on Codes re GRIB representation.
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
C     J. Hennessy     ECMWF     25.06.91
C
C
C     Modifications.
C     _____________
C
C     J.D.Chambers    ECMWF     06.09.95
C     Reverse processing in loop to run from last to first in
C     case reals are larger than integers.
C
C     J.D.Chambers    ECMWF     16.05.96
C     Handle case when packed value occupies full word.
C
C----<
C     -----------------------------------------------------------------
C     Section 0.    Definition of variables.
C     -----------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Subroutine arguments
      INTEGER KDATA
      INTEGER KLEN
      INTEGER LOOP
      REAL PDATA
      REAL PREF
      REAL PSCALE
      LOGICAL LALLPOS
      DIMENSION PDATA(KLEN)
      DIMENSION KDATA(KLEN)
C
      REAL*8 BIG, RTEMP
      DATA BIG /4294967296.0/
C
C     -----------------------------------------------------------------
C*    Section 1 . Calculation of values.
C     -----------------------------------------------------------------
C
  100 CONTINUE
#ifdef CRAY
C
      DO 110 LOOP = KLEN, 1, -1
        PDATA(LOOP) = PREF + KDATA(LOOP)*PSCALE
  110 CONTINUE
#else
C
      IF( .NOT. LALLPOS ) THEN
        DO 110 LOOP = KLEN, 1, -1
          PDATA(LOOP) = PREF + KDATA(LOOP)*PSCALE
  110   CONTINUE
C
C     Packed values use all bits in integer word, ie sign bit is
C     really a data bit.
      ELSE
        DO 120 LOOP = KLEN, 1, -1
          IF( KDATA(LOOP) .GE. 0 ) THEN
            PDATA(LOOP) = PREF + KDATA(LOOP)*PSCALE
          ELSE
            RTEMP = BIG + DBLE(KDATA(LOOP))
            PDATA(LOOP) = PREF + RTEMP*PSCALE
          ENDIF
  120   CONTINUE
      ENDIF
#endif
C
C     -----------------------------------------------------------------
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
      RETURN
      END
