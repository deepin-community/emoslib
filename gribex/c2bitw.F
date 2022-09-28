C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION C2BITW ( KVALUE, KXBITS, KPOWER, KMXPWR )
C
C---->
C**** C2BITW
C
C     Purpose.
C     --------
C
C     Compute BIT Width necessary to encode a positive integer value.
C
C**   Interface.
C     ----------
C
C     IWIDTH = C2BITW ( KVALUE, KXBITS, KPOWER, KMXPWR )
C
C     Function return value is the requested bit width.
C
C     Input Parameters.
C     -----------------
C
C     KVALUE     - Positive integer value.
C     KXBITS     - MaXimum number of BITS estimated.
C     KPOWER     - Auxilary array, containing precomputed powers of 2-1.
C     KMXPWR     - Maximum number of bits allowed.
C
C
C     Output Parameters.
C     ------------------
C
C     None.
C
C
C     Method.
C     -------
C
C     Loop down on possible bit width values.
C
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
C     None.
C
C
C     Comments.
C     --------
C
C     None.
C
C
C     Author.
C     -------
C
C     J. Clochard, Meteo France, for ECMWF - February 1998.
C
C
C     Modifications.
C     _____________
C
C     None.
C
C----<
C
C     -----------------------------------------------------------------
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Subroutine arguments
C
      INTEGER KVALUE, KXBITS, KMXPWR
      INTEGER KPOWER (0:KMXPWR)
C
C     Local variables.
C
      INTEGER IRETFN, J
C
C     -----------------------------------------------------------------
C*    Section 1 . Direct computing.
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
      DO 101 J=MIN (KXBITS,KMXPWR)-1,0,-1
C
      IF (KVALUE.GT.KPOWER(J)) THEN
        IRETFN=J+1
        GOTO 102
      ENDIF
C
  101 CONTINUE
C
      IRETFN=0
C
  102 CONTINUE
C
C     -----------------------------------------------------------------
C*    Section 9 . Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      C2BITW = IRETFN
C
C
      RETURN
C
      END
