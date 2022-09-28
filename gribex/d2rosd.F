C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION D2ROSD ( KDATA,  KLEN,   KORDER, KPOWER, KMXPWR,
     X                          KBIAS,  OVECTA, ODEBUG )
C
C---->
C**** D2ROSD
C
C     Purpose.
C     --------
C
C     Rebuild Original field from Spatial Differences, when using
C     non-local features of GRIB second-order extended packing.
C
C**   Interface.
C     ----------
C
C     IRET = D2ROSD ( KDATA,  KLEN,  KPOWER, KMXPWR, OVECTA, ODEBUG )
C
C     Function result value is zero, except for non-valid order.
C
C     Input Parameters.
C     -----------------
C
C     KDATA      - Array of normalized values.
C     KLEN       - Exact number of grid-points to handle.
C     KPOWER     - Auxilary array, containing precomputed powers of 2-1.
C     KMXPWR     - Maximum number of bits allowed.
C     KBIAS      - BIAS of spatial differences.
C     OVECTA     - True if vectorizing algorithm has to be used.
C     ODEBUG     - True for some debug printout.
C
C
C     Output Parameters (effective only if function value is non zero).
C     ------------------
C
C     KDATA      - Array of normalized values (transformed).
C
C
C     Method.
C     -------
C
C     For vector systems, an external loop on order of spatial
C     differencing is made, and inner recursive sum is made by a
C     or by a vectorizing algorithm (that implies an overhead of
C     LOG2(KLEN) in operations and memory accesses).
C
C     For scalar systems, a single scan is performed, with contents
C     specific for each order.
C
C
C     Externals.
C     ----------
C
C     C2BITW    - Computes bit width of a positive integer value.
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
C     J. Clochard, Meteo France, for ECMWF - April 1998.
C
C
C     Modifications.
C     _____________
C
C     J. Clochard, September 1998.
C     Optimisation for scalar systems (memory access reduced).
C
C
C----<
C     -----------------------------------------------------------------
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Parameters
C
      INTEGER JPORDR
C
C       Maximum order of spatial differencing allowed.
C
      PARAMETER ( JPORDR=3 )
C
C     Subroutine arguments
C
      INTEGER KLEN, KMXPWR, KBIAS, KORDER
      INTEGER KDATA (KLEN), KPOWER (0:KMXPWR)
C
      LOGICAL OVECTA, ODEBUG
C
C     Local variables.
C
      INTEGER IOFF, J, J2, JORDER, ILOG2N, IORIGI, ILEN, IRETFN, I1DIFF
      INTEGER I2DIFF
C
      INTEGER C2BITW
      EXTERNAL C2BITW
C
C     -----------------------------------------------------------------
C*    Section 1 . Performs initial checks.
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
#ifndef ebug2o
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9100) KLEN, KORDER, KBIAS
#ifndef ebug2o
      ENDIF
#endif
C
      IF (KORDER.LE.0.OR.KORDER.GT.JPORDR) THEN
        IRETFN = 23110
        WRITE(GRPRSM,FMT=9110) KORDER, JPORDR
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 2 . Preprocessing of first values of field.
C     -----------------------------------------------------------------
C
  200 CONTINUE
C
      IF (OVECTA) THEN
C
C       For higher orders, and due to iterative outer loop on order,
C       it is necessary that the 2nd value contains the first 1st-order
C       difference (between the 2 first values), and so on.
C
        DO 202 JORDER = 2, KORDER
C
#ifdef CRAY
CDIR$ NOVECTOR
#endif
#ifdef FUJITSU
!OCL SCALAR
#endif
          DO 201 J = KORDER, JORDER, -1
C
            KDATA(J)=KDATA(J)-KDATA(J-1)
C
  201     CONTINUE
#ifdef CRAY
CDIR$ VECTOR
#endif
#ifdef FUJITSU
!OCL VECTOR
#endif
C
  202   CONTINUE
C
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 3 . Rebuild original field integer values.
C     -----------------------------------------------------------------
C
  300 CONTINUE
C
      IF (OVECTA) THEN
C
C                       Vectorizable algorithm.
C                       ----------------------
C
        DO 315 JORDER = KORDER, 1, -1
C
          ILEN=KLEN-JORDER
          ILOG2N=C2BITW ( ILEN-1, KMXPWR, KPOWER, KMXPWR )
C
          IF (JORDER.EQ.KORDER) THEN
C
C              Bias is added at first scan.
C
            DO 312 J2 = 1, ILOG2N
C
              IOFF=KPOWER(J2-1)+1
C
#ifdef CRAY
CDIR$ IVDEP
#endif
#ifdef FUJITSU
!OCL NOVREC
#endif
              DO 311 J = KLEN, IOFF+JORDER, -1
C
                KDATA(J)=KDATA(J)+(KDATA(J-IOFF)+KBIAS)
C
  311         CONTINUE
C
  312       CONTINUE
C
          ELSE
C
            DO 314 J2 = 1, ILOG2N
C
              IOFF=KPOWER(J2-1)+1
C
#ifdef CRAY
CDIR$ IVDEP
#endif
#ifdef FUJITSU
!OCL NOVREC
#endif
              DO 313 J = KLEN, IOFF+JORDER, -1
C
                KDATA(J)=KDATA(J)+KDATA(J-IOFF)
C
  313         CONTINUE
C
  314       CONTINUE
C
          ENDIF
C
  315   CONTINUE
C
      ELSE
C
C                       Recurrent (scalar) algorithm.
C                       ----------------------------
C
C             Only one scan, with specific code for each order.
C
C         Initialize recursion.
C
        IORIGI=KDATA(KORDER)
C
        IF (KORDER.EQ.1) THEN
C
          DO 321 J = 2, KLEN
C
            IORIGI=IORIGI+(KDATA(J)+KBIAS)
            KDATA(J)=IORIGI
C
  321     CONTINUE
C
        ELSEIF (KORDER.EQ.2) THEN
C
          I1DIFF=KDATA(2)-KDATA(1)
C
          DO 322 J = 3, KLEN
C
            I1DIFF=I1DIFF+(KDATA(J)+KBIAS)
            IORIGI=IORIGI+I1DIFF
            KDATA(J)=IORIGI
C
  322     CONTINUE
C
        ELSE
C
          I1DIFF=KDATA(3)-KDATA(2)
          I2DIFF=I1DIFF-(KDATA(2)-KDATA(1))
C
          DO 323 J = 4, KLEN
C
            I2DIFF=I2DIFF+(KDATA(J)+KBIAS)
            I1DIFF=I1DIFF+I2DIFF
            IORIGI=IORIGI+I1DIFF
            KDATA(J)=IORIGI
C
  323     CONTINUE
C
        ENDIF
C
#ifdef ebug2o
        ILEN=MIN (KLEN,10)
        PRINT *,'d2rosd - JORDER=',JORDER,',KDATA(1:',ILEN,')=',
     S           (KDATA(J),J=1,ILEN)
C
#endif
      ENDIF
C
      IRETFN=0
C
C     -----------------------------------------------------------------
C*    Section 9 . Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      D2ROSD = IRETFN
C
#ifndef ebug2o
C
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9900) IRETFN
#ifndef ebug2o
      ENDIF
#endif
C
      RETURN
C
 9100 FORMAT (' D2ROSD: Function start, KLEN =',I8,', KORDER =',I2,
     X        ', KBIAS=',I9,'.')
C
 9110 FORMAT (' D2ROSD: Order requested:',I5,', out of [1,',I1,'].')
C
 9312 FORMAT (' D2ROSD: Range diagnostic is ',A,
     X        ' of spatial differences, order',I2,'.')
 9331 FORMAT (' D2ROSD: Selected order:',I2,', range diagnostics:',
     X        4I8,'.')
C
 9900 FORMAT (' D2ROSD: Function return code =',I6,'.')
C
      END
