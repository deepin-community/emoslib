C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE EMOSLIB_SSCAL (KN, SA, XVEC, KNCX)
C
C---->
C**** *EMOSLIB_SSCAL*
C
C     PURPOSE.
C     --------
C
C     SIMPLE REPLACEMENT FOR IBM (for both BLAS SSCAL and DSCAL)
C
C**   INTERFACE.
C     ----------
C
C     * CALL EMSOLIB_SSCAL (KN, SA, XVEC, KNCX) *
C
C
C     METHOD.
C     -------
C
C     XVEC (1 + IP * KNCX) = XVEC (1 + IP * KNCX) * SA
C     FOR IP = 0 TO KN - 1
C
C     EXTERNALS.
C     ----------
C
C     NONE
C
C     REFERENCE.
C     ----------
C
C     NONE
C
C     AUTHOR.
C     -------
C
C     K. FIELDING         * ECMWF *     FEB 1992.
C
C     MODIFICATIONS.
C     --------------
C
C     NONE
C
C     COMMON BLOCKS USED
C     -------------------
C
C     NONE
C----<
C
      INTEGER KN, KNCX
Cjdc  DOUBLE PRECISION SA
Cjdc  DOUBLE PRECISION XVEC (*)
      REAL SA
      REAL XVEC (*)
C
      INTEGER IP
C
C     1.    MAIN LOOP OF CODE
C           ----------------------------------------------
C
  100 CONTINUE
      DO 110 IP = 0, KN - 1
         XVEC (1 + IP * KNCX) = XVEC (1 + IP * KNCX) * SA
  110 CONTINUE
C
C     END OF ROUTINE
C
      RETURN
      END
