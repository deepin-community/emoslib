C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE EXTGRPC(KBUFL,YBUFF, I,K,CVAL,KERR )
C
C**** *EXTGRP*
C
C
C     PURPOSE.
C     --------
C         CONVERT GROUP IN THE REPORT
C         TO INTEGERS IN ARRAY 'KINT' .
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *EXTGRP(KBUFL,YBUFF, I,K,IVAL,KERR )
C
C                    kbufl - size of ybuff in bytes
C                    ybuff - character string
C
C                    I - CONVERSION OF GROUP STARTS IN WORD I
C                        OF ARRAY 'ybuff' .
C
C                    k - number of characters to convert
C
C
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/2003.
C
C
      IMPLICIT LOGICAL(O,G)
C
      CHARACTER*(*) YBUFF , CVAL
C
C
C     ------------------------------------------------------------------
C*          1.   EXTRACT GROUP OF CHARACTERS.
C                ----------------------------
 100  CONTINUE
C
C
      CVAL=YBUFF(I:I+K-1)
C
      I=I+K
C
C
      RETURN
      END
