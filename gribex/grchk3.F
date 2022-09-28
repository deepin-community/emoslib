C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C


      SUBROUTINE GRCHK3 (KSEC1,KSEC3,PSEC3,KRET)
C
C---->
C**** GRCHK3 - Check parameters for Section 3 of GRIB Code.
C
C     Purpose.
C     --------
C
C           Check parameters for Section 3 of GRIB code against
C           valid values for GRIB Edition 1.
C
C**   Interface.
C     ----------
C
C           CALL GRCHK3 (KSEC1,KSEC3,PSEC3,KRET)
C
C           Integer    K.
C           Real       P.
C           Logical    O.
C           Character  H.
C
C               Input Parameters.
C               -----------------
C
C               KSEC1      - Array containing integer parameters for
C                            section 1 of GRIB code.
C
C               KSEC3      - Array containing integer parameters for
C                            section 3 of GRIB code.
C
C               PSEC3      - Array containing real parameters for
C                            section 3 of GRIB code.
C
C               Output Parameters.
C               ------------------
C
C               KRET       - Return code.
C                            0   , No error encountered.
C                            1   , Error in GRIB Code parameter.
C
C     Method.
C     -------
C
C           Values checked against current code/flag tables
C           and against maximum or minimum permitted values.
C           They are also checked against the current status
C           of the implementation of GRIBEX and ECMWF usage.
C
C     Externals.
C     ----------
C
C           None.
C
C     Reference.
C     ----------
C
C           WMO Manual on Codes for GRIB Code.
C
C     Comments.
C     ---------
C
C           Routine contains Sections 0 to 5 and Section 9.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      16.09.91
C
C     Modifications.
C     --------------
C
C           J. Hennessy      ECMWF      01.10.91
C           Additional parameter KSEC1 and PSEC3 added.
C
C     ----------------------------------------------------------------
C----<
C
C
C
C
C
C
C
C
C
C
C*    Section 0 . Definition of variables. Data statements.
C     ----------------------------------------------------------------
C
C*    Prefix conventions for variable names.
C
C     Logical      L (but not LP), global or common.
C                  O, dummy arguments.
C                  G, local variable.
C                  LP, parameter.
C     Character    C, Global or common.
C                  H, dummy arguments.
C                  Y (but not YP), local variables.
C                  YP, parameter.
C     Integer      M and N, global or common.
C                  K, dummy arguments.
C                  I, local variables.
C                  J (but not JP), loop control.
C                  JP, parameter.
C     Real         A to F and Q to X, global or common.
C                  P (but not PP), dummy arguments.
C                  Z, local variables.
C                  PP, parameter.
C

      IMPLICIT NONE
C
#include "common/grprs.h"
C
      INTEGER KRET
      INTEGER KSEC1
      INTEGER KSEC3
C
      REAL    PSEC3
C
      DIMENSION KSEC1(*)
      DIMENSION KSEC3(*)
C
      DIMENSION PSEC3(*)
C
C     ----------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C
C*    Section 1 . Set initial values.
C     ----------------------------------------------------------------
C
  100 CONTINUE
C
C     Reset return code.
C
      KRET = 0
C
C     ----------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C
C*    Section 2 . Check values.
C     ----------------------------------------------------------------
C
  200 CONTINUE
C
C*    Check bit-map table reference field.
C
      IF (KSEC3(1).LT.0.OR.KSEC3(1).GT.65535)
     C   THEN
             KRET = 1
             WRITE(GRPRSM,9001) KSEC3(1)
         ENDIF
C
C     ----------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C
C
C*    Section 9 . Return to calling routine. Format statements.
C     ----------------------------------------------------------------
C
  900 CONTINUE
C
 9001 FORMAT (1H ,'GRCHK3 : Invalid bit-map table reference - ',I9)
C
      RETURN
      END
