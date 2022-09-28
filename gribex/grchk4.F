C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GRCHK4 (KSEC1,KSEC4,PSEC4,KRET)
C
C---->
C**** GRCHK4 - Check parameters for Section 4 of GRIB Code.
C
C     Purpose.
C     --------
C
C           Check parameters for Section 4 of GRIB code against
C           valid values for GRIB Edition 1.
C
C**   Interface.
C     ----------
C
C           CALL GRCHK4 (KSEC1,KSEC4,PSEC4,KRET)
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
C               KSEC4      - Array containing integer parameters for
C                            section 4 of GRIB code.
C
C               PSEC4      - Array containing real parameters for
C                            section 4 of GRIB code.
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
C           of the implementation of GRIBEX.
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
C           Routine contains Sections 0 to 2 and Section 9.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      18.06.91
C
C     Modifications.
C     --------------
C
C           J. Hennessy      ECMWF      01.10.91
C           Additional parameters KSEC1 and PSEC4 added.
C
C           J. Hennessy      ECMWF      17.07.92
C           Additional flag and matrix of values checks added.
C
C           J.D.Chambers     ECMWF      27.04.94
C           Allow complex packing for spherical harmonics
C
C           J. Clochard, Meteo France, for ECMWF - January 1998.
C           Take into account second-order packing for grid-point data.
C           Revised printouts.
C
C----<
C     ----------------------------------------------------------------
C*    Section 0 . Definition of variables. Data statements.
C     ----------------------------------------------------------------
C
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
      INTEGER KRET
      INTEGER KSEC1
      INTEGER KSEC4
C
      REAL    PSEC4
C
      DIMENSION KSEC1(*)
      DIMENSION KSEC4(*)
C
      DIMENSION PSEC4(*)
C
C     ----------------------------------------------------------------
C*    Section 1 . Set initial values.
C     ----------------------------------------------------------------
C
  100 CONTINUE
C
C     Reset return code.
      KRET = 0
C
C     ----------------------------------------------------------------
C*    Section 2 . Check values against code tables and extreme values.
C     ----------------------------------------------------------------
C
  200 CONTINUE
C
C*    Check number of values to be encoded.
      IF (KSEC4(1).EQ.0) THEN
        KRET = 1
        WRITE(GRPRSM,*)
     X    'GRCHK4: Invalid number of data values to be encoded'
     X    , KSEC4(1)
      ENDIF
C
C*    Check number of bits per data value.
      IF (KSEC4(2).LT.1.OR.KSEC4(2).GT.32) THEN
        KRET = 1
        WRITE(GRPRSM,*)
     X     'GRCHK4: Invalid number of bits for packed data values'
     X    , KSEC4(2)
      ENDIF
C
C*    Check on type of data (grid or spherical harmonics).
      IF ( KSEC4(3).NE.0 .AND. KSEC4(3).NE.128 ) THEN
        WRITE(GRPRSM,*) 'GRCHK4: Invalid type of data', KSEC4(3)
      ENDIF
C
C*    Check type of packing.
      IF ( KSEC4(4).NE.0 .AND. KSEC4(4).NE.64 ) THEN
        KRET = 1
        WRITE(GRPRSM,*) 'GRCHK4: Invalid type of packing', KSEC4(4)
      ENDIF
C
C*    Check data representation.
      IF (KSEC4(5).NE.0.AND.KSEC4(5).NE.32) THEN
        KRET = 1
        WRITE(GRPRSM,*) 'GRCHK4: Invalid data representation', KSEC4(5)
      ENDIF
C
C*    Check additional flag field.
      IF (KSEC4(6).NE.0.AND.KSEC4(6).NE.16) THEN
        KRET = 1
        WRITE(GRPRSM,*)
     X    'GRCHK4: Invalid additional flag field', KSEC4(6)
      ENDIF
C
C*    Check reserved field.
      IF (KSEC4(7).NE.0) THEN
        WRITE(GRPRSM,*)
     X    'GRCHK4: Reserved field not 0. May cause trouble'
        WRITE(GRPRSM,*) 'GRCHK4: in future releases of GRIBEX.'
      ENDIF
C
C*    Check number of values indicator.
      IF (KSEC4(8).NE.0.AND.KSEC4(8).NE.64) THEN
        KRET = 1
        WRITE(GRPRSM,*) 'GRCHK4: Invalid number of fields indicator'
     X    , KSEC4(8)
      ENDIF
C
C*    Check secondary bit maps indicator.
      IF (KSEC4(9).NE.0.AND.KSEC4(9).NE.32) THEN
        KRET = 1
        WRITE(GRPRSM,*) 'GRCHK4: Invalid secondary bit map indicator'
     X    , KSEC4(9)
      ENDIF
C
C*    Check value widths indicator.
      IF (KSEC4(6).EQ.16.AND.KSEC4(10).NE.0.AND.KSEC4(10).NE.16) THEN
        KRET = 1
        WRITE(GRPRSM,*) 'GRCHK4: Invalid value widths', KSEC4(10)
      ENDIF
C
C*    Tests specific to second-order packing.
C
      IF (KSEC4(3).EQ.0.AND.KSEC4(4).EQ.64) THEN
C
C*    Check constant width.
        IF (KSEC4(10).EQ.0.AND.(ABS(KSEC4(11)).GE.KSEC4(2))) THEN
          KRET = 1
          WRITE(GRPRSM,9011) KSEC4(11), KSEC4(2)-1
        ENDIF
C
C*    Check general extended 2nd-order packing indicator.
        IF (KSEC4(12).NE.0.AND.KSEC4(12).NE.8) THEN
          KRET = 1
          WRITE(GRPRSM,*)
     X     'GRCHK4: Invalid general extended 2n-order packing indicator'
     X      , KSEC4(12)
        ENDIF
C
C*    Check boustrophedonic ordering indicator.
        IF (KSEC4(13).NE.0.AND.KSEC4(13).NE.4) THEN
          KRET = 1
          WRITE(GRPRSM,*)
     X     'GRCHK4: Invalid boustrophedonic ordering indicator'
     X      , KSEC4(13)
        ENDIF
C
C*    Check spatial differencing indicator.
        IF (KSEC4(14).NE.0.AND.KSEC4(14).NE.2) THEN
          KRET = 1
          WRITE(GRPRSM,*)
     X    'GRCHK4: Invalid spatial differencing indicator'
     X      , KSEC4(14)
        ENDIF
C
      ENDIF
C
C     ----------------------------------------------------------------
C*    Section 9 . Return to calling routine.
C     ----------------------------------------------------------------
C
  900 CONTINUE
C
 9011 FORMAT (TR1,'GRCHK4 : Second-order packing with constant width ',
     C        'option:',/,TR10,'Constant width value',I3,
     C        ' not in range [-/+',I2,'(=KSEC4(2)-1)].')
C
      RETURN
C
      END
