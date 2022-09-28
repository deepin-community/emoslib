C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C
      PROGRAM LINK_PRODGEN
      IMPLICIT NONE
C
C     Checks symbols when linking.
C
      INTEGER I
      REAL R
      CHARACTER C
      LOGICAL L
      COMPLEX J
      INTEGER IARGC
C
      INTEGER HIRLAM
      INTEGER HIRLAMW
      INTEGER HIRLSM
      INTEGER INTF
      INTEGER INTIN
      INTEGER INTOUT
      INTEGER JGETGG
      INTEGER JGGLAT
      INTEGER WV2DINT
      INTEGER WVQLINT
      IF (IARGC().LT.0) THEN
          CALL FREECF  (I)
          CALL JNUMGG  (I,C,I,I)
          CALL JVOD2UV (J,J,I,J,J,I)
          I = HIRLAM  (L,R,I,I,R,R,R,R,I,I,I)
          I = HIRLAMW (L,R,R,I,I,R,R,R,R,R,I,I,I)
          I = HIRLSM  (L,R,I,I,R,R,R,R,I,I,I)
          I = INTF    (I,I,R,I,I,R)
          I = INTIN   (C,I,R,C)
          I = INTOUT  (C,I,R,C)
          I = JGETGG  (I,C,R,I,I)
          I = JGGLAT  (I,R)
          I = WV2DINT (I,I,I,I,R,R,R,R,R,I,R,R)
          I = WVQLINT (I,I,I,I,R,R,R,R,R,I,R,R)
      END IF
      END

