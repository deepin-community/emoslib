C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C
      PROGRAM LINK_MARS
      IMPLICIT NONE
C
C     Checks symbols when linking.
C
      INTEGER I
      REAL R
      CHARACTER C
      INTEGER IARGC
C
      INTEGER AREACHK
      INTEGER IBASINI
      INTEGER INTF
      INTEGER INTIN
      INTEGER INTOUT
      INTEGER intf2
      INTEGER intuvp2
      INTEGER intuvs2
      INTEGER intvect2
      IF (IARGC().LT.0) THEN
          CALL INTLOG (I,C,I)
          I = AREACHK  (R,R,R,R,R,R)
          I = IBASINI  (I)
          I = INTF     (I,I,R,I,I,R)
          I = INTIN    (C,I,R,C)
          I = INTOUT   (C,I,R,C)
          I = intf2    (C,I,C,I)
          I = intuvp2  (C,C,I,C,C,I)
          I = intuvs2  (C,C,I,C,C,I)
          I = intvect2 (C,C,I,C,C,I)
      END IF
      END

