C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      LOGICAL FUNCTION ISSAMERARRAY(N1,AN1,N2,AN2)
C
C---->
C**** *ISSAMERARRAY*
C
C     PURPOSE
C     _______
C
C     Check if two real arrays have the same entries.
C
C
C     Input parameters
C     ________________
C
C     N1   - Array 1 length
C     AN1  - Array 1 entries (real values)
C     N2   - Array 2 length
C     AN2  - Array 2 entries (real values)
C
C----<
      IMPLICIT NONE
      INTEGER N1, AN1(*), N2, AN2(*)
      INTEGER I


C     Cover special cases
      ISSAMERARRAY =
     X  N1.GT.0 .AND.
     X  N2.GT.0 .AND.
     X  N1.EQ.N2


C     Iterate through entries, failing on first difference
      I = 1
      DO WHILE (ISSAMERARRAY .AND. I.LE.N1)
        ISSAMERARRAY = AN1(I).EQ.AN2(I)
        I = I+1
      ENDDO

      END FUNCTION

