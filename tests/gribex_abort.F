C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C
      PROGRAM GRIBEX_ABORT
      IMPLICIT NONE
C
C     This test expects to fail (internal stop). It should not arrive
C     to the END statement, which would return 0.
C
      INTEGER I
      REAL R
      CHARACTER C
      CALL GRIBEX(I,I,I,R,I,R,I,R,I,I,I,I,C,I)
      END

