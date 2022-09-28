C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      LOGICAL FUNCTION ISGGVALID(HTYPE,KNUM)
C
C---->
C**** ISGGVALID
C
C     PURPOSE
C     _______
C
C     This function checks if a Gaussian grid definition is valid.
C
C
C     Parameters (input)
C     __________________
C
C     KNUM     - Gaussian grid number
C     HTYPE    - Gaussian grid type
C                = 'R' for reduced ("quasi-regular"),
C                = 'O' for reduced/octahedral,
C                = 'F' for full,
C                = 'U' for a user-defined gaussian grid
C
C----<
      IMPLICIT NONE

      INTEGER KNUM
      CHARACTER*1 HTYPE

      ! Condition:
      ! - for regular_gg or user-defined, valid for KNUM>=2
      ! - for reduced_gg, check grid number
      ISGGVALID = ( HTYPE.EQ.'F' .OR. HTYPE.EQ.'f' .OR.
     .              HTYPE.EQ.'O' .OR. HTYPE.EQ.'o' .OR.
     .              HTYPE.EQ.'U' .OR. HTYPE.EQ.'u' ) .AND.
     .            ( KNUM.GE.   2 )
     .            .OR.
     .            ( HTYPE.EQ.'R' .OR. HTYPE.EQ.'r' ) .AND.
     .            ( KNUM.EQ.  32 .OR.
     .              KNUM.EQ.  48 .OR.
     .              KNUM.EQ.  64 .OR.
     .              KNUM.EQ.  80 .OR.
     .              KNUM.EQ.  96 .OR.
     .              KNUM.EQ. 128 .OR.
     .              KNUM.EQ. 160 .OR.
     .              KNUM.EQ. 200 .OR.
     .              KNUM.EQ. 256 .OR.
     .              KNUM.EQ. 320 .OR.
     .              KNUM.EQ. 400 .OR.
     .              KNUM.EQ. 512 .OR.
     .              KNUM.EQ. 640 .OR.
     .              KNUM.EQ.1024 .OR.
     .              KNUM.EQ.1280 .OR.
     .              KNUM.EQ.2000 )

      END

