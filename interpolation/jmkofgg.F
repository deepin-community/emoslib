C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JMKOFGG( HTYPE,
     X                    KNORTH,KSOUTH,PWEST,PEAST,KNUM,KPTS,KOFSET)
C
C---->
C**** JMKOFGG
C
C     Purpose
C     _______
C
C     This routine builds up the offsets into the storage array for
C     every row in the gaussian grid.
C
C
C     Interface
C     _________
C
C     CALL JMKOFGG(HTYPE,KNORTH,KSOUTH,PWEST,PEAST,KNUM,KPTS,KOFSET)
C
C
C     Input parameters
C     ________________
C
C     KNUM    - Gaussian grid number
C     HTYPE   - Gaussian grid type
C               = 'R' for reduced ("quasi-regular"),
C               = 'O' for reduced/octahedral,
C               = 'F' for full,
C               = 'U' for user-defined reduced gaussian grid
C     KNORTH  - Northern latitude row number
C     KSOUTH  - Southern latitude row number
C     PWEST   - Westernmost longitude for output field (degrees)
C     PEAST   - Easternmost longitude for output field (degrees)
C     KPTS    - Array giving the number of points in each row for the
C               gaussian grid
C
C
C     Output parameters
C     ________________
C
C     KOFSET  - Array of offsets for storing each row of the grid
C               values in an output buffer
C
C
C     Common block usage
C     __________________
C
C     None
C
C
C     Method
C     ______
C
C     Build up the offsets allowing for latitudes which are not in the
C     requested latitude band.  All offsets outside the latitude band
C     are set to zero.
C
C
C     Externals
C     _________
C
C     None
C
C
C     Reference
C     _________
C
C     None
C
C
C     Comments
C     ________
C
C
C
C     Author
C     ______
C
C     J.D.Chambers      ECMWF       Jan 1994
C
C
C     Modifications
C     _____________
C
C     None
C
C
C----<
C     _______________________________________________________
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
      IMPLICIT NONE
C
C     Subroutine arguments
C
      CHARACTER*1 HTYPE
      INTEGER KNORTH, KSOUTH, KNUM, KPTS, KOFSET
      REAL    PWEST, PEAST
Cjdc  DIMENSION KPTS(*), KOFSET(*)
      DIMENSION KPTS(320), KOFSET(320)
C
C     Local variables
C
      INTEGER LAT, NUMPTS
      REAL RINTVL
C
C     _______________________________________________________
C
C*    Section 1. Fill in array of offsets
C     _______________________________________________________
C
  200 CONTINUE
C
C     Loop through all possible latitudes
      DO LAT = 1, 2*KNUM
        IF ( (LAT .LT. KNORTH) .OR. (LAT .GT. KSOUTH) ) THEN
          KOFSET(LAT) = 0
        ELSE
          IF ( LAT .EQ. KNORTH ) THEN
            KOFSET(LAT) = 1
          ELSE
            IF( HTYPE.EQ.'R' .OR. HTYPE.EQ.'r' .OR.
     X          HTYPE.EQ.'O' .OR. HTYPE.EQ.'o' .OR.
     X          HTYPE.EQ.'U' .OR. HTYPE.EQ.'u' ) THEN
              NUMPTS = KPTS(LAT -1)
            ELSE
              RINTVL = (PEAST - PWEST)*FLOAT( KPTS(LAT -1) ) / 360.0
              NUMPTS = NINT( RINTVL ) + 1
            ENDIF
            KOFSET(LAT) = KOFSET(LAT -1) + NUMPTS
          ENDIF
        ENDIF
      ENDDO
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
  900 CONTINUE
C
      RETURN
      END
