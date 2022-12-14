C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

        SUBROUTINE PBPSEU(KUNIT,KARRAY,KINLEN,KOUTLEN,KRET)
C
C---->
C**** PBPSEU
C
C     PURPOSE
C     _______
C
C     Reads next PSEUDO GRIB product from a file.
C
C     INTERFACE
C     _________
C
C     CALL PBPSEU(KUNIT,KARRAY,KINLEN,KOUTLEN,KRET)
C
C
C     Input parameters
C     ________________
C
C     KUNIT  - Unit number for the file returned from PBOPEN
C     KARRAY - FORTRAN array big enough to hold the PSEU product
C     KINLEN - Size in BYTES of the FORTRAN array
C
C
C     Output parameters
C     ________________
C
C     KOUTLEN - Size in BYTES of the PSEU product read
C     KRET    -  0  if a PSEU product has been successfully read
C               -1  if end-of-file is hit before a PSEU product is read
C               -3  if the size of KARRAY is not sufficient for the
C                   PSEU product
C
C
C     Common block usage
C     __________________
C
C     None.
C
C
C     Method
C     ______
C
C     Calls PSEUREAD.
C
C
C     Externals
C     _________
C
C     PSEUREAD - Read next PSEUDO GRIB product.
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers       ECMWF
C
C
C     MODIFICATIONS
C     _____________
C
C     None.
C
C******************************************************************************
C----<
C
C     Subroutine arguments
C
      INTEGER KARRAY(1)
      INTEGER KUNIT,KINLEN,KOUTLEN,KRET
C
C     Local argument(s)
C
      INTEGER NREAD, IRET
C
C     Get the PSEU product
C
      CALL PSEUREAD( KARRAY, KINLEN, NREAD, IRET, KUNIT )
C
C     Escape if the user buffer is too small to hold even the early sections of
C     the product or EOF encountered
C
      IF( IRET.EQ.-4 ) THEN
        KOUTLEN = NREAD
        KRET = -1
        RETURN
      ENDIF
C
C     Escape if no PSEU product is found in the file
C
      IF( IRET.EQ.-1 ) THEN
        KOUTLEN = 0
        KRET = -1
        RETURN
      ENDIF
C
C     Check if the array is big enough for the PSEU product
C
      IF( IRET.EQ.-3 ) THEN
        KOUTLEN = NREAD
        KRET = -3
        RETURN
      ENDIF
C
C     Set success code if product retrieved
C
      IF( NREAD.GE.0 )  THEN
        KOUTLEN = NREAD
        KRET = 0
      ENDIF
C
      RETURN
      END
