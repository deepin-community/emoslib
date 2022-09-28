C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXPRS0(KSEC0)
C
C**** *CREXPRS0*
C
C
C     PURPOSE.
C     --------
C           PRINT SECTION 0 OF CREX MESSAGE.
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *CREXPRS0(KSEC0)*
C
C        INPUT :
C               *KSEC0*   -  ARRAY CONTAINING SECTION 0 INFORMATION
C                            KSEC0( 1)-- LENGTH OF SECTION 0 (BYTES)
C                            KSEC0( 2)-- TOTAL LENGTH OF CREX MESSAGE (BYTES)
C                            KSEC0( 3)-- Crex Edition number
C
C     METHOD.
C     -------
C
C            NONE
C
C     EXTERNALS.
C     ----------
C
C            NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          MILAN DRAGOSAVAC    *ECMWF*       07/01/2004.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
      DIMENSION KSEC0(*)
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT SECTION 0.
C                ----------------
 100  CONTINUE
C
      WRITE(*,'(1H1)')
C
      WRITE(*,'(1H ,A)')    '         CREX SECTION 0    '
      WRITE(*,'(1H )')
      WRITE(*,'(1H ,A,I5)') 'LENGTH OF SECTION 0 (BYTES)         ',
     1                       KSEC0(1)
      WRITE(*,'(1H ,A,I5)') 'TOTAL LENGTH OF CREX MESSAGE (BYTES)',
     1                       KSEC0(2)
C
      RETURN
      END
