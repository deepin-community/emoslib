C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GET_FREE_UNIT(KUNIT)
C
C**** *CTABLE*
C
C
C     PURPOSE.
C     --------
C        Get free unit number  
C         
C
C**   INTERFACE.
C     ----------
C          NONE.
C
C
C
C
C     *METHOD.
C      -------
C          NONE.
C
C
C
C     EXTERNALS.
C     ----------
C          NONE.
C
C
C
C
C     REFERENCE.
C     ----------
C
C
C     AUTHOR.

C          M. DRAGOSAVAC       *ECMWF*       JANUARY 2008.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)

C
      DO I=10,300
         INQUIRE(UNIT=i,IOSTAT=IOS,OPENED=OPENED)
         IF(.NOT.OPENED) THEN
            KUNIT=I
            GO TO 100
         END IF
      END DO
100   CONTINUE
      END 
