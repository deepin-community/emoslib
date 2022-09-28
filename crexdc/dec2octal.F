C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE DEC2OCTAL(KOCTAL,KDECIMAL,KERR)
C**** *DEC2OCTAL*
C
C
C     PURPOSE.
C     --------
C         Convert decomal integer into octal value
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *DEC2OCTAL(KOCTAL,KDECIMAL,KERR)
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          NONE.
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
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C

      INTEGER KOCTAL
      INTEGER KDECIMAL
      CHARACTER*10 C
c
      KERR=0
      WRITE(C,'(O10)',IOSTAT=IOS) KDECIMAL
      IF(IOS.NE.0) then
        Print*,'DEC2OCTAL: internal write error ',ios
        KERR=33
        RETURN
      ENDIF
C
      READ(C, '(I10)',IOSTAT=IOS) KOCTAL
      IF(IOS.NE.0) then
        Print*,'DEC2OCTAL: internal read error ',ios
        KERR=35
        RETURN
      ENDIF

c     
      RETURN
      END
