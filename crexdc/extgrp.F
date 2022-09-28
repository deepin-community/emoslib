C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE EXTGRP(KBUFL,YBUFF, I,K,IVAL,KERR )
C
C**** *EXTGRP*
C
C
C     PURPOSE.
C     --------
C
C         Extract value of group of character
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *EXTGRP(kbufl,ybuff, i,k,ival,kerr )
C
C                    kbufl - size of ybuff in bytes
C                    ybuff - character string
C
C                    I - CONVERSION OF GROUP STARTS IN WORD I
C                        OF ARRAY 'KCHAR' .
C
C                    K - number of characters to convert
C
C
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
C
C     REFERENCE.
C     ----------
C 
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/2003.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
      COMMON /CREXWORK/ NBPT,NGS,NVIND,NBPTSEC2,RVIND,EPS,NE,IEOLD,
     1                  NC07YYY
C
      CHARACTER*(*) YBUFF
      CHARACTER*8 FMT
      REAL*8 RVIND, EPS
C
      DIMENSION NN(5)
C
C     ------------------------------------------------------------------
C*          1.   EXTRACT GROUP OF CHARACTERS.
C                ----------------------------
 100  CONTINUE
C
      IVAL=NVIND
C
      FMT='(I10.10)'
      WRITE(FMT(3:4),'(I2.2)') K
      FMT(6:7)=FMT(3:4)
C
C
c     Check for '-' sign
c
      IF(NE.EQ.1) THEN
         IF(YBUFF(I:I).EQ.'-') THEN
            I=I+1
         END IF
      END IF
c     
c     Check for slash or non figure character
c
      DO IZ=I,I+K-1
      IF(YBUFF(IZ:IZ).EQ.'/') RETURN
      IICH=ICHAR(YBUFF(IZ:IZ)) 
      IF(IICH.LT.48.OR.IICH.GT.57) RETURN
      END DO
c
      READ(YBUFF(I:I+K-1),FMT,IOSTAT=IOS) IVAL
      IF(IOS.NE.0) THEN
         KERR=35
         CALL CREXERR(KERR)
         RETURN
      END IF
C
      IF(YBUFF(I-1:I-1).EQ.'-') IVAL=-IVAL
C
      I=I+K
C
      RETURN
      END
