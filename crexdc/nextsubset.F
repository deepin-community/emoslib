C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE NEXTSUBSET(KBUFL,YBUFF, I,J,KERR )
C
C**** *NEXTSUBSET*
C
C
C     PURPOSE.
C     --------
C
C         SCANS BULLETIN IN 'KCHAR' FOR NEXT '+CrCrLf' CHARACTER.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTSUBSET((KBUFL,YBUFF, I,J,KERR)*
C         INPUT     : KBUFL - size of ybuff in bytes
C                     YBUFF - character string
C                     I     - scan starts at character I.
C                     J     - scan stops at character J .
C
C         OUTPUT    : I     - position of required character
C                             character not found.
C                     KERR  - return error code

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
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          MILAN DRAGOSAVAC    *ECMWF*      05/11/2003
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      character*(*) ybuff
      character*1 plus
      character*4 plcrlf 
C
C     ------------------------------------------------------------------
C*          1.   SCANE BULLETIN FOR NEXT '+CrCrLf'
C                ---------------------------------
 100  CONTINUE
C
      plus='+'
      plcrlf='+'//char(13)//char(13)//char(10)
C

      i = iabs(i)
      k = i
      do 101 i=k,j
         if(ybuff(i-1:i-1).ne.plus.and.ybuff(i:i).eq.plus.and.
     1      ybuff(i+1:i+1).ne.plus) then
            return
         end if
  101 continue
C
      I=J
      RETURN
      END
