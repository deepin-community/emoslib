C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE NEXTSEC(KBUFL,YBUFF, I,J,KERR )
C
C**** *NEXTSEC*
C
C
C     PURPOSE.
C     --------
C
C         Scan ybuff for '++' characters
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTSEC(I,J)*
C         INPUT     : KBUFL - size of ybuff in bytes
C                     YBUFF - character string
C                     I     - scan starts at word I.
C                     J     - scan stops at word J .
C
C         OUTPUT    : I     - position of required character.
C                     KERR  - error code

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
C          MILAN DRAGOSAVAC    *ECMWF*      07/01/2004.
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
      CHARACTER*(*) YBUFF
      CHARACTER*2 plusplus
C
C     ------------------------------------------------------------------
C*          1.   SCANE BULLETIN FOR NEXT '=' SIGN.
C                ---------------------------------
 100  CONTINUE
C
      plusplus='++'
C
      
      i = iabs(i)
      k = i
      do 101 i=k,j
         if(ybuff(i:i+1).eq.plusplus) return
  101 continue
C
      i=j
      RETURN
      END
