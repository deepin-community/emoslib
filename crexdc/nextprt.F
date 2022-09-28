C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE NEXTPRT (KBUFL,YBUFF,I,J,KERR )
C
C**** *NEXTPRT*
C
C
C     PURPOSE.
C     --------
C         SCANS BULLETIN IN 'KCHAR' FOR NEXT CHARACTER WHICH
C         IS NOT 'SOH' , 'CR' , 'LF' , 'SPACE' OR 'GS' .
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTPRT(KBUFL,YBUFF,I,J,KERR*
C
C         INPUT     : KBUFL - length of YBUFF in bytes
C                     YBUFF - charcter string
C                     I     - SCAN STARTS AT WORD I.
C                     J     - SCAN STOPS AT WORD J .
C
C         OUTPUT    : I     - POSITION OF REQUIRED CHARACTER
C                     KERR  - ERROR CODE
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
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. D. DRAGOSAVAC    *ECMWF*       07/01/2004.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      CHARACTER*(*) YBUFF
      CHARACTER*1 soh,lf,cr,space,gs,plus,minus
C
C     ------------------------------------------------------------------
C
C*          1.   SCAN BULLETIN.
C                --------------
 100  CONTINUE
C
C     'SOH' = 1 , 'LF' = 10 , 'CR' = 13 , SPACE = 32 , 'GS' = 29.
C     '+' = 43, 'MINUS'='-'
c
      kerr=0
      soh=char(1)
      lf=char(10)
      cr=char(13)
      space=char(32)
      gs=char(29)
      plus=char(43)
      minus='-'
C
      i = iabs(i)
      K = I
      do 101 i=k,j
         if(ybuff(i:i).ne.soh.and.
     1      ybuff(i:i).ne.lf.and.
     2      ybuff(i:i).ne.cr.and.
     3      ybuff(i:i).ne.space.and.
     4      ybuff(i:i).ne.gs.and.
     4      ybuff(i:i).ne.minus.and.
     5      ybuff(i:i).ne.plus)  then
            return
         end if
  101 continue
C
      RETURN
      END
