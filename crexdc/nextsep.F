C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE NEXTSEP(KBUFL,YBUFF,I,J,KERR)
C
C**** *NEXTSEP*
C
C
C     PURPOSE.
C     --------
C         LOCATE THE NEXT SEPARATOR
C         CHARACTER WHICH IS 'CR' OR 'LF' OR 'SPACE' OR '+'
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTSEP(KBUFL,YBUFF,I,J,KERR)*
C         INPUT     : KBUFL - size if ybuff
C                     YBUFF - character string
C                     I     - SCAN STARTS AT byte 'I' OF YBUFF
C                     J     - SCAN ENDS AT byte 'J' OF YBUFF
C
C         OUTPUT    : I     - POSITION OF NEXT 'CR' OR 'LF' OR 'SPACE'
C                             OR '+'

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
C          MILAN DRAGOSAVAC    *ECMWF*       15/02/2003.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      CHARACTER*(*) YBUFF
      CHARACTER*1 cr,lf,space,plus,minus 
C
      COMMON /CREXWORK/ NBPT,NGS,NVIND,NBPTSEC2,RVIND,EPS,NE,IEOLD,
     1                  NC07YYY
      REAL*8 RVIND,EPS
C
C     ------------------------------------------------------------------
C
C*          1.   SCAN BULLETIN.
C                --------------
 100  CONTINUE
C
C     'CR' = 13 , 'LF' = 10 , 'SPACE' = 32 'PLUS' = 43,
C
      cr=char(13)
      lf=char(10)
      space=char(32)
      plus=char(43)
      minus='-'
C
      i=iabs(i)
      k = i
      do 101 i=k,j
       if(NE.eq.0) then
         if(ybuff(I:I).eq.cr.or.
     1      ybuff(I:I).eq.lf.or.
     2      ybuff(I:I).eq.space.or.
     3      ybuff(I:I).eq.minus.or.
     3      ybuff(I:I).eq.plus) return
       else
         if(ybuff(I:I).eq.cr.or.
     1      ybuff(I:I).eq.lf.or.
     2      ybuff(I:I).eq.space.or.
     3      ybuff(I:I).eq.plus) return
       end if
  101 continue
C
      return
      end
