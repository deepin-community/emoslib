C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXS0( KBUFL,YBUFF,KSUP,KSEC0,KERR)
C
C**** *BUEXS0*
C
C
C     PURPOSE.
C     --------
C          Expands section 0 of Crex message. 
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *CREXS0( KBUFL,YBUFF,KSUP,KSEC0,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of CREX message (words)
C               *YBUFF*   -  Character string containing Crex message
C        OUTPUT:
C               *KSUP*    -  Integer array of 9 words containing
C                            suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- Reserved
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- Reserved
C                         -  KSUP( 5) -- M (number of elements in values
C                                           array, first index)
C                         -  KSUP( 6) -- N (number of subsets,second index
C                                           of values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total CREX message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C
C               *KSEC0*   -  array containing section 0 information
C                            KSEC0( 1)-- length of section 0 (bytes)
C                            KSEC0( 2)-- total length of CREX message (bytes)
C                            KSEC0( 3)-- Not used
C
C               *KERR*    -  returned error code
C
C     METHOD.
C     --------
C
C          NONE.
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
      COMMON /CREXWORK/ NBPT,NGS,NVIND,NBPTSEC2,RVIND,EPS,NE,IEOLD,
     1                  NC07YYY
C
      DIMENSION KSEC0(*)
      DIMENSION KSUP(*)
      CHARACTER*(*) YBUFF
C
      CHARACTER*4 YCREX
      REAL*8 RVIND,EPS
C
C     ------------------------------------------------------------------
C*          1.   EXPAND SECTION 0.
C                -----------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      KSEC0(1)=4
      KSEC0(2)=KBUFL
      KSUP(8) =KBUFL
C
C*          1.1  INITIALIZE WORKING POINTERS NWPT AND NBPT.
C                ------------------------------------------
      NBPT = 1
C
C*          1.2  UNPACK FIRST FOUR OCTETS CONTAINING *CREX*.
C                -------------------------------------------
C
      IF(YBUFF(NBPT:NBPT+3).ne.'CREX') then
         KERR=1
         print*,'This bulletin is not CREX message.'
         RETURN
      END IF
C
C           1.2 Position pointer to begining of section1.
C               ---------------------------------------------
 120  CONTINUE
C
      NBPT=NBPT+4
      IF(NBPT.GT.KBUFL) THEN
         KERR=26
         print*,'CREXS0:'
         CALL CREXERR(KERR)
         RETURN
      END IF
C
      IRET=0
      CALL NEXTSEC(KBUFL,YBUFF,NBPT,NGS,KERR)
      IF(KERR.NE.0) THEN
         KERR=40
         print*,'NEXTSEC :' 
         CALL CREXERR(KERR)
         RETURN
      END IF
C
      CALL NEXTGRP(KBUFL,YBUFF,NBPT,NGS,KERR)
      IF(KERR.NE.0) THEN
         KERR=41
         print*,'NEXTGRP : '
         CALL CREXERR(KERR)
         RETURN
      END IF
C
      RETURN
C     -----------------------------------------------------------------
C
      END
