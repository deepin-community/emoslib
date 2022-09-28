C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXS1( KBUFL,YBUFF,KSUP,KSEC0,KSEC1,KERR)
C
C**** *CREXS1*
C
C
C     PURPOSE.
C     --------
C          Expands section 1 of CREX message. Saves expanded information
C          in the array KSEC1.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEXS1( KBUFL,YBUFF,KSUP,KSEC0,KSEC1,KERR)*
C
C          INPUT :
C               *KBUFL*   -  length of CREX message (words)
C               *YBUFF*   -  array containing CREX message
C          OUTPUT:
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
C                            KSEC0( 3)-- CREX Edition number
C
C               *KSEC1*   -  Integer array of at least 40 words
C                            containing CREX section 1 information
C                            KSEC1( 1)-- Reserved
C                            KSEC1( 2)-- CREX Edition number
C                            KSEC1( 3)-- Reserved
C                            KSEC1( 4)-- Reserved
C                            KSEC1( 5)-- Reserved
C                            KSEC1( 6)-- CREX message type
C                            KSEC1( 7)-- Reserved
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- Reserved
C                            KSEC1(10)-- Reserved
C                            KSEC1(11)-- Reserved
C                            KSEC1(12)-- Reserved
C                            KSEC1(13)-- Reserved
C                            KSEC1(14)-- CREX Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) - KSEC1(40) -- Reserved
C
C
C                            Crex Edition 2
C
C               *KSEC1*   -  Integer array of at least 40 words
C                            containing CREX section 1 information
C                            KSEC1( 1)-- Reserved
C                            KSEC1( 2)-- CREX Edition number
C                            KSEC1( 3)-- Originating Centre ( oooo)
C                            KSEC1( 4)-- Update sequence number (uu)
C                            KSEC1( 5)-- Number of subsets (sss)
C                            KSEC1( 6)-- CREX data category (nnn)
C                            KSEC1( 7)-- International data sub-category (mmm)
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- Year (yyyy)
C                            KSEC1(10)-- Month (mm)
C                            KSEC1(11)-- Day (dd)
C                            KSEC1(12)-- Hour (hh)
C                            KSEC1(13)-- Minute (mm)
C                            KSEC1(14)-- CREX Master table (tt)
C                            KSEC1(15)-- version number of Master table used (vv)
C                            KSEC1(16)-- Originating sub-centre (ppp)
C                            KSEC1(17)-- Bufr master table version number
C                            KSEC1(18)-- Bufr version number of local table used
C                            KSEC1(19) - KSEC1(40) -- Reserved
C
C               *KERR*    -  Return error code
C
C      METHOD.
C      -------
C
C           NONE.
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
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL=40000,
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      DIMENSION KSUP(JSUP),KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION ISEC1(JSEC1)
C
      COMMON /CREXWORK/ NBPT,NGS,NVIND,NBPTSEC2,RVIND,EPS,NE,IEOLD,
     1                  NC07YYY
C
      COMMON/CREXTAB/ NCREXBR(JTAB),NCREXBS(JTAB)    , NCREXBDW(JTAB),
     1                NCREXDR(JTAB),NCREXDST(JTAB)   ,
     2                NCREXDL(JTAB),NCREXDSQ(JTAB*20),NCREXP(64,255)

C
C             NCREXBR     - table B,  table reference              array
C             NCREXBS     - table B,  scale                        array
C             NCREXBDW    - table B,  data width                   array
C             NCREXDR     - table D,  table reference              array
C             NCREXDST    - table D,  starting pointers            array
C             NCREXDL     - table D,  lengths                      array
C             NCREXDSQ    - table D,  list of sequence descriptors array
C
       COMMON/CREXTABC/ CREXNAME(JTAB),CREXUNIT(JTAB),CREXLST(JELEM)
C
C             CREXNAME      -  table B, ELEMENT NAME           array
C             CREXUNIT      -  table B, unit                   array
c
C
      COMMON /CREXEL/ NCREXTDLEN,NCREXTDLST(JELEM),NCREXTDEXL,
     1                NCREXTDEXP(JELEM)
c
      CHARACTER*6 CREXLST
      CHARACTER*64 CREXNAME 
      CHARACTER*24 CREXUNIT
      REAL*8 RVIND,EPS
c
      CHARACTER*(*) YBUFF
C
C     ------------------------------------------------------------------
C*          1.  EXPAND SECTION 1.
C               ------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      IF(YBUFF(NBPT:NBPT).NE.'T') THEN
         KERR=42
         CALL CREXERR(KERR)
         RETURN
      END IF
C
      NBPT=NBPT+1
      ITEMP=NBPT
      CALL NEXTSEC(KBUFL,YBUFF, ITEMP,NGS,KERR )
      IF(KERR.NE.0) THEN
         KERR=40 
         CALL CREXERR(KERR)
         RETURN
      END IF
C
      IF(ITEMP.GT.NGS) THEN
         KERR=43
         CALL CREXERR(KERR)
         RETURN
      END IF
C
C
C     Get Master table used tt
C
      CALL EXTGRP(KBUFL,YBUFF, NBPT,2,KSEC1(14),KERR )
      IF(KERR.NE.0) THEN
         KERR=44
         CALL CREXERR(KERR)
         RETURN
      END IF
C
C     Get CREX Edition number  ee
C
      CALL EXTGRP(KBUFL,YBUFF, NBPT,2,KSEC1(2),KERR)
      IF(KERR.NE.0) THEN
         KERR=45
         CALL CREXERR(KERR)
         RETURN
      END IF
      KSEC0(3)=KSEC1(2)
C
C     Get Version number of master table used  vv
C
      CALL EXTGRP(KBUFL,YBUFF, NBPT,2,KSEC1(15),KERR)
      IF(KERR.NE.0) THEN
         KERR=46
         CALL CREXERR(KERR)
         RETURN
      END IF
C
      IF(KSEC1(2).EQ.2 ) THEN
C
C        Bufr master table version number  bb
C
         CALL EXTGRP(KBUFL,YBUFF, NBPT,2,KSEC1(17),KERR)
         IF(KERR.NE.0) THEN
            KERR=46
            CALL CREXERR(KERR)
            RETURN
         END IF
C
C        Bufr version number of local table ww
C
         CALL EXTGRP(KBUFL,YBUFF, NBPT,2,KSEC1(18),KERR)
         IF(KERR.NE.0) THEN
            KERR=46
            CALL CREXERR(KERR)
            RETURN
         END IF

      END IF

      CALL NEXTSEP(KBUFL,YBUFF,NBPT,ITEMP,KERR)
      CALL NEXTPRT(KBUFL,YBUFF,NBPT,ITEMP,KERR )
      IF(KERR.NE.0) THEN
         print*,'nextprt: error ',ierr
         RETURN
      END IF
C
      IF(YBUFF(NBPT:NBPT).NE.'A') THEN
         print*,'CREX table A not found.'
         RETURN
      END IF
C
      NBPT=NBPT+1
C
C     Get CREX table A   nnn
C
      CALL EXTGRP(KBUFL,YBUFF, NBPT,3,KSEC1(6),KERR)
      IF(KERR.NE.0) THEN
         print*,'extgrp : error ',kerr
         RETURN
      END IF
      IF(KSEC1(2).eq.2) THEN
C
C        Get international sub-category mmm
C
         CALL EXTGRP(KBUFL,YBUFF, NBPT,3,KSEC1(7),KERR)
         IF(KERR.NE.0) THEN
            print*,'extgrp : error ',kerr
            RETURN
         END IF

         CALL NEXTSEP(KBUFL,YBUFF,NBPT,ITEMP,KERR)
         CALL NEXTPRT(KBUFL,YBUFF,NBPT,ITEMP,KERR )
         IF(KERR.NE.0) THEN
            print*,'nextprt: error ',ierr
            RETURN
         END IF
C
         IF(YBUFF(NBPT:NBPT).NE.'P') THEN
            print*,'CREX oiginating centre not found.'
            RETURN
         END IF
C
         NBPT=NBPT+1
C
C        Get originating centre  ooooo
C
         CALL EXTGRP(KBUFL,YBUFF, NBPT,5,KSEC1(3),KERR)
         IF(KERR.NE.0) THEN
            print*,'extgrp : error ',kerr
            RETURN
         END IF
C
C        Get originating sub-centre ppp
C
         CALL EXTGRP(KBUFL,YBUFF, NBPT,3,KSEC1(16),KERR)
         IF(KERR.NE.0) THEN
            print*,'extgrp : error ',kerr
            RETURN
         END IF

         CALL NEXTSEP(KBUFL,YBUFF,NBPT,ITEMP,KERR)
         CALL NEXTPRT(KBUFL,YBUFF,NBPT,ITEMP,KERR )
         IF(KERR.NE.0) THEN
            print*,'nextprt: error ',ierr
            RETURN
         END IF
C
         IF(YBUFF(NBPT:NBPT).NE.'U') THEN
            print*,'CREX update sequence number not found.'
            RETURN
         END IF
C
         NBPT=NBPT+1
C
C        Get update sequence number uu
C
         CALL EXTGRP(KBUFL,YBUFF, NBPT,2,KSEC1(4),KERR)
         IF(KERR.NE.0) THEN
            print*,'extgrp : error ',kerr
            RETURN
         END IF
C
         CALL NEXTSEP(KBUFL,YBUFF,NBPT,ITEMP,KERR)
         CALL NEXTPRT(KBUFL,YBUFF,NBPT,ITEMP,KERR )
         IF(KERR.NE.0) THEN
            print*,'nextprt: error ',ierr
            RETURN
         END IF
C
         IF(YBUFF(NBPT:NBPT).NE.'S') THEN
            print*,'CREX number of subsets not found.'
            RETURN
         END IF
C
         NBPT=NBPT+1
C          Get number of subsets
C
         CALL EXTGRP(KBUFL,YBUFF, NBPT,3,KSEC1(5),KERR)
         IF(KERR.NE.0) THEN
            print*,'extgrp : error ',kerr
            RETURN
         END IF
C
         CALL NEXTSEP(KBUFL,YBUFF,NBPT,ITEMP,KERR)
         CALL NEXTPRT(KBUFL,YBUFF,NBPT,ITEMP,KERR )
         IF(KERR.NE.0) THEN
            print*,'nextprt: error ',ierr
            RETURN
         END IF

         IF(YBUFF(NBPT:NBPT).NE.'Y') THEN
            print*,'CREX date not found.'
            RETURN
         END IF

C
         NBPT=NBPT+1
C
C        Get CREX date
C
         CALL EXTGRP(KBUFL,YBUFF, NBPT,4,KSEC1(9),KERR)
         IF(KERR.NE.0) THEN
            print*,'extgrp : error ',kerr
            RETURN
         END IF
         CALL EXTGRP(KBUFL,YBUFF, NBPT,2,KSEC1(10),KERR)
         IF(KERR.NE.0) THEN
            print*,'extgrp : error ',kerr
            RETURN
         END IF
C
         CALL EXTGRP(KBUFL,YBUFF, NBPT,2,KSEC1(11),KERR)
         IF(KERR.NE.0) THEN
            print*,'extgrp : error ',kerr
            RETURN
         END IF

         CALL NEXTSEP(KBUFL,YBUFF,NBPT,ITEMP,KERR)
         CALL NEXTPRT(KBUFL,YBUFF,NBPT,ITEMP,KERR )
         IF(KERR.NE.0) THEN
            print*,'nextprt: error ',ierr
            RETURN
         END IF
C
         IF(YBUFF(NBPT:NBPT).NE.'H') THEN
            print*,'CREX time not found.'
            RETURN
         END IF
C
         NBPT=NBPT+1
C
C        Get CREX time
C
         CALL EXTGRP(KBUFL,YBUFF, NBPT,2,KSEC1(12),KERR)
         IF(KERR.NE.0) THEN
            print*,'extgrp : error ',kerr
            RETURN
         END IF
         CALL EXTGRP(KBUFL,YBUFF, NBPT,2,KSEC1(13),KERR)
         IF(KERR.NE.0) THEN
            print*,'extgrp : error ',kerr
            RETURN
         END IF

C
      END IF
C
      KTDLEN=0
C
 200  CONTINUE
C
      CALL NEXTSEP(KBUFL,YBUFF,NBPT,ITEMP,KERR)
      CALL NEXTPRT(KBUFL,YBUFF,NBPT,ITEMP,KERR )
      IF(KERR.NE.0) THEN
         print*,'nextprt: error ',ierr
         RETURN
      END IF
C
      IF(NBPT.GE.ITEMP) GO TO 300
C
      IF(YBUFF(NBPT:NBPT).EQ.'E') THEN
         NE=1
         GO TO 200 
      END IF
      KTDLEN=KTDLEN+1
      CALL EXTGRPC(KBUFL,YBUFF, NBPT,6,CREXLST(KTDLEN),KERR)
      IF(KERR.NE.0) THEN
         print*,'extgrpc : error ',kerr
         RETURN
      END IF
C
      GO TO 200
C
 300  CONTINUE
C
      NCREXTDLEN=KTDLEN 
C     ------------------------------------------------------------------
C
      RETURN
      END
