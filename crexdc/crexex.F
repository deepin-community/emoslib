C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXEX(KBUFL,YBUFF,KSUP,KSEC0,KSEC1,KSEC3,
     2                  KELEM,CNAMES,CUNITS,KVALS,VALUES,CVALS,KERR)
C
C**** *CREXEX*
C
C
C     PURPOSE.
C     --------
C          Decode Crex message into fully expanded form; returning
C          information relevant for all CREX sections, expanded values,
C          their names and units.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *CREXEX(KBUFL,YBUFF,KSUP,KSEC0,KSEC1,KSEC3,
C     1                   KELEM,CNAMES,CUNITS,KVALS,VALUES,CVALS,KERR)*
C
C        INPUT :
C               *KBUFL*   -  Length of CREX message (words)
C               *YBUFF*   -  Character string  containing Crex message
C               *KELEM*   -  Integer (expected number of expanded elements)
C               *KVALS*   -  Integer (expected number of data values)
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
C               *KSEC0*   -  Integer array of 3 words containing
C                            CREX section 0 information
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
C
C               *KSEC3*   -  Integer array of 4 words containing
C
C                            KSEC3( 1)-- Reserved
C                            KSEC3( 2)-- Reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- Reserved
C
C
C               *CNAMES*  -  Character*64  array of KELEM containing
C                            CREX Table B element names
C               *CUNITS*  -  Character*24 array of KELEM containig
C                            CREX Table B units
C               *VALUES*  -  Real*8 array of KVALS containing expanded
C                            data values
C               *CVALS*   -  Character*80 array of KVALS containing
C                            CREX code table or CCITTIA5 CREX elements
C                            entries
C               *KERR*    -  returned error code
C
C
C     METHOD.
C     -------
C
C          Crex message passed as argument to this routine is decoded
C          section by section. Suplementary information and expanded data
C          are returned as well as error code.
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
      DIMENSION KSEC3(JSEC3)
C
      COMMON /CREXOFF/ NSIZEG(JELEM),NCREXWTRG(JWORK),
     1                 NBP,NSUBSET,OMULTI
C
      REAL*8 VALUES(KVALS)
C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
C
      CHARACTER*(*) YBUFF
C
      CHARACTER*4 CECMWF
C
C     ------------------------------------------------------------------
C
C*          1.   SET CONSTANTS.
C                --------------
 100  CONTINUE
C
      KERR=0
C
      M0=1
C
      DO 101 I=1,JSEC0
      KSEC0(I)=0
 101  CONTINUE
C
      DO 102 I=1,JSEC1
      KSEC1(I)=0
 102  CONTINUE
C
      DO 103 I=1,JSEC3
      KSEC3(I)=0
 103  CONTINUE
C
      DO 104 I=1,JSUP
      KSUP(I)=0
 104  CONTINUE
C
C
      CALL CREXINIT(KBUFL,KERR)
C
C     -----------------------------------------------------------------
C*          2.  EXPAND SECTION 0.
C               -----------------
 200  CONTINUE
C
      CALL CREXS0( KBUFL,YBUFF,KSUP,KSEC0,KERR )
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C*          3.   EXPAND SECTION 1. 
C                ------------------
 300  CONTINUE
C
      CALL CREXS1( KBUFL,YBUFF,KSUP,KSEC0,KSEC1,KERR )
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C*          4.   LOAD CREX TABLES.
C                -----------------
 400  CONTINUE
C
      CALL CREXTB(KSEC1,KERR)
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C*          5.  EXPAND SECTION 2.
C               -----------------
 500  CONTINUE
C
      OMULTI=.TRUE.
      NSUBSET=0
C
      DO WHILE(OMULTI)
C
      NSUBSET=NSUBSET+1
C
      CALL CREXS2(KBUFL,YBUFF,KSUP,KSEC3,KELEM,CNAMES,CUNITS,KERR)
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C
      CALL CREXS2DATA(KBUFL ,YBUFF ,KSUP  ,KSEC3,
     1            KELEM ,CNAMES,CUNITS,KVALS,VALUES,CVALS,KERR)
      IF(KERR.GT.0) RETURN
C
      END DO
C
C     ------------------------------------------------------------------
C*          6.   EXPAND SECTION 3.
C                -----------------
 600  CONTINUE
C
      CALL CREXS5(KBUFL,YBUFF,KERR)
      IF(KERR.GT.0) RETURN
C
C     -----------------------------------------------------------------
      RETURN
C
      END
