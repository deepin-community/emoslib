C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXDES(K,KSEC1,KTDLEN,CREXKTDLST,KDLEN,KDATA,KELEM,
     1                   KTDEXL,CREXKTDEXP,CNAMES,CUNITS,KERR)
C
C**** *CREXDES*
C
C
C     PURPOSE.
C     --------
C           EXPAND DATA DESCRIPTORS TO SHOW USER TEMPLATE 
C           DESCRIBED BY ARRAY KTDLST AND KDATA
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *CREXDES(K,KSEC1,KTDLEN,CREXKTDLST,KDLEN,KDATA,KELEM,
C                           KTDEXL,CREXKTDEXP,CNAMES,CUNITS,KERR)*
C
C
C
C        INPUT :
C               *K*       -   AN INTEGER, PRINTING SWITCH 0 - NO PRINT
C                                                         1 - PRINT
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
C               *KTDLEN*  -  Integer number of data descriptors in the list
C               *CREXKTDLST* Integer array of at least ktdlen words
C               *KDLEN*   -  Integer (dimension of kdata array)
C               *KDATA*   -  Integer array containing data needed for data
C                            descriptor expansion (delayed replication factors)
C                            which appear in the values array
C               *KELEM*   -
C        OUTPUT:
C
C               *KTDEXL   -  An integer containing number of expanded elements
C
C               *KTDEXP*  -  An integer array containing expanded list of descriptors
C
C               *CNAMES*  -  Character*64  array of KELEM containing
C                            CREX Table B element names
C               *CUNITS*  -  Character*24 array of KELEM containig
C                            CREX Table B units
C               *VALUES*  -  Real*8 array of KVALS containing expanded
C                            data values
C               *KERR*    -  returned error code
C
C
C
C
C     METHOD.
C     -------
C
C            NONE
C
C     EXTERNALS.
C     ----------
C
C            NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       04/02/2003.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)


      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL=40000,
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /CRWORK/ NBPT,NGS,NVIND,NBPTSEC2,RVIND,EPS,NE,IEOLD,
     1                  NC07YYY
C
      COMMON /CRWT/ NCREXDWINC,NCREXSCAM,NCREXAFDW,NCREXWT ,ODREPF,
     2             NCREXSTACK(JELEM),NCREXWTEN(JELEM),
     3             NCREXWTR (JELEM),NCREXWTS (JELEM),
     4             NCREXWTDW(JELEM)
C
C             NCREXDWINC   -  data width increment
C             NCREXSCAM    -  scale multiplier
C             NCREXAFDW    -  augmented field data width
C             NCREXWT      -  pointer to working table
C             NCREXSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             NCREXWTR     -  working table reference
C             NCREXWTS     -  working scale
C             NCREXWTDW    -  working data width
C
C
C
      COMMON /CRWTC/ CREXWTEN(JELEM),CREXWTU(JELEM)
C
C             CREXWTEN    -  working table element naame
C             CREXWTU     -  working table units
C
      CHARACTER CREXWTEN*64,CREXWTU*24
C
      COMMON /CREL/ NCREXTDLEN,NCREXTDLST(JELEM),NCREXTDEXL,
     1              NCREXTDEXP(JELEM)
C
C             NCREXTDLEN - number of Data descriptors in section 3
C             NCREXTDLST - list of Data descriptors
C             NCREXTDEXL - number of expanded Data Descriptors
C             NCREXTDEXP - list of expanded Data descriptors
C
      COMMON /CRBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values

      CHARACTER*15  FMT
      CHARACTER*6  CREXKTDLST(KELEM)
      CHARACTER*6 CREXKTDEXP(KELEM)
C
      INTEGER IVALUE(KELEM)
C
      INTEGER IVAL,IIVAL
c
      REAL*8 EPS
      REAL*8 RVIND
      REAL*8 VAL
C
      CHARACTER*64 CNAMES(kelem)
      CHARACTER*24 CUNITS(kelem)

      DIMENSION KSEC0(JSEC0),KSEC1(JSEC1)
C
      CHARACTER*4 CECMWF
C
C     ------------------------------------------------------------------
C
c*         1. Set Constants
c             ----------------
 100  CONTINUE
c
      KERR=0
      IF(CECMWF.NE.'ECMF') THEN
         CALL CREXIVAR(KERR)
         CECMWF='ECMF'
      END IF
c
c*         2. Load Crex Tables
c             ----------------
 200  CONTINUE
c
      CALL CREXETAB(KSEC1,KERR)
      IF(KERR.GT.0) RETURN

c
c*         3.1 Expand Data Descriptors
c              -----------------------
c
 310  CONTINUE
c
      KPT=0
      CALL CREXEDD(KPT,KTDLEN,CREXKTDLST,KDLEN,KDATA,KSEC3,
     1             KELEM,CNAMES,CUNITS,KERR)
      IF(KERR.GT.0) RETURN
C
      KTDEXL =M
      DO 301 I=1,KTDEXL
      WRITE(CREXKTDEXP(I),'(i6.6)') NCREXWTR(I)
      IF(CREXKTDEXP(I)(1:1).EQ.'0') CREXKTDEXP(I)(1:1)="B"
 301  CONTINUE
C
      IF(K.EQ.1) THEN
C
         WRITE(*,'(1H ,//)')
         WRITE(*,'(1H ,A)')    '       DATA DESCRIPTORS (UNEXPANDED)'
C
         WRITE(*,'(1H )')
         DO 110 I=1,KTDLEN
          IF(CREXKTDLST(I)(1:1).EQ.'3') CREXKTDLST(I)(1:1)="D"
          IF(CREXKTDLST(I)(1:1).EQ.'2') CREXKTDLST(I)(1:1)="C"
          IF(CREXKTDLST(I)(1:1).EQ.'1') CREXKTDLST(I)(1:1)="R"
          IF(CREXKTDLST(I)(1:1).EQ.'0') CREXKTDLST(I)(1:1)="B"
          WRITE(*,'(1H ,I4,2X,A)') I,CREXKTDLST(I)
 110     CONTINUE
C
         WRITE(*,'(1H ,/)')
         WRITE(*,'(1H ,A)')    '       DATA DESCRIPTORS (EXPANDED)'
         WRITE(*,'(1H )')
         WRITE(*,'(1H ,A,A)')'                ELEMENT NAME',
     1   '                            UNIT'
         WRITE(*,'(1H )')
C
         DO 120 I=1,KTDEXL
          WRITE(*,'(1H ,I5,2X,A,2X,A,2X,A)') I,CREXKTDEXP(I),
     1                      CNAMES(I)(1:40),CUNITS(I)(1:18)
 120     CONTINUE
      END IF
C
C
C     -----------------------------------------------------------------
      RETURN
C
      END
