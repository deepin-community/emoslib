C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXEDD(KPT,KTDLEN,CREXLST,KDLEN,KDATA,KSEC3,
     1             KELEM,CNAMES,CUNITS,KERR)
C
C**** *CREXEDD*
C
C
C     PURPOSE.
C     --------
C
C          Expand Crex data descriptors
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *CREXETD( KPT,KTDLEN,CREXLST,KDLEN,KDATA,KSEC3,
C                           KVALS,VALUES,KELEM,KERR)*
C
C        INPUT :
C               *KPT*     -  POINTER TO KDATA ARRAY
C               *KTDLEN*  -  number of data descriptors in section 1
C               *KTDLST*  -  array containing data descriptors in section 1
C               *KDLEN*   -  DIMENSION OF KDATA ARRAY
C               *KDATA*   -  ARRAY CONTAINING DATA NEEDED FOR DESCRIPTOR
C                            EXPANSION ( delayed replications)
C               *KSEC3*   -  Integer array of 4 words containing
C
C                            KSEC3( 1)-- Reserved
C                            KSEC3( 2)-- Reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- Reserved
C                            data values
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C        OUTPUT:
C               *KERR*    -  returned error code
C
C     METHOD.
C      -------
C
C          Expands list of data descriptors in section 1
C     of Crex message. Working tables for further data decoding are set,
C     list of Crex data descriptors and a list of expanded Crex data descriptors
C     according to table D reference are returned respectively.
C
C
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
C------------------------------------------------------------------------------------
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
C
      COMMON /CRWORK/ NBPT,NGS,NVIND,NBPTSEC2,RVIND,EPS,NE,IEOLD,
     1                  NC07YYY
C
      COMMON/CRTAB/ NCREXBR(JTAB),NCREXBS(JTAB)    , NCREXBDW(JTAB),
     1              NCREXDR(JTAB),NCREXDST(JTAB)   ,
     2              NCREXDL(JTAB),NCREXDSQ(JTAB*20),NCREXP(64,255)

C
C             NCREXBR     - table B,  table reference              array
C             NCREXBS     - table B,  scale                        array
C             NCREXBDW    - table B,  data width                   array
C             NCREXDR     - table D,  table reference              array
C             NCREXDST    - table D,  starting pointers            array
C             NCREXDL     - table D,  lengths                      array
C             NCREXDSQ    - table D,  list of sequence descriptors array
C
      COMMON /CRTABC/ CREXNAME(JTAB),CREXUNIT(JTAB)
C
C             CREXNAME      -  table B, ELEMENT NAME           array
C             CREXUNIT      -  table B, unit                   array
c
C
      COMMON /CRBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      COMMON /CRWT/ NCREXDWINC,NCREXSCAM,NCREXAFDW,NCREXWT ,ODREPF,
     2              NCREXSTACK(JELEM),NCREXWTEN(JELEM),
     3              NCREXWTR (JELEM),NCREXWTS (JELEM),
     4              NCREXWTDW(JELEM)
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
C
      COMMON /CREL/ NCREXTDLEN,NCREXTDLST(JELEM),NCREXTDEXL,
     1              NCREXTDEXP(JELEM)
C
C             NCREXTDLEN - number of Data descriptors in section 3
C             NCREXTDLST - list of Data descriptors
C             NCREXTDEXL - number of expanded Data Descriptors
C             NCREXTDEXP - list of expanded Data descriptors
C
      CHARACTER*64 CREXWTEN
      CHARACTER*24 CREXWTU
C
      CHARACTER*64 CREXNAME
      CHARACTER*24 CREXUNIT
C
      CHARACTER*(*) CREXLST(*)
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
C
      DIMENSION ISTACK(JELEM),IISTACK(JELEM)
C
      DIMENSION KSUP(JSUP),KSEC3(JSEC3)
C
      REAL*8 RVIND,EPS
c
C     ------------------------------------------------------------------
C
C*          1.   EXPAND PRELIMINARY ITEMS OF SECTION 3.
C                --------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
CC     -----------------------------------------------------------------
C
C*          2.   EXPAND DATA DESCRIPTORS.
C                ------------------------
 200  CONTINUE
C
C
C*          2.1  CALCULATE EXPECTED NUMBER OF DATA DESCRIPTORS.
C                ----------------------------------------------
C                AND INITIALIZE NUMBER OF DATA VALUES PER SUB-SET.
C                -------------------------------------------------
 210  CONTINUE
C
      J = 0
      M = 0
      KPT=0
      NCREXWT=0
      NCREXTDLEN=ktdlen
C
C
C*          2.2  PUT DATA DESCRIPTORS IN STACK.
C                -----------------------------------------
 220  CONTINUE
C
      DO 221 JJ=1,KTDLEN
C
      if(CREXLST(JJ)(1:1).eq.'D') CREXLST(JJ)(1:1)='3'
      if(CREXLST(JJ)(1:1).eq.'B') CREXLST(JJ)(1:1)='0'
      if(CREXLST(JJ)(1:1).eq.'C') CREXLST(JJ)(1:1)='2'
      if(CREXLST(JJ)(1:1).eq.'R') CREXLST(JJ)(1:1)='1'
c
      IOS=0
      READ( CREXLST(JJ),'(i6.6)',IOSTAT=IOS) ISTACK(JJ)
      IF(IOS.NE.0) THEN
         print*,'Internal read error in CREXLST.'
         print*,'NCREXTDLEN=ktdlen=',NCREXTDLEN
         print*,jj,' CREXLST(JJ)=',CREXLST(JJ)
         kerr=35
         RETURN
      END IF
C
      IISTACK(JJ)=ISTACK(JJ)
C
 221  CONTINUE
C
      JMAX=NCREXTDLEN
C     ------------------------------------------------------------------
C*          2.3  GET NEXT DESCRIPTOR FROM THE STACK.
C                -----------------------------------
 230  CONTINUE
C
C
      J   = J + 1
      IF(J.GT.JMAX) GO TO 300
C
      IDD = ISTACK(J)
      IF(IDD.EQ.0)  GO TO 230
C
      IF = IDD/100000
C
C     ------------------------------------------------------------------
C*          2.4  CHECK IF IT IS REPLICATION DESCRIPTOR.
C                --------------------------------------
 240  CONTINUE
C
      IF( IF.EQ.0) THEN
C
C*          2.6  ELEMENT DESCRIPTOR, SO UPDATE WORKING TABLE.
C                --------------------------------------------
 260     CONTINUE
C
         CALL CREXEPWT(IDD,KERR)
         IF(KERR.GT.0) RETURN
C
      ELSEIF( IF.EQ.1) THEN
C
C*          2.4.1     SOLVE REPLICATION PROBLEM.
C                     --------------------------
C
C
         CALL CREXREP(KPT,KDLEN,KDATA,J,JMAX,IDD,ISTACK,KERR)
         IF(KERR.GT.0) RETURN
C
      ELSEIF( IF.EQ.2) THEN
C
C*          2.5.3 PROCESS OPERATOR.
C                 -----------------
            CALL CREXOPER(KPT,KDLEN,KDATA,J,IDD,ISTACK,KERR)
            IF(KERR.GT.0) RETURN
C
      ELSEIF( IF.EQ.3) THEN
C
C*          2.5.2 REPLACE BY LIST OF DESCRIPTORS FROM TABLE *D.
C                 ---------------------------------------------
            CALL CREXETDR(J,JMAX,IDD,ISTACK,KERR)
            IF(KERR.GT.0) THEN
               DO 252 IQ=1,JELEM
               NCREXSTACK(IQ)=0.
 252           CONTINUE
               RETURN
            END IF
      ELSE
         KERR=37
         CALL CREXERR(KERR)
         RETURN
      END IF
C
      GO TO 230
C
C     ------------------------------------------------------------------
C
C*          3. COLLECT  SUPPLEMENTARY ITEMS.
C              -----------------------------
 300  CONTINUE
C
      NCREXTDEXL =M
      DO 301 I=1,NCREXTDEXL
      NCREXTDEXP(I)=NCREXWTR(I)
      CNAMES(I)=CREXWTEN(I)
      CUNITS(I)=CREXWTU(I)
 301  CONTINUE
C
      DO 302 I=1,NCREXTDLEN
      NCREXTDLST (I)=IISTACK(I)
 302  CONTINUE
C
      KSUP(3)= 4
      KSUP(5)= M
      KSEC3(3)=NSUBSET
      KSUP(6)= KSEC3(3)
C
      RETURN
C
      END
