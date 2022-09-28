C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXS2(KBUFL,YBUFF,KSUP,KSEC3,KELEM,CNAMES,CUNITS,KERR)
C
C**** *CREXS2*
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
C          *CALL* *CREXS2( KBUFL,YBUFF,KSUP,KSEC3,KELEM,CNAMES,CUNITS,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of crex message (words)
C               *YBUFF*   -  array containing CREX message
C               *KELEM*   -  dimension of CNAMES, CUNITS array
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
C               *KSEC3*   -  array containing section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data compression)
C               *CNAMES*  -  character array containing element names
C               *CUNITS*  -  character array containig units
C               *KERR*    -  returned error code
C
C     METHOD.
C      -------
C
C          Expands list of data descriptors in section 1
C          of Crex message. Working tables for further data decoding are set,
C          list of Crex data descriptors and a list of expanded Crex data descriptors
C          according to table D reference are returned respectively.
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
C             NCREXDR    - table D,  table reference              array
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
      COMMON /CREXBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      COMMON /CREXWT/ NCREXDWINC,NCREXSCAM,NCREXAFDW,NCREXWT ,ODREPF,
     2               NCREXSTACK(JELEM),NCREXWTEN(JELEM),
     3               NCREXWTR (JELEM),NCREXWTS (JELEM),NCREXWTRV(JELEM),
     4               NCREXWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NCREXDWINC   -  data width increment
C             NCREXSCAM    -  scale multiplier
C             NCREXAFDW    -  augmented field data width
C             NCREXWT      -  pointer to working table
C             NCREXSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             NCREXWTR     -  working table reference
C             NCREXWTS     -  working scale
C             NCREXWTRV    -  working reference value
C             NCREXWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
      CHARACTER CREXWTEN*64,CREXWTU*24
C
C
      COMMON /CREXWTC/ CREXWTEN(JELEM),CREXWTU (JELEM)
C
C             CREXWTEN    -  working table element naame
C             CREXWTU     -  working table units
C
      COMMON /CREXEL/ NCREXTDLEN,NCREXTDLST(JELEM),NCREXTDEXL,
     1                NCREXTDEXP(JELEM)
C
C             NCREXTDLEN - number of Data descriptors in section 3
C             NCREXTDLST - list of Data descriptors
C             NCREXTDEXL - number of expanded Data Descriptors
C             NCREXTDEXP - list of expanded Data descriptors
C
C
      COMMON /CREXOFF/ NSIZEG(JELEM),NCREXWTRG(JWORK),
     1                 NBP,NSUBSET,OMULTI
C
      CHARACTER*64 CNAMES(KELEM),CREXNAME
      CHARACTER*24 CUNITS(KELEM),CREXUNIT
      CHARACTER*(*) YBUFF
      CHARACTER*6 CREXLST
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
C     -----------------------------------------------------------------
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
      NCREXWT=0
      IF( NSUBSET.EQ.1) THEN
          NBPTSEC2=NBPT
      END IF
C
C
C*          2.2  UNPACK AND PUT DATA DESCRIPTORS IN STACK.
C                -----------------------------------------
 220  CONTINUE
C
      DO 221 JJ=1,NCREXTDLEN
C
      if(CREXLST(JJ)(1:1).eq.'D') CREXLST(JJ)(1:1)='3'
      if(CREXLST(JJ)(1:1).eq.'B') CREXLST(JJ)(1:1)='0'
      if(CREXLST(JJ)(1:1).eq.'C') CREXLST(JJ)(1:1)='2'
      if(CREXLST(JJ)(1:1).eq.'R') CREXLST(JJ)(1:1)='1'
c
      IOS=0
      READ( CREXLST(JJ),'(i6.6)',IOSTAT=IOS) ISTACK(JJ)
      IF(IOS.NE.0) THEN
         KERR=35
         CALL CREXERR(KERR)
         print*,'Internal read error in CREXLST.'
         print*,'NCREXTDLEN=ktdlen=',NCREXTDLEN
         print*,jj,' CREXLST(JJ)=',CREXLST(JJ)
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
         CALL CREXUPWT(IDD,KELEM,KERR)
         IF(KERR.GT.0) RETURN
C       
      ELSEIF( IF.EQ.1) THEN
C
C*          2.4.1     SOLVE REPLICATION PROBLEM.
C                     --------------------------
C
C
         CALL CREXSRP(KBUFL,YBUFF,KSEC3,J,JMAX,
     1                IDD,ISTACK,KELEM,KERR)
         IF(KERR.GT.0) RETURN
C
      ELSEIF( IF.EQ.2) THEN
C
C*          2.5.3 PROCESS OPERATOR.
C                 -----------------
            CALL CREXPRCO(KBUFL,YBUFF,J,IDD,ISTACK,KELEM,KERR)
            IF(KERR.GT.0) RETURN
C
      ELSEIF( IF.EQ.3) THEN
C
C*          2.5.2 REPLACE BY LIST OF DESCRIPTORS FROM TABLE *D.
C                 ---------------------------------------------
            CALL CREXSTDR(J,JMAX,IDD,ISTACK,KERR)
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
c
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
      DO 303 I=1,NCREXTDEXL
      IJ=I+(NSUBSET-1)*KELEM
      NCREXWTRG(IJ)=NCREXWTR(I)
 303  CONTINUE
C
      NSIZEG(NSUBSET)  =M

      KSUP(3)= 4
      KSUP(5)= M
      KSUP(6)= KSEC3(3)
C
      RETURN
      END
