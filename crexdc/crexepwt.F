C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXEPWT(KDD,KERR)
C
C**** *CREXEPWT*
C
C
C     PURPOSE.
C     --------
C          UPDATES WORKING TABLES SETTING ELEMENT NAME,UNIT,SCALE,
C     REFERENCE VALUE AND DATA WIDTH.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *CREXEPWT(KDD,KELEM,KERR)*
C
C        INPUT :
C               *KDD*     -  DATA DESCRIPTOR
C        OUTPUT:
C               *KERR*    -  RETURN ERROR CODE
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
C          NONE.
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
     1          JSEC4=2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL=40000,
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
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
C
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
C
C

C      COMMON /CRTABC/ CREXNAME(JTAB),CREXUNIT(JTAB),CREXLST(JELEM)
       COMMON /CRTABC/ CREXNAME(JTAB),CREXUNIT(JTAB)
C
C             CREXNAME      -  table B, ELEMENT NAME           array
C             CREXUNIT      -  table B, unit                   array
C
      COMMON /CRATB/  NJA,NCREXATBTR(JTAB),NCREXATBS (JTAB),
     1                NCREXATBDW(JTAB)
C
C
C             NCREXATBTR      - augmented table B table reference
C             NCREXATBS       - augmented table B scale
C             NCREXATBDW      - augmented table B data width
C
C
      COMMON /CRATBC/ CREXATBEN(JTAB),CREXATBU (JTAB)
C
C             CREXATBEN      - augmented table B element name
C             CREXATBU       - augmented table B units
C
C
      COMMON /CRBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
C
C
      REAL*8 RVIND,EPS
C
      character*64 CREXNAME, CREXATBEN
      character*24 CREXATBU,CREXUNIT
C
C
C
C     ------------------------------------------------------------------
C*          1.   UPDATE WORKING TABLE.
C                ---------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      ICLASS=KDD/1000
      IYYY  =KDD-ICLASS*1000+1
      ICLASS=ICLASS+1
C
C*          1.1  ASSOCIATED FIELD ?
C                ------------------
 110  CONTINUE
C
      IF(NCREXAFDW.EQ.0) GO TO 140
C
C*          1.2  UNITS ELEMENT DESCRIPTOR ?
C                --------------------------
 120  CONTINUE
C
      I=NCREXP(ICLASS,IYYY)
      IF(I.EQ.0) THEN
         KERR=23
         WRITE(*,*) ' CREXEPWT : ', KDD
         CALL CREXERR(KERR)
         DO IQ=1,JELEM
         NCREXSTACK(IQ)=0.
         END DO
         RETURN
      END IF
C
      IF(CREXUNIT(I)(1:4).EQ.'CODE') GO TO 140
      IF(CREXUNIT(I)(1:4).EQ.'FLAG') GO TO 140
      IF(CREXUNIT(I)(1:3).EQ.'NUM' ) GO TO 140
C
C*          1.3   ADD SPECIAL ENTRY TO WORKING TABLE.
C                 -----------------------------------
 130  CONTINUE
C
      NCREXWT=NCREXWT+1
      CREXWTEN(NCREXWT)='ASSOCIATED FIELD'
      CREXWTU (NCREXWT)=' '
      NCREXWTDW(NCREXWT)=NCREXAFDW
      NCREXWTR (NCREXWT)= 999999
      NCREXWTEN(NCREXWT)= 0
      NCREXWTS (NCREXWT)= 0
      NCREXAFDW=0
C
      M=NCREXWT
C
C     ------------------------------------------------------------------
C*          1.4   SEARCH AUGMENTED TABLE *B ENTRIES .
C                 -----------------------------------
 140  CONTINUE
C
C
C
C*          1.5  GET TABLE *B ENTRY .
C                ---------------------
 150  CONTINUE
C
      I=NCREXP(ICLASS,IYYY)
      IF(I.EQ.0) THEN
         KERR=23
         WRITE(*,*)  'CREXEPWT : ',KDD
         CALL CREXERR(KERR)
         DO 2 IQ=1,JELEM
         NCREXSTACK(IQ)=0.
 2       CONTINUE
         RETURN
      END IF
C
 155  CONTINUE
C
C     -----------------------------------------------------------------
C*          1.6   MODIFY ENTRY FOR OPERATOR IN FORCE.
C                 -----------------------------------
 160  CONTINUE
C
C*                ADD ENTRY TO WORKING TABLE.
C                 ---------------------------
      NCREXWT=NCREXWT+1
      NCREXWTR (NCREXWT) = KDD
      IF(NCREXSCAM.ne.0) THEN
         NCREXWTS (NCREXWT) = NCREXSCAM
      ELSE
         NCREXWTS (NCREXWT) = NCREXBS (I)
      END IF
C
      NCREXSCAM=0
C
      IF(NCREXDWINC.ne.0) THEN
         NCREXWTDW(NCREXWT) = NCREXDWINC
      ELSE
         NCREXWTDW(NCREXWT) = NCREXBDW(I)
      END IF
C
      NCREXDWINC=0
C
      IF(NCREXWTR (NCREXWT).eq.031002) THEN
         NCREXWTDW(NCREXWT)=4
      END IF
C
C     CHECK IF DATA ARE PRESENT IN DATA SECTION.
C
      IF(NCREXWTR(NCREXWT).eq.31011.or.
     1   NCREXWTR(NCREXWT).eq.31012) then
         IF(NCREXWTR(NCREXWT-1).eq.31011.or.
     2      NCREXWTR(NCREXWT-1).eq.31012) THEN
            NCREXWTDW(NCREXWT)=0
         END IF
      END IF

 175  CONTINUE
C
C*          1.8 UPDATE M.
C               ---------
 180  CONTINUE
C
C
      CREXWTEN(NCREXWT) = CREXNAME(I)
      IF(NC07YYY.NE.0) THEN
         CREXWTU (NCREXWT) = YUNIT
         YUNIT=' '
         NC07YYY=0
      else
         CREXWTU (NCREXWT) = CREXUNIT(I)
      END IF
      NCREXWTEN(NCREXWT) = 0
      IF(CREXUNIT(I)(1:3).EQ.'CHA') NCREXWTEN(NCREXWT)=658367
C
       M = M + 1
C
C     ------------------------------------------------------------------
C
 200  CONTINUE
C
      RETURN
C
      END
