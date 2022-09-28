C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXUPWT(KDD,KELEM,KERR)
C
C**** *CREXUPWT*
C
C
C     PURPOSE.
C     --------
C          Updates working tables setting element name,unit,scale,
C     reference value and data width.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *CREXUPWT(KDD,KELEM,KERR)*
C
C        INPUT :
C               *KDD*     -  data descriptor
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C        OUTPUT:
C               *KERR*    -  return error code
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
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL=40000,
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
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
C
      COMMON /CREXWTC/ CREXWTEN(JELEM),CREXWTU (JELEM)
C
C             CREXWTEN    -  working table element naame
C             CREXWTU     -  working table units
C
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
C
C

       COMMON/CREXTABC/ CREXNAME(JTAB),CREXUNIT(JTAB),CREXLST(JELEM)
C
C             CREXNAME      -  table B, ELEMENT NAME           array
C             CREXUNIT      -  table B, unit                   array
C
C
      COMMON /CREXATB/  NJA,NCREXATBTR(JTAB),NCREXATBS (JTAB),
     1                  NCREXATBRV(JTAB),NCREXATBDW(JTAB)
C
C
C             NCREXATBTR      - augmented table B table reference
C             NCREXATBS       - augmented table B scale
C             NCREXATBRV      - augmented table B reference value
C             NCREXATBDW      - augmented table B data width
C
C
      COMMON /CREXATBC/ CREXATBEN(JTAB),CREXATBU (JTAB)
C
C             CREXATBEN      - augmented table B element name
C             CREXATBU       - augmented table B units
C
C
      COMMON /CREXBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
      COMMON /CREXUT/ YUNIT,CUNITNAME,CIA5,CIA2,NCODE,NRECORDS
c
c             NCODE  - array containing unit code number
c             CUNITNAME - array containing unit name
c             CIA5 - array  containing unit CCITTIA5 name
c             CIA2 - array  containing unit CCITTIA2 name
c             NRECORDS - number of entries in the list
c
C
      COMMON /CREXWORK/ NBPT,NGS,NVIND,NBPTSEC2,RVIND,EPS,NE,IEOLD,
     1                  NC07YYY
C

      DIMENSION NCODE(JCTEXT)
      CHARACTER*30 CUNITNAME(JCTEXT)
      CHARACTER*24 CIA5(JCTEXT)
      CHARACTER*24 CIA2(JCTEXT)
      CHARACTER*24 YUNIT

C
      CHARACTER*64 CREXNAME, CREXATBEN
      CHARACTER*24 CREXATBU,CREXUNIT
      CHARACTER*6  CREXLST
C
      CHARACTER*64 CREXWTEN
      CHARACTER*24 CREXWTU
      CHARACTER YCH6*6
      REAL*8 RVIND
      REAL*8 EPS
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
         PRINT*,'CREXUPWT : ',KDD
         CALL CREXERR(KERR)
         DO 1 IQ=1,JELEM
         NCREXSTACK(IQ)=0.
 1       CONTINUE
         RETURN
      end if
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
      NCREXWTRV(NCREXWT)= 0
      NCREXAFDW=0
C
C*                UPDATE CNAMES AND CUNITS
C
      M=M+1
      IF(M.GT.KELEM) THEN
         KERR=25
         PRINT*,'CREXUPWT:'
         CALL CREXERR(KERR)
         RETURN
      END IF
C
C     ------------------------------------------------------------------
C*          1.4   SEARCH AUGMENTED TABLE *B ENTRIES .
C                 -----------------------------------
 140  CONTINUE
C
C*          1.5  GET TABLE *B ENTRY .
C                ---------------------
 150  CONTINUE
C
      I=NCREXP(ICLASS,IYYY)
      IF(I.EQ.0) THEN
         KERR=23
         PRINT*,'CREXUPWT : ',KDD
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
      if(NCREXSCAM.ne.0) then
         NCREXWTS (NCREXWT) = NCREXSCAM
      else
         NCREXWTS (NCREXWT) = NCREXBS (I)
      end if
      NCREXSCAM=0
c
      NCREXWTRV(NCREXWT) = NCREXBR(I) 
      if(NCREXDWINC.ne.0) then
         NCREXWTDW(NCREXWT) = NCREXDWINC
      else
         NCREXWTDW(NCREXWT) = NCREXBDW(I)
      end if
      NCREXDWINC=0
      if(NCREXWTR (NCREXWT).eq.031002) then
         NCREXWTDW(NCREXWT)=4
         NCREXWTRV(NCREXWT) = 0
      end if
C
C     CHECK IF DATA ARE PRESENT IN DATA SECTION.
C
      if(NCREXWTR(NCREXWT).eq.31011.or.
     1   NCREXWTR(NCREXWT).eq.31012) then
         if(NCREXWTR(NCREXWT-1).eq.31011.or.
     2      NCREXWTR(NCREXWT-1).eq.31012) then
            NCREXWTDW(NCREXWT)=0
         end if
      end if
C
 175  CONTINUE
C
C     ------------------------------------------------------------------
C*          1.8  UPDATE M, CNAMES, CUNITS.
C                -------------------------
 180  CONTINUE
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
       IF(M.GT.KELEM) THEN
          KERR=25
          PRINT*,'CREXUPWT:'
          CALL CREXERR(KERR)
          RETURN
       END IF
C
C     -----------------------------------------------------------------
C
 200  CONTINUE
C
      RETURN
      END
