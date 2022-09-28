C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXPRCO(KBUFL,YBUFF,KJ,KDD,KSTACK,KELEM,KERR)
C
C**** *CREXPRCO*
C
C
C     PURPOSE.
C     --------
C
C          Process CREX operator.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *CREXPRCO(KBUFL,YBUFF,KJ,KDD,KSTACK,KELEM,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of CREX message (words)
C               *YBUFF*   -  array containing CREX message
C               *KJ*      -  pointer to array kstack
C               *KDD*     -  data descriptor
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C        OUTPUT:
C               *KSTACK*  - list of descriptors
C               *KERR*    - return error code
C
C     *METHOD.
C      -------
C
C          NONE.
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
C
      COMMON /CREXWORK/ NBPT,NGS,NVIND,NBPTSEC2,RVIND,EPS,NE,IEOLD,
     1                  NC07YYY
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
      COMMON /CREXBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      COMMON /CREXUT/ YUNIT,CUNITNAME,CIA5,CIA2,NCODE,NRECORDS
C
C             NCODE  - array containing unit code number
C             CUNITNAME - array containing unit name
C             CIA5 - array  containing unit CCITTIA5 name
C             CIA2 - array  containing unit CCITTIA2 name
C             NRECORDS - number of entries in the list
C
      DIMENSION NCODE(JCTEXT)
      CHARACTER*30 CUNITNAME(JCTEXT)
      CHARACTER*24 CIA5(JCTEXT)
      CHARACTER*24 CIA2(JCTEXT)
      CHARACTER*24 YUNIT
C
      CHARACTER CREXWTEN*64,CREXWTU*24
      DIMENSION KSTACK(*)
      CHARACTER*(*) YBUFF
      REAL*8 RVIND,EPS
C
C     ------------------------------------------------------------------
C
C*          1.   DETERMINE *F *X AND *Y.
C                -----------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      IF  = KDD / 100000
      IDIF= KDD - IF * 100000
      IX  = IDIF / 1000
      IY  = IDIF - IX * 1000
C
      IF( IF.NE.2 ) THEN
         KERR=21
         PRINT*,' CREXPRCO :'
         CALL CREXERR(KERR)
         GO TO 400
      END IF
C
C*          1.1   CHANGE DATA WIDTH ?
C                 -------------------
 110  CONTINUE
C
      IF(IX.EQ.1) THEN
         NCREXDWINC= IY
         GO TO 400
      END IF
C
C*          1.2   CHANGE SCALE ?
C                 --------------
 120  CONTINUE
C
      IF(IX.EQ.2) THEN
C
C*          1.2.1  UPDATE SCALE MULTIPLIER.
C                  ------------------------
C
         IF(IY.GT.900) IY=-(IY-900)
         NCREXSCAM=IY
         GO TO 400
      END IF
C
C
C*          1.5   SIGNIFY CHARACTER ?
C                 -------------------
 150  CONTINUE
C
      IF(IX.EQ.5) THEN
C
C*          1.5.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NCREXWT = NCREXWT + 1
         CREXWTEN(NCREXWT)='CHARACTERS'
         CREXWTU (NCREXWT)=' '
         NCREXWTR (NCREXWT)= kdd      
         NCREXWTDW(NCREXWT)= IY
         NCREXWTEN(NCREXWT)=658367
         NCREXWTS (NCREXWT)=0
         NCREXWTRV(NCREXWT)=0
         GO TO 400
      END IF
C
C          1.6 CHANGE UNIT
C              -----------
 160  CONTINUE
C
      IERR=0
      IF(IX.EQ.7) THEN
         CALL GET_UNITS(IY,IERR)
         IF(IERR.EQ.0) THEN
            NC07YYY=IY
         END IF
C
         GO TO 400
C
      END IF
C
C     ------------------------------------------------------------------
C
 300  CONTINUE
C
      KERR=22
      PRINT*,'CREXPRCO:'
      CALL CREXERR(KERR)
C
C     ------------------------------------------------------------------
C
 400  CONTINUE
C
      RETURN
      END
