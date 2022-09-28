C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXOPER(KPT,KDLEN,KDATA,KJ,KDD,KSTACK,KERR)
C
C
C**** *CREXOPER*
C
C
C     PURPOSE.
C     --------
C
C          PROCESS BUFR OPERATOR.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *CREXOPER(KPT,KDLEN,KDATA,KJ,KDD,KSTACK,KERR)*
C
C        INPUT :
C               *KPT*    - POINTER TO KDATA ARRAY
C               *KDLEN*  - DIMENSION OF KDATA ARRAY
C               *KDATA*  - ARRAY CONTAINING DATA NEEDED FOR DESCRIPTOR
C                          EXPANSION
C               *KJ*     - POINTER T ARRAY KSTACK
C               *KDD*    - DATA DESCRIPTOR
C        OUTPUT:
C               *KSTACK* - LIST OF DESCRIPTORS
C               *KERR*   - RETURN ERROR CODE
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
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
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
      CHARACTER CREXWTEN*64,CREXWTU*24
C
      COMMON /CRWTC/ CREXWTEN(JELEM),CREXWTU(JELEM)
C
C             CREXWTEN    -  working table element naame
C             CREXWTU     -  working table units
C
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
      REAL*8 RVIND
      REAL*8 EPS
C
      DIMENSION KSTACK(*)
      DIMENSION KDATA(KDLEN)
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
         WRITE(*,*)  ' CREXOPER :'
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
C*          1.4   ADD ASSOCIATED FIELD ?
C                 ----------------------
 140  CONTINUE
C
      IF(IX.EQ.4) THEN
C
C*          1.4.1   UPDATE ASSOCIATED FIELD WIDTH.
C                   ------------------------------
         NCREXAFDW= IY
         GO TO 400
      END IF
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
         CREXWTU(NCREXWT)=' '
         NCREXWTR (NCREXWT)= kdd
         NCREXWTDW(NCREXWT)= IY
         NCREXWTEN(NCREXWT)=658367
         NCREXWTS (NCREXWT)=0
         GO TO 400
      END IF
C
C
C*          1.6    CHANGE UNIT
C                 ------------
 160  CONTINUE
C
      IF(IX.EQ.7) THEN
C
         IERR=0
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
      WRITE(*,*)  'CREXOPER:'
      CALL CREXERR(KERR)
C
C     ------------------------------------------------------------------
C
 400  CONTINUE
C
      RETURN
      END
