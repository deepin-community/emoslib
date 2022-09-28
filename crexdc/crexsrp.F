C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXSRP(KBUFL,YBUFF,KSEC3,KJ,KJ1,KDD,KSTACK,KELEM,KERR)
C
C**** *CREXSRP*
C
C
C     PURPOSE.
C     --------
C          Resolve data descriptor replication problem.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *CREXSRP(KBUFL,YBUFF,KSEC3,KJ,KJ1,KDD,KSTACK,KELEM,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of CREX message (words)
C               *YBUFF*   -  array containing CREX message
C               *KSEC3*   -  array containig section 3 information
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C               *KDD*      - data descriptor
C        OUTPUT:
C               *KJ*       - pointer to kstack array
C               *KJ1*      - pointer to last element in kstack
C               *KSTACK*   - list of data descriptors
C               *KERR*     - return code
C
C     METHOD.
C     -------
C          NONE.
C
C     EXTERNALS.
C     ----------
C
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
C          MILAN DRAGOSAVAC    *ECMWF*       01/02/2003.
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
      CHARACTER CREXWTEN*64,CREXWTU*24
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL=40000,
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
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
      COMMON /CREXOFF/ NSIZEG(JELEM),NCREXWTRG(JWORK),
     1                 NBP,NSUBSET,OMULTI

C
      DIMENSION ILIST(JELEM)
      DIMENSION KSTACK(*),KSEC3(JSEC3)
C
      CHARACTER*(*) YBUFF
      REAL*8 RVIND,EPS
C
C
C     ------------------------------------------------------------------
C
C*          1.   STORE K, NUMBER OF DESCRIPTORS TO BE REPLICATED.
C                ------------------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      IF  = KDD / 100000
      IDIF= KDD - IF * 100000
      IX  = IDIF / 1000
      IY  = IDIF - IX * 1000
      K   = IX
C
C
C*          1.1   DELAYED REPLICATION ?
C                 ---------------------
 110  CONTINUE
C
      IF( IY .NE. 0 ) THEN
C
C*          1.2   STORE NUMBER OF DESCRIPTORS, K, AND REPLICATION
C                 -----------------------------------------------
C                 FACTOR JR.
C                 ----------
 120     CONTINUE
C
         K  = IX
         JR = IY
         GO TO 500
      ELSE
C
C           1.3   INSERT DELAYED REPLICATION 
C
         ODREPF=.TRUE.
         KDD=31002
         GO TO 300
      END IF
C
C     ------------------------------------------------------------------
C
C*          2.   GET NEXT DESCRIPTOR.
C                --------------------
 200  CONTINUE
C
      KJ =KJ + 1
      KDD= KSTACK(KJ)
C
C     ------------------------------------------------------------------
C
C*          2.1  REPLICATION FACTOR ?
C                --------------------
 210  CONTINUE
C
      IF(KDD.NE.31001.AND.KDD.NE.31002.AND.
     1   KDD.NE.31000.AND.
     1   KDD.NE.31011.AND.KDD.NE.31012 )THEN
C
C*          2.1.1  SEQUENCE DESCRIPTOR ?
C                  ---------------------
C
         IF=KDD/100000
C
         IF( IF.EQ.3) THEN
C
C*          2.1.1.1  SOLVE TABLE D REFERENCE.
C                    ------------------------
            CALL CREXSTDR(KJ,KJ1,KDD,KSTACK,KERR)
            IF(KERR.GT.0) THEN
               DO 252 IQ=1,JELEM
               NCREXSTACK(IQ)=0.
 252           CONTINUE
               RETURN
            END IF
            GO TO 200
         END IF
C
         IF( IF.EQ.2) THEN
            CALL CREXPRCO(KBUFL,YBUFF,KJ,KDD,KSTACK,KELEM,KERR)
            IF(KERR.GT.0) THEN
               CALL CREXERR(KERR)
               RETURN
            END IF
            GO TO 200
         END IF
C
         KERR=36
         CALL CREXERR(KERR)
         RETURN
      END IF
C
C     ------------------------------------------------------------------
C
C*          3.  UPDATE WORKING TABLE.
C               ---------------------
 300  CONTINUE
C
      CALL CREXUPWT(KDD,KELEM,KERR)
      IF(KERR.GT.0) RETURN
C
C     CHECK IF NEXT DESCRIPTOR CANCEL OPERATOR FOR DELAYED
C     REPLICATION
C
      IIIF=KSTACK(KJ+1)/100000
      IIII=KSTACK(KJ+1)-IIIF*100000
      IIIX=IIII/1000
      IIIY=IIII-IIIX*1000
C
      IF(IIIF.EQ.2.AND.IIIY.EQ.0) THEN
         KJ=KJ+1
         KDD=KSTACK(KJ)
         CALL CREXPRCO(KBUFL,YBUFF,KJ,KDD,KSTACK,KELEM,KERR)
         IF(KERR.GT.0) THEN
            CALL CREXERR(KERR)
            RETURN
         END IF
      END IF
C
C     ------------------------------------------------------------------
C
C*          4.  LOCATE AND STORE JR, THE REPLICATION FACTOR FROM DATA.
C               ------------------------------------------------------
 400  CONTINUE
C
C
C     UNCOMPRESSED DATA
C
      IF(NSUBSET.EQ.1) THEN
         IBPT=NBPTSEC2
      ELSE
         IBPT=NBP       
      END IF
      CALL NEXTPRT(KBUFL,YBUFF,IBPT,NGS,KERR )
      IF(KERR.NE.0) THEN
         print*,'nextprt: error ',ierr
         RETURN
      END IF
C
c     Now IBPT points to the first group
c
      ITEMP=IBPT
      CALL NEXTSEC(KBUFL,YBUFF, ITEMP,NGS,KERR )
      IF(KERR.NE.0) THEN
         print*,'nextsec: error ',ierr
         RETURN
      END IF
C
c     Now itemp points to the end of previous section
c
      CALL NEXTPRT(KBUFL,YBUFF,IBPT,ITEMP,KERR )
      IF(KERR.NE.0) THEN
         print*,'nextprt: error ',ierr
         RETURN
      END IF
c
      DO 401 J=1,NCREXWT-1
C
      IBPT=IBPT+NCREXWTDW(j)+NE
c     CALL NEXTPRT(KBUFL,YBUFF,IBPT,ITEMP,KERR )
c     IF(KERR.NE.0) THEN
c        print*,'nextprt: error ',ierr
c        RETURN
c     END IF
c
      CALL NEXTSEP(KBUFL,YBUFF,IBPT,ITEMP,KERR )
      CALL NEXTGRP(KBUFL,YBUFF,IBPT,ITEMP,KERR)
      IF(KERR.NE.0) THEN
         print*,'nextsec: error ',ierr
         RETURN
      END IF
C
 401  CONTINUE
C
      IF(NE.EQ.1) IBPT=IBPT+1
      CALL EXTGRP(KBUFL,YBUFF, IBPT,NCREXWTDW(NCREXWT),JR,KERR)
      IF(KERR.NE.0) THEN
         print*,'extgrp : error ',kerr
         RETURN
      end if
C
      IF(JR.EQ.0) THEN
         KJ=KJ+K
         GO TO 640
      END IF
C
      JRTK=JR*K+KJ1-K
      IF(JRTK.GT.JELEM) THEN
         KERR=19
         PRINT*,'BUSRP :'
         CALL CREXERR(KERR)
         PRINT*,'Replication factor =',JR
         RETURN
      END IF
C
C     ------------------------------------------------------------------
C*          5.  GET NEXT K DESCRIPTORS.
C               -----------------------
 500  CONTINUE
C
      DO 501 J=1,K
C
      ILIST(J)=KSTACK(KJ+J)
C
 501  CONTINUE
C
C     ------------------------------------------------------------------
C*          6.  ADD JR TIMES K DESCRIPTORS IN PLACE OF K
C               ----------------------------------------
C               DESCRIPTORS OBTAINED.
C               ---------------------
 600  CONTINUE
C
C     ------------------------------------------------------------------
C*          6.1  PUSH DOWN DESCRIPTORS IN KSTACK FOR (JR-1)*K PLACES
C                ---------------------------------------------------
C                STARTING AT KJ1 AND ENDING AT KJ+K.
C                -----------------------------------
 610  CONTINUE
C
      JRKM1=(JR-1)*K
C
      DO 611 J=KJ1,KJ+K,-1
C
      KSTACK(J+JRKM1)=KSTACK(J)
C
 611  CONTINUE
C
C*          6.2  INSERT LIST IN THE STACK.
C                -------------------------
 620  CONTINUE
C
      DO 622 J=1,JR
C
      KJJM1K=KJ+(J-1)*K
C
      DO 623 J1=1,K
C
      KSTACK(KJJM1K+J1)=ILIST(J1)
C
 623  CONTINUE
 622  CONTINUE
C
C     ------------------------------------------------------------------
C*          6.3  ADJUST DESCRIPTOR COUNT FOR LIST LENGTH.
C                ----------------------------------------
 630  CONTINUE
C
      KJ1 = KJ1  + (JR-1)*K
C
C     ------------------------------------------------------------------
C*          6.4  ADJUST NUMBER OF DATA DESCRIPTORS NOT PRESENT.
C                ----------------------------------------------
 640  CONTINUE
C
C
C     ------------------------------------------------------------------
 700  CONTINUE
C
      RETURN
      END
