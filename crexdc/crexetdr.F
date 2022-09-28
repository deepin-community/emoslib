C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXETDR(KJ,KJ1,KDD,KSTACK,KERR)
C
C**** *CREXETDR*
C
C
C     PURPOSE.
C     --------
C          SOLVE BUFR TABLE D REFERENCE.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *CREXETDR(KJ,KJ1,KDD,KSTACK,KERR)*
C
C        INPUT :
C                 *KDD*      - DATA DESCRIPTOR
C        OUTPUT:
C                 *KJ*       - POINTER TO KSTACK ARRAY
C                 *KJ1*      - POINTER TO LAST ELEMENT IN KSTACK
C                 *KSTACK*   - LIST OF DATA DESCRIPTORS
C                 *KERR*     - RETURN ERROR CODE
C
C
C     METHOD.
C     -------
C
C           NONE.
C
C     EXTERNALS.
C     ----------
C
C           NONE.
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
C
      COMMON/CRTAB/ NCREXBR(JTAB),NCREXBS(JTAB)    , NCREXBDW(JTAB),
     1              NCREXDR(JTAB),NCREXDST(JTAB)   ,
     2              NCREXDL(JTAB),NCREXDSQ(JTAB*20),NCREXP(64,255)

C
C             NCREXBR     - table B,  table reference              array
C             NCREXBS     - table B,  scale                        array
C             NCREXBDW    - table B,  data width                   array
C             NCREXDR    - table D,  table reference              array
C             NCREXDST    - table D,  starting pointers            array
C             NCREXDL     - table D,  lengths                      array
C             NCREXDSQ    - table D,  list of sequence descriptors array
C
       COMMON /CRTABC/ CREXNAME(JTAB),CREXUNIT(JTAB)
C
C             CREXNAME      -  table B, ELEMENT NAME           array
C             CREXUNIT      -  table B, unit                   array
C
C
C
      CHARACTER*64 CREXNAME
      CHARACTER*24 CREXUNIT
C
      DIMENSION ILIST(JELEM),KSTACK(*)
C
C     ------------------------------------------------------------------
C
C*          1.   OBTAIN LIST OF DESCRIPTORS FROM BUFR TABLE D.
C                ---------------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      DO 110 J=1,JTAB
C
      IF(KDD.EQ.NCREXDR(J)) THEN
         I=J
         GO TO 120
      END IF
C
 110  CONTINUE
C
      KERR=20
      WRITE(*,*)  'CREXETDR : ',KDD
      CALL CREXERR(KERR)
      RETURN
C
 120  CONTINUE
C
      J1=NCREXDST(I)
      J2=NCREXDL (I)
      J3=0
C
      DO 121 J=J1,J1+J2-1
C
      J3 = J3 +1
      ILIST(J3) = NCREXDSQ(J)
C
 121  CONTINUE
C
C     ------------------------------------------------------------------
C*          2.  PUSH DOWN DATA DESCRIPTION DESCRIPTORS
C               --------------------------------------
C               TO MAKE ROOM FOR LIST.
C               ----------------------
 200  CONTINUE
C
      J2M1=J2-1
C
      DO 210 J=KJ1,KJ+1,-1
C
      KSTACK(J+J2M1) = KSTACK(J)
C
 210  CONTINUE
C
C     ------------------------------------------------------------------
C*          3.  INSERT LIST IN PLACE OF SEQUENCE DESCRIPTORS.
C               ---------------------------------------------
 300  CONTINUE
C
      KJM1=KJ-1
C
      DO 310 J=1,J3
C
      KSTACK(KJM1+J)= ILIST(J)
C
 310  CONTINUE
C
C     ------------------------------------------------------------------
C*          4.  ADJUST DESCRIPTOR COUNT FOR LIST LENGTH.
C               ----------------------------------------
 400  CONTINUE
C
      KJ  = KJ  - 1
      KJ1 = KJ1 +J3 -1
C     ------------------------------------------------------------------
C*          4.1  ADJUST NUMBER OF DATA DESCRIPTORS NOT PRESENT.
C                ----------------------------------------------
 610  CONTINUE
C
      IF(N221.NE.0)  N221= KJ1  - KJ + 1
C     -----------------------------------------------------------------
 500  CONTINUE
C
      RETURN
C
      END
