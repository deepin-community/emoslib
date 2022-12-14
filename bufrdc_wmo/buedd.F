C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE BUEDD(KPT,KTDLEN,KTDLST,KDLEN,KDATA,KSEC3,
     1                 KVALS,VALUES,KELEM,CNAMES,CUNITS,KERR)
C
C**** *BUEDD*
C
C
C     PURPOSE.
C     --------
C
C          EXPAND SECTION 3 OF BUFR MESSAGE.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEDD(KPT,KTDLEN,KTDLST,KDLEN,KDATA,KSEC3,
C                        KVALS,VALUES,KELEM,CNAMES,CUNITS,KERR)
C
C        INPUT :
C               *KPT*     -  POINTER TO KDATA ARRAY
C               *KTDLEN*  -  NUMBER OF DATA DESCRIPTORS IN SECTION 3
C               *KTDLST*  -  ARRAY CONTAINING DATA DESCRIPTORS IN SECTION 3
C               *KDLEN*   -  DIMENSION OF KDATA ARRAY
C               *KDATA*   -  ARRAY CONTAINING DATA NEEDED FOR DATA DESCRIPTOR
C                            EXPANSION
C               *KSEC3*   -  ARRAY CONTAINING SECTION 3 INFORMATION
C                            KSEC3( 1)-- LENGTH OF SECTION 3 (BYTES)
C                            KSEC3( 2)-- RESERVED
C                            KSEC3( 3)-- NUMBER OF SUBSETS
C                            KSEC3( 4)-- FLAG (DATA TYPE,DATA COMPRESSION)
C        OUTPUT :
C               *KERR*    -  RETURNED ERROR CODE
C
C     METHOD.
C     -------
C           DATA DESCRIPTOR TAKEN FROM KTDLST ARRAY ARE FULLY
C        EXPANDED USING DATA FROM KDATA ARRAY IF NEEDED.
C        ( DELAYED REPLICATION FACTORS ETC.)
C
C
C     EXTERNALS.
C     ----------
C
C          BUNEXS        - SET WORD AND BIT POINTERS AT THE BEGINING OF
C                          NEXT SECTION
C          BUNPCK        - UNPACKS BIT PATHERN
C          BUREP         - SOLVES REPLICATION PROBLEM
C          BUETDR        - SOLVES TABLE D REFERENCE
C          BUOPER        - PROCESS OPERATOR
C          BUEPWT        - UPDATES WORKING TABLE
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
#     include "parameter.F"
#     include "bcomunit.F"
#     include "bcmwork.F"
#     include "bcmbef.F"
#     include "bcmwt.F"
#     include "bcmwtc.F"
#     include "bcmel.F"
C
      CHARACTER CWTEN*64,CWTU*24
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
C
#ifndef R_4
      REAL*8 VALUES(KVALS)
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   VALUES(KVALS)
      REAL   RVIND
      REAL   EPS
#endif
C
      DIMENSION ISTACK(JELEM),IISTACK(JELEM)
      DIMENSION IMASK(8)
C
      DIMENSION KSEC3(JSEC3)
      DIMENSION KDATA(KDLEN),KTDLST(KTDLEN)
C
      DATA IMASK/1,2,4,8,16,32,64,128/
C     ------------------------------------------------------------------
C
C*          1.
C                --------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      N = KSEC3(3)
      NOLD = 0
C
C
C*          2.   EXPAND DATA DESCRIPTORS.
C                ------------------------
 200  CONTINUE
C
C
C*          2.1  SET EXPECTED NUMBER OF DATA DESCRIPTORS.
C                ----------------------------------------
C                AND INITIALIZE NUMBER OF DATA VALUES PER SUB-SET.
C                -------------------------------------------------
 210  CONTINUE
C
      J      = 0
      KPT    = 0
      NWT    = 0
      JMAX   = KTDLEN
      JMAXNEW=JMAX
C
      IF(KTDLEN.GT.JELEM) THEN
         WRITE(KNTN,*) ' BUETD :'
         KERR=29
         CALL BUERR(KERR)
         RETURN
      END IF
C
C*          2.2  PUT DATA DESCRIPTORS IN STACK.
C                ------------------------------
 220  CONTINUE
C
      DO 221 JJ=1,JMAX
C
      ISTACK(JJ)=KTDLST(JJ)
      IISTACK(JJ)=ISTACK(JJ)
C
 221  CONTINUE
C
C*          2.2.1 CHECK IF IT IS SAME DATA DESCRIPTOR DESCRIOPTION.
C                 -------------------------------------------------
C                 TO MAKE MORE EFFICIENT DATA DESCRIPTOR DESCRIPTION
C                 EXPANSION, IN CASE THAT DELAYED REPLICATION FACTOR
C                 IS NOT PRESENT AND DATA DESCRIPTORS ARE THE SAME,
C                 PREVIOUS WORKING TABLE SHOULD BE USED. IT IS POSIBLE
C                 AT THIS PLACE IN THE FUTURE TO MAKE MORE SOPHISTICATED
C                 CONTROL.
C
C
      DO 222 JC=1,JMAX
C
      IF(ISTACK(JC).NE.NSTACK(JC)) THEN
C
C
         ODREPF=.FALSE.
C
C        SWAP CONTENT OF THE STACKS.
C
         DO 223 JJC=1,JMAX
         NSTACK(JJC)=ISTACK(JJC)
 223     CONTINUE
C
         NTDLEN = JMAX
         M=0
         NOLD=N
         NFCM=0
         MREL=0
         OMARKER=.FALSE.
         MBMP=0
         MBMPL=0
C
         GO TO 230
C
      END IF
C
 222  CONTINUE
C
C
C*    IF MARKER OPERATOR PRESENT EXPAND DESCRIPTORS AGAIN
C
      IF(OMARKER) THEN
         M=0
         NOLD=N
         NFCM=0
         MREL=0
         OMARKER=.FALSE.
         NTDLEN=JMAX
         MBMP=0
         MBMPL=0
         GO TO 230
      END IF
C
C*    RETURN IF DELAYED REPLICATION FACTOR IS NOT PRESENT.
C
      IF(JMAX.NE.NTDLEN) THEN
         M=0
         NOLD=N
         NFCM=0
         MREL=0
         OMARKER=.FALSE.
         NTDLEN=JMAX
         MBMP=0
         MBMPL=0
         GO TO 230
      END IF
C
      OB=.FALSE.
      IF(IAND(KSEC3(4),IMASK(7)).NE.0)  OB=.TRUE.
C
      IF(ODREPF) GO TO 229
C
      IF(.NOT.ODREPF) THEN
         IF(N.GT.NOLD) NOLD=N
         IF(OB.AND.NFCM.EQ.1)      GO TO 300
         IF(.NOT.OB.AND.NFCM.EQ.0) GO TO 300
      END IF
C
 229  CONTINUE
C
      M=0
      NOLD=N
      NFCM=0
      MREL=0
      OMARKER=.FALSE.
      NTDLEN=JMAX
      MBMP=0
      MBMPL=0
C
C     ------------------------------------------------------------------
C*          2.3  GET NEXT DESCRIPTOR FROM THE STACK.
C                -----------------------------------
 230  CONTINUE
C
      J   = J + 1
      IF(J.GT.JMAX) GO TO 270
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
      IF(IF.EQ.0) THEN
C
C*          2.6  ELEMENT DESCRIPTOR, SO UPDATE WORKING TABLE.
C                --------------------------------------------
 260     CONTINUE
C
c        IF(IDD.EQ.31031.OR.IDD.EQ.31192) THEN
c           NWT=NWT+1
c           NWTR(NWT)=IDD
c           NWTS(NWT)=0
c           NWTRV(NWT)=0
c           NWTDW(NWT)=1
c           NWTEN(NWT)=0
c           CWTEN(NWT)='DATA PRESENT INDICATOR'
c           CWTU (NWT)='FLAG TABLE 031031'
c           M=M+1
c        ELSEIF(IDD.EQ.33007.OR.IDD.EQ.63192) THEN
c           NWT=NWT+1
c           NWTR(NWT)=IDD
c           NWTS(NWT)=0
c           NWTRV(NWT)=0
c           NWTDW(NWT)=7
c           NWTEN(NWT)=0
c           CWTEN(NWT)='% CONFIDENCE'
c           CWTU (NWT)='%'
c           M=M+1
c        ELSE
            CALL BUEPWTC(IDD,KERR)
            IF(KERR.GT.0) RETURN
c        END IF
      ELSEIF(IF.EQ.1) THEN
C
C*          2.4.1     SOLVE REPLICATION PROBLEM.
C                     --------------------------
C
         CALL BUREPC(KPT,KDLEN,KDATA,J,JMAX,IDD,ISTACK,KERR)
         IF(KERR.GT.0) RETURN
C
      ELSEIF(IF.EQ.2) THEN
C
C
C*          2.5.3 PROCESS OPERATOR.
C                 -----------------
         CALL BUOPERC(KPT,KDLEN,KDATA,J,IDD,ISTACK,KERR)
         IF(KERR.GT.0) RETURN
C
      ELSEIF(IF.EQ.3) THEN
C
C
C*          2.5.2 REPLACE BY LIST OF DESCRIPTORS FROM TABLE *D.
C                 ---------------------------------------------
         CALL BUETDR(J,JMAX,IDD,ISTACK,KERR)
         IF(KERR.GT.0) THEN
            DO 252 IQ=1,JELEM
            NSTACK(IQ)=0.
 252        CONTINUE
            RETURN
         END IF
      ELSE
         KERR=37
         CALL BUERR(KERR)
         RETURN
      END IF
C
      GO TO 230
C
C     ------------------------------------------------------------------
C*          2.7 RESOLVE MARKER OPERATOR.
C               ------------------------
 270  CONTINUE
C
C      IF(OMARKER) THEN
C         CALL BUEPMRKC(KSEC3,KVALS,VALUES,KELEM,CNAMES,CUNITS,KERR)
C         IF(KERR.GT.0) RETURN
C      END IF
C
C     ------------------------------------------------------------------
C
C*          3. COLLECT  SUPPLEMENTARY ITEMS.
C              -----------------------------
 300  CONTINUE
C
      NTDEXL =M
      DO 301 I=1,NTDEXL
      NTDEXP(I)=NWTR(I)
 301  CONTINUE
C
      NTDLEN=JMAXNEW
      DO 302 I=1,NTDLEN
      NTDLST(I)=IISTACK(I)
 302  CONTINUE
C
      DO 303 I=1,M
      CNAMES(I)=CWTEN(I)
      CUNITS(I)=CWTU (I)
 303  CONTINUE
C
      RETURN
      END
