C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE BUPRCO(KBUFL,KBUFF,KJ,KDD,KSTACK,KELEM,KERR)
C
C**** *BUPRCO*
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
C          *CALL* *BUPRCO(KBUFL,KBUFF,KJ,KDD,KSTACK,KELEM,KERR)*
C
C        INPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C               *KJ*      -  POINTER TO ARRAY KSTACK
C               *KDD*     -  DATA DESCRIPTOR
C               *KELEM*   -  DIMENSION OF CNAMES, CUNITS ARRAY
C        OUTPUT:
C               *KSTACK*  - LIST OF DESCRIPTORS
C               *KERR*    - RETURN ERROR CODE
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
C          BUUATB         - UPDATE AUGMENTED TABLE B
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
#     include "bcomwork.F"
#     include "bcomwt.F"
#     include "bcomwtc.F"
#     include "bcombef.F"
C
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
      CHARACTER CWTEN*64,CWTU*24
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSTACK(*)
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
         CALL BUERR(KERR)
         WRITE(KNTN,*) KDD,' IS TO BE OPERATOR'
         GO TO 400
      END IF
C
C*          1.1   CHANGE DATA WIDTH ?
C                 -------------------
 110  CONTINUE
C
      IF(IX.EQ.1) THEN
         IF(IY.EQ.0) THEN
            NDWINC=0
         ELSE
           NDWINC= NDWINC + (IY-128)
         END IF
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
         IF(IY.EQ.0) THEN
            NSCAM=0
         ELSE
            NSCAM=NSCAM + (IY-128)
         END IF
         GO TO 400
      END IF
C
C*          1.3  CHANGE REFERENCE VALUE ?
C                ------------------------
 130  CONTINUE
C
      IF(IX.EQ.3) THEN
C
C*          1.3.1  UPDATE AUGMENTED TABLE B.
C                  -------------------------
         CALL BUUATB(KBUFL,KBUFF,KJ,IY,KSTACK,KELEM,KERR)
         GO TO 400
      END IF
C
C*          1.4   ADD ASSOCIATED FIELD ?
C                 ----------------------
 140  CONTINUE
C
      IF(IX.EQ.4) THEN
C
C*          1.4.1   UPDATE ASSOCIATED FIELD WIDTH.
C                   ------------------------------
         IF(IY.EQ.0) THEN
            NFD=NFD-1
            IF(NFD.LT.0) THEN
               KERR=51
               CALL BUERR(KERR)
               RETURN
            ELSEIF(NFD.EQ.0) THEN
               NAFDW=0
            END IF
         ELSE
            NFD=NFD+1
            NAFDWA(NFD)=IY
         END IF
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
         NWT = NWT + 1
         CWTEN(NWT)='CHARACTERS'
         CWTU (NWT)=' '
         NWTR (NWT)= KDD      ! 0
         NWTDW(NWT)= IY * 8
         NWTEN(NWT)=658367
         NWTS (NWT)=0
         NWTRV(NWT)=0
         M=M+1
         GO TO 400
      END IF
C
C*          1.5.2 SIGNIFY DATA WIDTH FOR IMMEDISTELY
C                 FOLLOWED LOCAL DESCRIPTOR
C
 152  CONTINUE
C
      IF(IX.EQ.6) THEN
         NWT = NWT + 1
         KJ=KJ+1
         CWTEN(NWT)='UNKNOWN'
         CWTU (NWT)='UNKNOWN'
         NWTR (NWT)= KSTACK(KJ)
         NWTDW(NWT)= IY
         NWTS (NWT)= 0
         NWTRV(NWT)= 0
         NWTEN(NWT)= NVIND
         M=NWT
C
C        CHECK IF LOCAL TABLE ENTRY KNOWN
C        --------------------------------
C
C         DO 153 I=1,JTAB
C         IF(NWTR(NWT).EQ.NTABBTR(I)) THEN
C            CWTEN(NWT)=CTABBEN(I)
C            CWTU (NWT)=CTABBU (I)
C            IF(CWTU(NWT)(1:3).EQ.'CCI') NWTEN(NWT)=65367
C            NWTS (NWT)=NTABBS (I)
C            NWTRV(NWT)=NTABBRV(I)
C            NWTDW(NWT)=NTABBDW(I)
C            NWTEN(NWT)=NTABBTR(I)
C            GO TO 400
C         END IF
C  153    CONTINUE
C
         GO TO 400
      END IF
C
      IF(IX.EQ.7) THEN
C
C*          1.5.4  UPDATE SCALE MULTIPLIER.
C                  ------------------------
C
         IF(IY.EQ.0) THEN
            N07     =0
            NSCAM07 =0
            NDWINC07=0
         ELSE
            N07     =IY                               ! exponent
            NSCAM07 =IY
            NDWINC07=((10*IY)+2)/3                    ! data width

         END IF
         GO TO 400
      END IF

      IF(IX.EQ.8) THEN
C
C*          1.5.5  SET DATA WIDTH FOR CCITTIA5
C                  ---------------------------
C
         IF(IY.EQ.0) THEN
           N08=0
         ELSE
           N08=IY          ! number of chatacters
         END IF
         GO TO 400
      END IF
C
C
C*          1.6   QUALITY INFORMATION FOLLOWS.
C                 ----------------------------
 160  CONTINUE
C
      IF(IX.EQ.62) THEN
C
      IF(IY.EQ.0) GO TO 400
C
C*          1.7.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
         CWTEN(NWT)='QUALITY INFORMATION FOLLOW'
         CWTU (NWT)=' '
         NWTDW(NWT)= 0
         NWTR (NWT)= 222000
         NWTRV(NWT)= 0
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
         M=M+1
         IF(M.GT.KELEM) THEN
            KERR=25
            WRITE(KNTN,*)  'BUPRCO:'
            CALL BUERR(KERR)
            RETURN
         END IF
         GO TO 400
      END IF
C
C
C*          2.  PROCESSING NEW OPERATORS.
C               -------------------------
 200  CONTINUE
C
C
C*          2.1   DATA NOT PRESENT.
C                 -----------------
 210  CONTINUE
C
      IF(IX.EQ.21) THEN
         N221=IY
         GO TO 400
      END IF
C
C
C*          2.2   QUALITY INFORMATION FOLLOWS.
C                 ----------------------------
 220  CONTINUE
C
      IF(IX.EQ.22) THEN
C
C*          1.7.2  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
         CWTEN(NWT)='QUALITY INFORMATION FOLLOW'
         CWTU (NWT)=' '
         NWTDW(NWT)= 0
         NWTR (NWT)= 222000
         NWTRV(NWT)= 0
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
         M=M+1
         IF(M.GT.KELEM) THEN
            KERR=25
            WRITE(KNTN,*)  'BUPRCO:'
            CALL BUERR(KERR)
            RETURN
         END IF
         GO TO 400
      END IF
C
C
C*          2.3   SUBSTITUTED VALUES FOLLOWS.
C                 ---------------------------
 230  CONTINUE
C
      IF(IX.EQ.23) THEN
         IF(IY.EQ.0) THEN
            NWT=NWT+1
            CWTEN(NWT)='SUBSTITUTED VALUES FOLLOW'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=223000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.KELEM) THEN
               KERR=25
               WRITE(KNTN,*)  'BUPRCO:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
            IF(NFD.NE.0) THEN
               DO I=1,NFD
               NWT=NWT+1
               CWTEN(NWT)='ASSOCIATED FIELD'
               CWTU (NWT)=' '
               NWTDW(NWT)= 0
               NWTR (NWT)= 0
               NWTRV(NWT)= 0
               NWTEN(NWT)= 0
               NWTS (NWT)= 0
               M=M+1
               END DO
               IF(M.GT.KELEM) THEN
                  KERR=25
                  WRITE(KNTN,*)  'BUPRCO:'
                  CALL BUERR(KERR)
                  RETURN
               END IF
            END IF
            OMARKER=.TRUE.
            NWT=NWT+1
             CWTEN(NWT)='SUBSTITUTED VALUE MARKER'
             CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=KDD
             NWTRV(NWT)= 0
             NWTEN(NWT)= 0
             NWTS (NWT)= 0
            M=M+1
            IF(M.GT.KELEM) THEN
               KERR=25
               WRITE(KNTN,*)  'BUPRCO:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C
C*          2.4   FIRST ORDER STATISTICS FOLLOWS.
C                 -------------------------------
 240  CONTINUE
C
      IF(IX.EQ.24) THEN
         IF(IY.EQ.0) THEN
            NWT=NWT+1
            CWTEN(NWT)='FIRST ORDER STATISTICS FOLLOW'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=224000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.KELEM) THEN
               KERR=25
               WRITE(KNTN,*)  'BUPRCO:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
            IF(NFD.NE.0) THEN
               DO I=1,NFD
               NWT=NWT+1
               CWTEN(NWT)='ASSOCIATED FIELD'
               CWTU (NWT)=' '
               NWTDW(NWT)= 0
               NWTR (NWT)= 0
               NWTRV(NWT)= 0
               NWTEN(NWT)= 0
               NWTS (NWT)= 0
               M=M+1
               END DO
               IF(M.GT.KELEM) THEN
                  KERR=25
                  WRITE(KNTN,*)  'BUPRCO:'
                  CALL BUERR(KERR)
                  RETURN
               END IF
            END IF
            OMARKER=.TRUE.
            NWT=NWT+1
             CWTEN(NWT)='FIRST ORDER STATISTICS VALUE MARKER'
             CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=KDD
             NWTRV(NWT)= 0
             NWTEN(NWT)= 0
             NWTS (NWT)= 0
            M=M+1
            IF(M.GT.KELEM) THEN
               KERR=25
               WRITE(KNTN,*)  'BUPRCO:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C*          2.5   DIFFERENCE STATISTICAL VALUES FOLLOW.
C                 -------------------------------------
 250  CONTINUE
C
      IF(IX.EQ.25) THEN
         IF(IY.EQ.0) THEN
            NWT=NWT+1
            CWTEN(NWT)='DIFFERENCE STATISTICAL VALUES FOLLOW'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=225000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.KELEM) THEN
               KERR=25
               WRITE(KNTN,*)  'BUPRCO:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
            IF(NFD.NE.0) THEN
               DO I=1,NFD
               NWT=NWT+1
               CWTEN(NWT)='ASSOCIATED FIELD'
               CWTU (NWT)=' '
               NWTDW(NWT)= 0
               NWTR (NWT)= 0
               NWTRV(NWT)= 0
               NWTEN(NWT)= 0
               NWTS (NWT)= 0
               M=M+1
               END DO
               IF(M.GT.KELEM) THEN
                  KERR=25
                  WRITE(KNTN,*)  'BUPRCO:'
                  CALL BUERR(KERR)
                  RETURN
               END IF
            END IF
            OMARKER=.TRUE.
            NWT=NWT+1
             CWTEN(NWT)='DIFFERENCE STATISTICS VALUE MARKER'
             CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=KDD
             NWTRV(NWT)= 0
             NWTEN(NWT)= 0
             NWTS (NWT)= 0
            M=M+1
            IF(M.GT.KELEM) THEN
               KERR=25
               WRITE(KNTN,*)  'BUPRCO:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C
C*          2.6   REPLACED/RETAINED VALUES FOLLOWS.
C                 ---------------------------------
 260  CONTINUE
C
      IF(IX.EQ.32) THEN
         IF(IY.EQ.0) THEN
            NWT=NWT+1
            CWTEN(NWT)='REPLACE/RETAINED VALUES FOLLOW'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=232000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.KELEM) THEN
               KERR=25
               WRITE(KNTN,*)  'BUPRCO:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
            IF(NFD.NE.0) THEN
               DO I=1,NFD
               NWT=NWT+1
               CWTEN(NWT)='ASSOCIATED FIELD'
               CWTU (NWT)=' '
               NWTDW(NWT)= 0
               NWTR (NWT)= 0
               NWTRV(NWT)= 0
               NWTEN(NWT)= 0
               NWTS (NWT)= 0
               M=M+1
               END DO
               IF(M.GT.KELEM) THEN
                  KERR=25
                  WRITE(KNTN,*)  'BUPRCO:'
                  CALL BUERR(KERR)
                  RETURN
               END IF
            END IF
            OMARKER=.TRUE.
            NWT=NWT+1
             CWTEN(NWT)='REPLACE/RETAINED VALUE MARKER'
             CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=KDD
             NWTRV(NWT)= 0
             NWTEN(NWT)= 0
             NWTS (NWT)= 0
            M=M+1
            IF(M.GT.KELEM) THEN
               KERR=25
               WRITE(KNTN,*)  'BUPRCO:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C*          2.7   CANCEL BACKWARD REFERENCE.
C                 --------------------------
 270  CONTINUE
C
      IF(IX.EQ.35) THEN
C
C*          1.7.2  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
         CWTEN(NWT)='CANCEL BACKWARD DATA REFERENCE'
         CWTU (NWT)=' '
         NWTDW(NWT)= 0
         NWTR (NWT)= 235000
         NWTRV(NWT)= 0
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
C
C         MREL=0
         M=M+1
         IF(M.GT.KELEM) THEN
            KERR=25
            WRITE(KNTN,*)  'BUPRCO:'
            CALL BUERR(KERR)
            RETURN
         END IF
         GO TO 400
      END IF
C
C*          2.8   DEFINE BACKWARD REFERENCE BIT MAP.
C                 ----------------------------------
 280  CONTINUE
C
      IF(IX.EQ.36) THEN
C
C*          2.8.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
         CWTEN(NWT)='BACKWARD REFERENCE BIT MAP'
         CWTU (NWT)=' '
         NWTDW(NWT)= 0
         NWTR (NWT)= 236000
         NWTRV(NWT)= 0
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
C
         M=M+1
         IF(M.GT.JELEM) THEN
            KERR=30
            WRITE(KNTN,*)  'BUOPER:'
            CALL BUERR(KERR)
            RETURN
         END IF
         GO TO 400
      END IF
C
C*          2.9   DEFINE BACKWARD REFERENCE BIT MAP.
C                 ----------------------------------
 290  CONTINUE
C
      IF(IX.EQ.37) THEN
         IF(IY.EQ.0) THEN
C
C*          2.9.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
            NWT = NWT + 1
            CWTEN(NWT)='USE PREVIOUSLY DEFINED BIT MAP'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)= 237000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
C
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               WRITE(KNTN,*)  'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
C
C*          2.9.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
            NWT = NWT + 1
            CWTEN(NWT)='CANCEL REFERENCE TO PREDEFINED BIT MAP'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)= KDD
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
C
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               WRITE(KNTN,*)  'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
      IF(IX.EQ.9) THEN
         IF(IY.EQ.0) THEN
            N40=0
         ELSE
            N40=IY
         END IF
C
         GO TO 400
      END IF
C

C
C*    5. DEFINE EVENT
C        ------------
 500  CONTINUE
C
      IF(IX.EQ.41) THEN
         IF(IY.EQ.0) THEN
C
C         5.1.1 ADD SPECIAL ENTRY TO WORKING TABLE.
C               -----------------------------------
            NWT = NWT + 1
            CWTEN(NWT)='DEFINE EVENT'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)= 241000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
C
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               WRITE(KNTN,*)  'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
C
C*          5.1.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
            NWT = NWT + 1
            CWTEN(NWT)='CANCEL DEFINE EVENT'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)= KDD
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
C
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               WRITE(KNTN,*)  'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C*        5.1  DEFINE CONDITIONING EVENT
C              -------------------------
 510  CONTINUE
C
      IF(IX.EQ.42) THEN
         IF(IY.EQ.0) THEN
C
C         5.1.1 ADD SPECIAL ENTRY TO WORKING TABLE.
C               -----------------------------------
            NWT = NWT + 1
            CWTEN(NWT)='DEFINE CONDITIONING EVENT'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)= 242000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
C
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               WRITE(KNTN,*)  'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
C
C*          5.1.2  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
            NWT = NWT + 1
            CWTEN(NWT)='CANCEL DEFINE CONDITIONING EVENT'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)= KDD
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
C
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               WRITE(KNTN,*)  'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C*        5.2  DEFINE CONDITIONING EVENT
C              -------------------------
 520  CONTINUE
C
      IF(IX.EQ.43) THEN
         IF(IY.EQ.0) THEN
C
C         5.2.1 ADD SPECIAL ENTRY TO WORKING TABLE.
C               -----------------------------------
            NWT = NWT + 1
            CWTEN(NWT)='CATEGORICAL FORECAST VALUES FOLLOW'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)= 243000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
C
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               WRITE(KNTN,*)  'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
C
C*          5.2.2  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
            NWT = NWT + 1
            CWTEN(NWT)='CANCEL CATEGORICAL FORECAST VALUES FOLLOW'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)= KDD
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
C
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               WRITE(KNTN,*)  'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C     ------------------------------------------------------------------
C
 300  CONTINUE
C
      KERR=22
      WRITE(KNTN,*)  'BUPRCO:'
      CALL BUERR(KERR)
C
C     ------------------------------------------------------------------
C
 400  CONTINUE
C
      RETURN
      END
