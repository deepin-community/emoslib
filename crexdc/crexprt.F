C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXPRT(K,KSUB1,KSUB2,KELEM,CNAMES,CUNITS,
     1                 CVALS,KVALS,VALUES,KSUP,KSEC1,KERR)
C
C**** *CREXPRT*
C
C
C     PURPOSE.
C     --------
C           PRINT EXPANDED CREX MESSAG.
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *CREXPRT(K,KSUB1,KSUB2,KELEM,CNAMES,CUNITS,
C                         CVALS,KVALS,VALUES,KSUP,KSEC1,KERR)*
C
C        INPUT :
C               *K*       -  SWITCH TO PRINT WITH/WITOUT CONTENT OF CODE TABLES
C                            0  - NO  CODE TABLE CONTENT
C                            1  - YES CODE TABLE CONTENT
C               *KSUB1*   -  STARTING SUBSET
C               *KSUB2*   -  ENDING SUBSET
C               *KELEM*   -  DIMENSION OF CNAMES, CUNITS ARRAY
C               *CNAMES*  -  CHARACTER ARRAY CONTAINING ELEMENT NAMES
C               *CUNITS*  -  CHARACTER ARRAY CONTAINIG UNITS
C               *CVALS*   -  CHARACTER ARRAY CONTAINING CREX CODE TABLE
C                            ENTRIES
C               *KVALS*   -  DIMENSION OF VALUES ARRAY
C               *VALUES*  -  REAL*8 ARRAY (EXPANDED DATA VALUES)
C
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
C               *KSEC1*   -  Integer array of at least 40 words
C                            containing CREX section 1 information
C                            KSEC1( 1)-- Reserved
C                            KSEC1( 2)-- CREX Edition number
C                            KSEC1( 3)-- Reserved
C                            KSEC1( 4)-- Reserved
C                            KSEC1( 5)-- Reserved
C                            KSEC1( 6)-- CREX message type
C                            KSEC1( 7)-- Reserved
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- Reserved
C                            KSEC1(10)-- Reserved
C                            KSEC1(11)-- Reserved
C                            KSEC1(12)-- Reserved
C                            KSEC1(13)-- Reserved
C                            KSEC1(14)-- CREX Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) - KSEC1(40) -- Reserved
C
C
C                            Crex Edition 2
C
C               *KSEC1*   -  Integer array of at least 40 words
C                            containing CREX section 1 information
C                            KSEC1( 1)-- Reserved
C                            KSEC1( 2)-- CREX Edition number
C                            KSEC1( 3)-- Originating Centre ( oooo)
C                            KSEC1( 4)-- Update sequence number (uu)
C                            KSEC1( 5)-- Number of subsets (sss)
C                            KSEC1( 6)-- CREX data category (nnn)
C                            KSEC1( 7)-- International data sub-category (mmm)
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- Year (yyyy)
C                            KSEC1(10)-- Month (mm)
C                            KSEC1(11)-- Day (dd)
C                            KSEC1(12)-- Hour (hh)
C                            KSEC1(13)-- Minute (mm)
C                            KSEC1(14)-- CREX Master table (tt)
C                            KSEC1(15)-- version number of Master table used (vv)
C                            KSEC1(16)-- Originating sub-centre (ppp)
C                            KSEC1(17)-- Bufr master table version number
C                            KSEC1(18)-- Bufr version number of local table used
C                            KSEC1(19) - KSEC1(40) -- Reserved
C
C        OUTPUT:
C               *KERR*    -  RETURNED ERROR CODE
C
C
C
C
C     METHOD.
C     -------
C
C            NONE
C
C     EXTERNALS.
C     ----------
C
C            NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          MILAN DRAGOSAVAC    *ECMWF*       04/02/2003.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL=40000,
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
      COMMON /CREXWORK/ NBPT,NGS,NVIND,NBPTSEC2,RVIND,EPS,NE,IEOLD,
     1                  NC07YYY

C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
      CHARACTER YCHAR*30,YLONG*320
C
C
      DIMENSION KTDLST(JELEM)
      DIMENSION KTDEXP(JELEM)

      DIMENSION KSUP(*),KSEC1(*)
      REAL*8 VALUES(KVALS)
      REAL*8 RVIND, EPS
C
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT CREX MESSAGE.
C                -------------------
 100  CONTINUE
C
      KERR=0
C
      ISUB1=KSUB1
      ISUB2=KSUB2
      IF(ISUB1.LE.0.OR.ISUB2.LE.0) THEN
         WRITE(*,'(A)')    ' WARNING - NEGATIVE KSUB1 OR KSUB2.'
         WRITE(*,'(A,I5)') ' WARNING - NUMBER OF SUBSETS IS ',KSUP(6)
         RETURN
      END IF
      IF(ISUB1.GT.KSUP(6)) THEN
         WRITE(*,'(A,I5)') ' WARNING - NUMBER OF SUBSETS IS ',KSUP(6)
         RETURN
      END IF
      IF(ISUB2.GT.KSUP(6)) THEN
         ISUB2=KSUP(6)
         WRITE(*,'(A,I5)') ' WARNING - KSUB2 REPLACED BY ',KSUP(6)
      END IF
C
      JQCP1= 0
C
      IF(K.EQ.0) THEN
         JQPR=0
         JQUA=0
         JQC=0
C          DO 171 J171=1,KSUP(5)
C          IF(CNAMES(J171)(1:8).EQ.'DATA PRE') THEN
C            JQPR=J171
C            GO TO 172
C          END IF
C  171     CONTINUE
C  172     DO 173 J173=1,KSUP(5)
C          IF(CNAMES(J173)(1:9).EQ.'QUALITY I') JQUA=J173
C  173     CONTINUE
C          DO 174 J174=1,KSUP(5)
C          IF(CNAMES(J174)(1:3).EQ.'% C') THEN
C            JQC =J174
C            GO TO 175
C          END IF
C  174     CONTINUE
C
C  175     CONTINUE
C
C         WRITE(*,'(1H1)')
C
C         WRITE(*,'(1H ,A)')    'EXPANDED CREX MESSAGE  '
C         WRITE(*,'(1H ,//)')
C         WRITE(*,'(1H ,A,I6)') 'CREX MESSAGE  DATA TYPE   ',KSEC1(6)
C         WRITE(*,'(1H ,A,I6)') 'RDB DATA SUBTYPE          ',KSEC1(7)
C         WRITE(*,'(1H ,A,I6)') 'TOTAL BUFR LENGTH (BYTES) ',KSUP(8)
C
         NTYPE=KSEC1(7)
         IF(JQUA.EQ.0) THEN      !IF(JQUA.NE.0) THEN
            JQUA=KSUP(5)
C
            DO 103 JB=ISUB1,ISUB2
            CALL CREXSEL2(JB,KELEM,KTDLEN,KTDLST,KTDEXL,KTDEXP,CNAMES,
     1                  CUNITS,KERR)
            JQUA=KTDEXL
C
            ILN=0
            WRITE(*,'(1H )')
C
            DO 104 JA=1,JQUA
C
            ILN=ILN+1
            JAJB=JA+(JB-1)*KELEM
C
            IF(ABS(VALUES(JAJB)-RVIND).LT.EPS) THEN
               WRITE(*,9918) ILN,CNAMES(JA),CUNITS(JA)
            ELSE
               IF(CUNITS(JA)(1:4).EQ.'CCIT') THEN
                  I=NINT(VALUES(JAJB)/1000)
                  NCHAR=VALUES(JAJB)-I*1000
                  NW=NCHAR/80
                  NWOFF=NCHAR-NW*80
                  IF(NWOFF.NE.0) NW=NW+1
C
                  YLONG=' '
                  YLONG(1:80)=CVALS(I)
C
                  II=I
                  DO 125 JC=1,NW-1
                  II=II+1
                  KF=JC*80+1
                  KL=(JC+1)*80
                  YLONG(KF:KL)=CVALS(II)
 125              CONTINUE
C
                  NLINE=NCHAR/30
                  IDIF =NCHAR-NLINE*30
                  IF(IDIF.NE.0) NLINE=NLINE+1
                  YCHAR=' '
                  YCHAR=YLONG(1:30)
C
                  WRITE(*,9919)ILN,CNAMES(JA),VALUES(JAJB),
     1                         CUNITS(JA),YCHAR
C
                  IF(NLINE.GT.1) THEN
                     DO 130 JJ=1,NLINE-1
C
                     K2=JJ*30+1
                     K1=(JJ+1)*30
                     YCHAR=' '
                     YCHAR=YLONG(K2:K1)
C
                     WRITE(*,9920) YCHAR
 130                 CONTINUE
C
                   END IF
               ELSE
                  WRITE(*,9917) ILN,CNAMES(JA),VALUES(JAJB),
     1            CUNITS(JA)
               END IF
            END IF
C
 104        CONTINUE
 103        CONTINUE
C
         ELSE
            JQPRM1=JQPR-1
            JQC=JQC-1
C
            DO 101 JB=ISUB1,ISUB2
C
            ILN=0
            JQCP1=0
C
            WRITE(*,'(1H )')
C
            DO 102 JA=1,JQUA-1
C
            ILN=ILN+1
            JAJB=JA+(JB-1)*KELEM
            JQPJB=JQPRM1+JA+(JB-1)*KELEM
C
            IF(VALUES(JQPJB).EQ.0.0) THEN
               JQCP1=JQCP1+1
               JQCPP1=JQC+JQCP1+(JB-1)*KELEM
               IF(ABS(VALUES(JAJB)-RVIND).LT.EPS) THEN
                  WRITE(*,9918) ILN,CNAMES(JA),CUNITS(JA)
               ELSE
                  WRITE(*,9916) ILN,CNAMES(JA),VALUES(JAJB),
     1            CUNITS(JA),
     1            CNAMES(JQC+JQCP1),VALUES(JQCPP1),
     1            CUNITS(JQC+JQCP1)
               END IF
            ELSE
               IF(ABS(VALUES(JAJB)-RVIND).LT.EPS) THEN
                  WRITE(*,9918) ILN,CNAMES(JA),CUNITS(JA)
                  IF(NTYPE.EQ.5.OR.NTYPE.EQ.3) JQCP1=JQCP1+1
               ELSE
                  WRITE(*,9917) ILN,CNAMES(JA),VALUES(JAJB),
     1            CUNITS(JA)
                  IF(NTYPE.EQ.5.OR.NTYPE.EQ.3) JQCP1=JQCP1+1
               END IF
            END IF
C
C
 102        CONTINUE
 101        CONTINUE
C
         END IF
      END IF
C
      IF(K.EQ.1) THEN
C
C---------------------------------------------------------------------
          WRITE(*,'(1H1)')
C
          WRITE(*,'(1H ,A)') 'WARNING : PRINTING CONTENT OF CODE'//
     1   ' TABLES NOT YET IMPLEMENTED.'
          RETURN
C---------------------------------------------------------------------
C
C          WRITE(*,'(1H ,A)')    'EXPANDED CREX MESSAGE  '
C          WRITE(*,'(1H ,//)')
C          WRITE(*,'(1H ,A,I6)') 'RDB DATA TYPE             ',KSEC1(6)
C          WRITE(*,'(1H ,A,I6)') 'RDB DATA SUBTYPE          ',KSEC1(7)
C          WRITE(*,'(1H ,A,I6)') 'TOTAL BUFR LENGTH (BYTES) ',KSUP(8)
C
C          DO 150 JB=1,KSUP(6)
C
C          WRITE(*,'(1H )')
C
C          DO 160 JA=1,KSUP(5)
C
C          JAJB=JA+(JB-1)*KELEM
C
C          IF(ABS(VALUES(JAJB)-RVIND).LT.EPS) THEN
C             WRITE(*,9903) CNAMES(JA)(1:32),CUNITS(JA)
C             WRITE(*,9903) CNAMES(JA)(33:64)
C          ELSE
C             IF(CUNITS(JA)(1:10).EQ.'CODE TABLE'.OR.
C     1         CUNITS(JA)(1:9) .EQ.'CCITTIA5'     ) THEN
C                I=NINT(VALUES(JAJB)/1000)
C                NCHAR=VALUES(JAJB)-I*1000
C                NW=NCHAR/80
C                NWOFF=NCHAR-NW*80
C                IF(NWOFF.NE.0) NW=NW+1
CC
C                YLONG(1:80)=CVALS(I)
C
C                II=I
C                DO 165 JC=1,NW-1
C                II=II+1
C                KF=JC*80+1
C                KL=(JC+1)*80
C                YLONG(KF:KL)=CVALS(II)
C  165           CONTINUE
C
C                NLINE=NCHAR/30
C                IDIF =NCHAR-NLINE*30
C                IF(IDIF.NE.0) NLINE=NLINE+1
C                YCHAR=YLONG(1:30)
C
C                WRITE(*,9904)CNAMES(JA)(1:32),VALUES(JAJB),
C     1         CUNITS(JA),YCHAR
C                WRITE(*,9904)CNAMES(JA)(33:64)
C
C                IF(NLINE.GT.1) THEN
C                   DO 170 JJ=1,NLINE-1
C
C                   K2=JJ*30+1
C                   K1=(JJ+1)*30
C                   YCHAR=YLONG(K2:K1)
C
C                   WRITE(*,9905) YCHAR
C  170              CONTINUE
C
C                END IF
C             ELSE
C                WRITE(*,9906) CNAMES(JA)(1:32),VALUES(JAJB),
C     1                       CUNITS(JA)
C                WRITE(*,9906) CNAMES(JA)(33:64)
C             END IF
C          END IF
C
C  160     CONTINUE
C  150     CONTINUE
C
        END IF
C
C
C       RETURN
C
C     ------------------------------------------------------------------
C
 200  CONTINUE
C
C     ------------------------------------------------------------------
 9903 FORMAT(1H ,A,'     MISSING',2X,A)
 9904 FORMAT(1H ,A,F20.4,2X,A,2X,A)
 9905 FORMAT(1H ,100X,A)
 9906 FORMAT(1H ,I5,1X,A,E14.4,2X,A)
 9916 FORMAT(1H ,I5,1X,A15,1X,F20.4,1X,A20,1X,A15,1X,F3.0,1X,A15)
 9917 FORMAT(1H ,I5,1X,A15,1X,F20.4,1X,A24)
 9918 FORMAT(1H ,I5,1X,A15,1X,'             MISSING',1X,A24)
 9919 FORMAT(1H ,I5,1X,A15,1X,F20.4,1X,A24,1X,A)
 9920 FORMAT(1H ,62X,A)
      END
