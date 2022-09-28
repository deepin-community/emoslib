C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXPRS1(KSEC1,KSEC3,KTDLEN,KTDLST,KTDEXL,KTDEXP,
     1                    KELEM,CNAMES)
C
C**** *CREXPRS1*
C
C
C     PURPOSE.
C     --------
C           PRINT SECTION 1 OF CREX MESSAGE.
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *CREXPRS1(KSEC1,KSEC3,KTDLEN,KTDLST,KTDEXL,KTDEXP,
C                            KELEM,CNAMES)*
C
C        INPUT :
C
C               *KSEC1*   -  ARRAY CONTAINING SECTION 1 INFORMATION
C                            KSEC1( 1)-- Not used
C                            KSEC1( 2)-- CREX EDITION NUMBER
C                            KSEC1( 3)-- Not used
C                            KSEC1( 4)-- Not Used
C                            KSEC1( 5)-- Not used
C                            KSEC1( 6)-- CREX MESSAGE TYPE
C                            KSEC1( 7)-- Not used
C                            KSEC1( 8)-- VERSION NUMBER OF LOCAL TABLE USED
C                            KSEC1( 9)-- Not used
C                            KSEC1(10)-- Not used
C                            KSEC1(11)-- Not used
C                            KSEC1(12)-- Not used
C                            KSEC1(13)-- Not used
C                            KSEC1(14)-- CREX MASTER TABLE
C                            KSEC1(15)-- VERSION NUMBER OF MASTER TABLE USED
C                            KSEC1(16) - KSEC1(JSEC1) -- Not used
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

C
C               *KSEC3*   -  Integer array of 4 words containing
C                            KSEC3( 1)-- Reserved
C                            KSEC3( 2)-- Reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- Reserved

C               *KTDLEN*  -  number of data descriptors in section 1
C               *KTDLST*  -  array containing data descriptors in section 1
C               *KTDEXL*  -  number of entries in list of expanded data descriptors
C               *KTDEXP*  -  array containig expanded data descriptors
C               *KELEM*   -  Integer (expected number of expanded elements)
C               *CNAMES*  -  Character*64  array of KELEM containing
C                            CREX Table B element names
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
C          MILAN DRAGOSAVAC    *ECMWF*       07/01/2004.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      DIMENSION KSEC1(*)
      DIMENSION KSEC3(*)
      DIMENSION KTDLST(KTDLEN),KTDEXP(KTDEXL)
C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*6  YYYYYY

C
C     ------------------------------------------------------------------
C
C*          1.   PRINT SECTION 1.
C                ----------------
 100  CONTINUE
C
      WRITE(*,'(1H1)')
C
      WRITE(*,'(1H ,A)')    '        CREX SECTION 1    '
      WRITE(*,'(1H )')
      IF(KSEC1(2).LT.2) THEN
       WRITE(*,'(1H ,A,I5)')'CREX EDITION NUMBER            ',KSEC1( 2)
       WRITE(*,'(1H ,A,I5)')'CREX MESSAGE TYPE              ',KSEC1( 6)
       WRITE(*,'(1H ,A,I5)')'VERSION NUMBER OF LOCAL TABLE  ',KSEC1( 8)
       WRITE(*,'(1H ,A,I5)')'VERSION NUMBER OF MASTER TABLE ',KSEC1(15)
       WRITE(*,'(1H ,A,I5)')'CREX MASTER TABLE              ',KSEC1(14)
C
       WRITE(*,'(1H1)')
       WRITE(*,'(1H ,A,I5)')'NUMBER OF SUBSETS              ',KSEC3( 3)
      ELSE
       WRITE(*,'(1H ,A,I5)')'CREX EDITION NUMBER            ',KSEC1( 2)
       WRITE(*,'(1H ,A,I5)')'ORIGINATING CENTRE             ',KSEC1( 3)
       WRITE(*,'(1H ,A,I5)')'ORIGINATING SUB-CENTRE         ',KSEC1(16)
       WRITE(*,'(1H ,A,I5)')'UPDATE SEQUENCE NUMBER         ',KSEC1( 4)
       WRITE(*,'(1H ,A,I5)')'CREX DATA CATEGORY             ',KSEC1( 6)
       WRITE(*,'(1H ,A,I5)')'INTERNATIONAL DATA SUB-CATEGORY',KSEC1( 7)
       WRITE(*,'(1H ,A,I5)')'YEAR                           ',KSEC1( 9)
       WRITE(*,'(1H ,A,I5)')'MONTH                          ',KSEC1(10)
       WRITE(*,'(1H ,A,I5)')'DAY                            ',KSEC1(11)
       WRITE(*,'(1H ,A,I5)')'HOUR                           ',KSEC1(12)
       WRITE(*,'(1H ,A,I5)')'MINUTE                         ',KSEC1(13)
       WRITE(*,'(1H ,A,I5)')'VERSION NUMBER OF MASTER TABLE ',KSEC1(15)
       WRITE(*,'(1H ,A,I5)')'VERSION NUMBER OF LOCAL TABLE  ',KSEC1( 8)
       WRITE(*,'(1H ,A,I5)')'CREX MASTER TABLE              ',KSEC1(14)
       WRITE(*,'(1H ,A,I5)')'BUFR MASTER TABLE NUMBER       ',KSEC1(17)
       WRITE(*,'(1H ,A,I5)')'BUFR VERSION OF LOCAL TABLE    ',KSEC1(18)
C
       WRITE(*,'(1H1)')
       WRITE(*,'(1H ,A,I5)')'NUMBER OF SUBSETS              ',KSEC3( 3)
C
      END IF
C
C
      WRITE(*,'(1H ,//)')
      WRITE(*,'(1H ,A)')    '       DATA DESCRIPTORS (UNEXPANDED)'
      WRITE(*,'(1H )')
C
      WRITE(*,'(1H )')
      DO 110 I=1,KTDLEN
       write(yyyyyy,'(i6.6)') KTDLST(I)
       if(yyyyyy(1:1).eq.'0') yyyyyy(1:1)='B'
       if(yyyyyy(1:1).eq.'1') yyyyyy(1:1)='R'
       if(yyyyyy(1:1).eq.'2') yyyyyy(1:1)='C'
       if(yyyyyy(1:1).eq.'3') yyyyyy(1:1)='D'
       WRITE(*,'(1H ,I4,2X,A)') I,yyyyyy
 110  CONTINUE
C
      WRITE(*,'(1H ,/)')
      WRITE(*,'(1H ,A)')    '       DATA DESCRIPTORS (EXPANDED)'
      WRITE(*,'(1H )')
      DO 120 I=1,KTDEXL
       write(yyyyyy,'(i6.6)') KTDEXP(I)
       if(yyyyyy(1:1).eq.'0') yyyyyy(1:1)='B'
       if(yyyyyy(1:1).eq.'1') yyyyyy(1:1)='R'
       if(yyyyyy(1:1).eq.'2') yyyyyy(1:1)='C'
       if(yyyyyy(1:1).eq.'3') yyyyyy(1:1)='D'
       WRITE(*,'(1H ,I5,2X,A,2X,A)') I,yyyyyy,CNAMES(I)
 120  CONTINUE

      RETURN
      END
