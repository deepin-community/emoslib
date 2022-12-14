C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CTABLE1(YNAME,KERR)
C
C**** *CTABLE1*
C
C
C     PURPOSE.
C     --------
C          THE MAIN PURPOSE OF THIS PROGRAMME IS TO CREATE WORKING
C          CODE TABLES FOR *BUFR* DECODING.
C
C**   INTERFACE.
C     ----------
C          NONE.
C
C
C
C
C     *METHOD.
C      -------
C          NONE.
C
C
C
C     EXTERNALS.
C     ----------
C          NONE.
C
C
C
C
C     REFERENCE.
C     ----------
C
C          BINARY UNIVERSAL FORM FOR DATA REPRESENTATION, *FM 94 BUFR*.
C
C          J.K.GIBSON AND *M.DRAGOSAVAC,1987:* DECODING *DATA *REPRESENTATION
C                          *FM 94 BUFR*,*TECHNICAL *MEMORANDUM *NO. 134
C
C          J.K.GIBSON,1986:*EMOS 2 - *STANDARDS FOR SOFTWARE DEVELOPMENT
C                           AND MAINTANANCE *,*TECHICAL MEMORANDUM *NO.
C                           *ECMWF*.
C
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC       *ECMWF*       JANUARY 1991.
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
#     include "parameter.F"
#     include "bcmwork.F"
#     include "bcmtab.F"
#     include "bcmtabc.F"
#     include "bcmct.F"
#     include "bcmctc.F"
#     include "bcmroot.F"

C
      CHARACTER*64 CTEXT
      CHARACTER*64 CTABBEN
      CHARACTER*24 CTABBU
      CHARACTER*256 YFNAME
      CHARACTER*(*) YNAME
      CHARACTER*120 YENTRY
C
      REAL*8 RVIND,EPS
C
C     ------------------------------------------------------------------
C*          1.   SET INITIAL CONSTANTS AND POINTERS
C                ----------------------------------
 100  CONTINUE
C
      J=0
      YFNAME=' '
C
      DO 101 I=1,JCTEXT
      CTEXT(I)=' '
 101  CONTINUE
C
      DO 102 I=1,JCTAB
      NREF(I)=0
      NSTART(I)=0
      NLEN(I)=0
 102  CONTINUE
C
      DO 103 I=1,JCTST
      NCODNUM(I)=0
      NSTARTC(I)=0
      NLENC  (I)=0
 103  CONTINUE
C
      II=INDEX(YNAME,' ')
      II=II-1
      YFNAME=YNAME(1:II)
      II=INDEX(YFNAME,' ')
      II=II-1
      CALL GET_FREE_UNIT(IUNIT)
      OPEN(UNIT=IUNIT,FILE=YFNAME(1:II),ERR=401,STATUS='OLD')
C
C     ------------------------------------------------------------------
C*          2.   READ IN CODE TABLE ENTRY
C                ------------------------
 200  CONTINUE
C
C
      READ(IUNIT,'(A)',ERR=402,END=300) YENTRY
C
      J = J+1
C
      IF(J.GT.JCTAB) THEN
         PRINT*,' DIMENSION TOO SMALL J=',J
         kerr=66
         return
      END IF 
C
C     ------------------------------------------------------------------
C*          2.1  SET ARRAYS FOR CODE TABLE TABLE REFERENCE, STARTING POINTERS
C                FOR LIST OF CODE NUMBERS, LENGTH , LIST OF CODE NUMBERS,
C                STARTING POINTERS AND LENGTH OF TEXT INFORMATION.
 210  CONTINUE
C
      READ(YENTRY,'(I6,1X,I4,1X,I4,1X,I2)') NREF(J),NLEN(J),NCODE,NLINE
    
C
      IF(J.EQ.1) THEN
         NSTART (J)  = 1
         NSTARTC(J)  = 1
         IPT = 1
         IIPT= 1
      ELSE
         NSTART(J)   = NSTART(J-1) + NLEN(J-1)
         IPT         = NSTART(J)
         IIPT        = IIPT + 1
         NSTARTC(IPT)= IIPT
      END IF
C
C
      NCODNUM(IPT)=NCODE
      NLENC ( IPT)=NLINE
C
      CTEXT (IIPT)=YENTRY(21:80)
C     -------------------------------------------------------------------
      IF(NLENC(IPT).GT.1) THEN
         DO 220 JA=1,NLENC(IPT)-1
         READ(IUNIT,'(A)',END=300) YENTRY
         IIPT=IIPT+1
         CTEXT(IIPT)=YENTRY(21:80)
 220     CONTINUE
      END IF
C
      IF(NLEN(J).GT.1) THEN
         DO 230 JA=1,NLEN(J)-1
         READ(IUNIT,'(A)',END=300) YENTRY
         READ(YENTRY,'(12X,I4,1X,I2)') NCODE,NLINE
         IPT   = IPT + 1
         IIPT  =IIPT + 1
         NCODNUM(IPT)= NCODE
         NSTARTC(IPT)=  IIPT
         NLENC  (IPT)=NLINE
         CTEXT(IIPT) = YENTRY(21:80)
         IF(NLENC(IPT).GT.1) THEN
            DO 240 JB=1,NLENC(IPT)-1
            READ(IUNIT,'(A)',END=300) YENTRY
            IIPT=IIPT+1
            CTEXT(IIPT)=YENTRY(21:80)
 240        CONTINUE
         END IF
 230     CONTINUE
      END IF
C
      GO TO 200
C
C     ------------------------------------------------------------------
 300  CONTINUE
C
      CLOSE(IUNIT)
C
C     write(*,'(1h )')
C     write(*,'(1H ,a,i4)') 'Total number of entries in Table C is ',j
C
      RETURN
C     -----------------------------------------------------------------
 400  CONTINUE
C
 401  CONTINUE
C
      KERR=62
      WRITE(*,4401) IOS,YFNAME
 4401 FORMAT(1H ,'Open error ',i4,' on ',a)
      RETURN
C
 402  CONTINUE
      KERR=7
      WRITE(*,4402) IOS,YFNAME
 4402 FORMAT(1H ,'Read error ',i4,' on ',a)
      RETURN
C
      END
