C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXTABLES(KSEC1,CREXBTR,CREXBS,CREXBDW,CREXNAMES,
     1                      CREXUNITS,CREXBP,KERR)
C
C**** *BTABLE*
C
C
C     PURPOSE.
C     --------
C          CREATE CREX TABLE B IN BINARY FORM.
C          THIS ROUTINE IS USED IN BUFR2CREX PROGRAM
C
C**   INTERFACE.
C     ----------
C          NONE.
C
C
C     *METHOD.
C      -------
C          NONE.
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
C     AUTHOR.
C     -------
C
C          MILAN DRAGOSAVAC       *ECMWF*       JANUARY 2003.
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
c
      PARAMETER(JP=3000)
C
      CHARACTER*64 CREXNAMES(JP)
      CHARACTER*24 CREXUNITS(JP)
      CHARACTER*160 YENTRY
      CHARACTER*256  YFNAME
C
      DIMENSION KSEC1(*)
      INTEGER CREXBTR(JP),CREXBS(JP),CREXBDW(JP)
      INTEGER CREXBP(64,255)
C
      CHARACTER*7   YTABB
      CHARACTER*256 CROOT
      INTEGER IEMOSNM
C
      INTEGER  EMOSNUM
      EXTERNAL EMOSNUM
C
      SAVE NXXP,NYYP,NZZP, OFIRST
C
C     ------------------------------------------------------------------
C*          1.   INITIALIZE CONSTANTS AND VARIABLES.
C                -----------------------------------
 100  CONTINUE
C
      J=0
      ICLASS0=0
      IVIND=2147483647
C
      CROOT=' '
      CALL GETENV('CREX_TABLES',CROOT)
      ILNG=INDEX(CROOT,' ')
      ILNG=ILNG-1
      IF(ILNG.EQ.1) THEN
C
C        SGI/HP/SUN CREX TABLES PATH
C
         CROOT='/home/ma/emos/tables/crex/'
C
C        ADD VERSION NUMBER
C
         ILNG=INDEX(CROOT,' ')
         IEMOSNM = EMOSNUM(1)
         WRITE(CROOT(ILNG:ILNG+5),'(I6.6)') IEMOSNM
         CROOT(ILNG+6:) = '/'
         ILNG=ILNG+6
      END IF
C
C             CREX EDITION 1 NAMING CONVENTION
C
C             BXXYYZZ , DXXYYZZ
C
C             B      - CREX TABLE 'B'
C             D      - CREX TABLE 'D'
C             XX     - Crex Master table used '00'
C             YY     - Crex Edition number used '01'
C             ZZ     - Crex Table version number '01'
C
       IXX=KSEC1(14)   ! BUFR Master Table 0
       IYY=KSEC1( 2)   ! BUFR Edition 
       IZZ=KSEC1(15)   ! Version of Master table
C
       IF(OFIRST) THEN
          IF(IXX.EQ.NXXP.AND.IYY.EQ.NYYP.AND.
     1       IZZ.EQ.NZZP) RETURN
       END IF
C
       WRITE(*,'(1H ,A)') 'Your path for crex tables is :'
       WRITE(*,'(1H ,A)')  CROOT(1:ILNG)

       OFIRST=.TRUE.

       NXXP=IXX
       NYYP=IYY
       NZZP=IZZ
C
C
      DO 101 I=1,64
      DO 101 II=1,255
       CREXBP(I,II)=0
 101  CONTINUE
C
      DO 102 I=1,JP
       CREXBTR(I)=IVIND
       CREXBS(I)=IVIND
       CREXBDW(I)=IVIND
       CREXNAMES(I)=' '
       CREXUNITS(I)=' '
 102  CONTINUE

C
      WRITE(YTABB,'(A1,I2.2,I2.2,I2.2)',iostat=ios) 'B',IXX,IYY,IZZ
      IF(IOS.NE.0) then
        KERR=33
        CALL CREXERR(KERR)
        RETURN
      END IF
C
      print*,'Crex table to be loaded :',YTABB
C
      YFNAME=CROOT(1:ILNG)//YTABB
C
      DO 301 JJ=1,99
       INQUIRE(UNIT=JJ,IOSTAT=IOS,OPENED=OPN)
       IF(.NOT.OPN) THEN
         IUNIT=JJ
         OPEN(UNIT=IUNIT,IOSTAT=IOS,ERR=401,FILE=YFNAME,
     4     FORM='FORMATTED',
     5      RECL=160,
     5     STATUS='OLD')
         GO TO 200
       END IF
 301   CONTINUE
C
C     ------------------------------------------------------------------
C*          2.   READ IN TABLE B ELEMENT.
C                ------------------------
C
 200  CONTINUE
C
      YENTRY=' '
      READ(IUNIT,'(A)',ERR=402,END=300) YENTRY
C
      J=J+1
      IF(J.GT.JP) THEN
         PRINT*,' DIMENSION TOO SMALL J=',J
         KERR=13
         RETURN
      END IF
C
C     ------------------------------------------------------------------
C*          2.1  SET ARRAYS FOR TABLE REFERENCE, ELEMENT NAME, UNITS,
C*               REFERENCE VALUE AND DATA WIDTH.
C
 210  CONTINUE
C
C
      IMINUS=0
      IMINUS=INDEX(YENTRY(12:16),'-')
      IF(IMINUS.NE.0) YENTRY(IMINUS:IMINUS)='9'
c    1  FMT='(1x,A,1x,A64,47x,A24,I3,8x,I3)')
      READ(YENTRY,'(2X,I5,1x,64x,71x,I3,7x,I3)',IOSTAT=IOS)
     1                                    CREXBTR(J),CREXBS(J),
     1                                    CREXBDW(J)
      IF(IOS.NE.0) THEN
         KERR=35
         CALL CREXERR(KERR)
         print*,'Line number ',j
         RETURN
      END IF
C
      CREXNAMES(J)=YENTRY( 9:72)
      CREXUNITS(J)=YENTRY(120:123)
C
      ICLASS=CREXBTR(J)/1000
      IYYY  =CREXBTR(J)-ICLASS*1000+1
C
      ICLASS=ICLASS+1
      CREXBP(ICLASS,IYYY)=J
C
      GO TO 200
C
 300  CONTINUE
C
      CLOSE(UNIT=IUNIT)
      RETURN
C
C
C
402   CONTINUE
      KERR=35
      CALL CREXERR(KERR)
      WRITE(*,4402) IOS,yfname
 4402 FORMAT(1H ,'Read error',i4,' on ',a)
      RETURN
C
C
 401  CONTINUE
C
      KERR=9
      CALL CREXERR(KERR)
      WRITE(*,4401) IOS,yfname
 4401 FORMAT(1H ,'Open error',i4,' on ',a)
C
      RETURN
      END
