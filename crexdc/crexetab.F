C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXETAB(KSEC1,KERR)
C
C**** *CREXETAB*
C
C
C     PURPOSE.
C     --------
C          Load Crex table B and  D  according to Edition and version
C     of Crex code.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *CREXETAB(KSEC1,KERR)*
C
C        OUTPUT:
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
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          MILAN DRAGOSAVAC    *ECMWF*       06/11/2003.
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
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL=40000,
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
      PARAMETER(JP=3000,JL=20)
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
      COMMON /CRROOT/ CROOT
C
C            croot    -  path for Crex tables
C
      CHARACTER*64 CREXNAME
      CHARACTER*24 CREXUNIT
      CHARACTER*6  CREXLST
C
      CHARACTER*256 YNAME
      CHARACTER*256  YFNAME
      CHARACTER*160 YENTRY
C
      CHARACTER*256 YTAB ,YTAC ,YTAD
      CHARACTER*7  YTABB,YTABC,YTABD
      CHARACTER*256 CROOT
      CHARACTER*6 CNCREXBR
C
      DIMENSION KSEC1(*)
C
C     EXTERNAL FUNCTIONS
C
      INTEGER EMOSNUM
      EXTERNAL EMOSNUM

      SAVE OFIRST, NXXP, NYYP, NZZP
C
C     ------------------------------------------------------------------
C
C*          1.   GET CREX TABLES/LOCAL CREX TABLES.
C                ----------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0) RETURN
C
C
C*          2.   SET UP CREX TABLE FILE NAME.
C                ----------------------------
 200  CONTINUE
C
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
            IXX=0
            IYY=KSEC1( 2)
            IZZ=KSEC1(15)
C
         IF(OFIRST) THEN
            IF(IXX.EQ.NXXP.AND.IYY.EQ.NYYP.AND.
     1         IZZ.EQ.NZZP) RETURN
         END IF
C
      J=0
      ICLASS0=0
      IVIND=2147483647
C
      DO 101 I=1,64
      do 101 ii=1,255
      NCREXP(I,ii)=0
 101  CONTINUE
C
      DO 102 i=1,JP
      NCREXBR(I)=IVIND
      NCREXBS (I)=IVIND
      NCREXBDW(I)=IVIND
      CREXNAME(I)=' '
      CREXUNIT(I)=' '
 102  CONTINUE
C
      J  =0
      IST=1
C
      DO 103 I=1,JP
      NCREXDR(I)=999999
      NCREXDL (I)=0
      NCREXDST(I)=0
 103  CONTINUE
C
      DO 104 I=1,JP*20
      NCREXDSQ(I)=0
 104  CONTINUE

      OFIRST=.TRUE.
C
      NXXP=IXX
      NYYP=IYY
      NZZP=IZZ
C
      WRITE(YTABB,'(A1,I2.2,I2.2,I2.2)') 'B',IXX,IYY,IZZ
C     WRITE(YTABC,'(A1,I2.2,I2.2,I2.2)') 'C',IXX,IYY,IZZ
      WRITE(YTABD,'(A1,I2.2,I2.2,I2.2)') 'D',IXX,IYY,IZZ
C
      PRINT*,'CREX Tables to be loaded ',YTABB,',',YTABD
C
C     ----------------------------------------------------------------
C*          3. OPEN AND READ FILES CONTAINING CREX TABLES.
C              -------------------------------------------
 300  CONTINUE
C
      CROOT=' '
      CALL GETENV('CREX_TABLES',CROOT)
      ILNG=INDEX(CROOT,' ')
      IF(ILNG.EQ.1) THEN
C
C       SGI/HP/SUN CREX TABLES PATH
C
        CROOT='/home/ma/emos/tables/crex/'
C
C       ADD VERSION NUMBER
C
        ILNG=INDEX(CROOT,' ')
        IEMOSNM = EMOSNUM(1)
        WRITE(CROOT(ILNG:ILNG+5),'(I6.6)') IEMOSNM
        CROOT(ILNG+6:) = '/'
      ENDIF

      I=INDEX(CROOT,' ')
      IF(I.NE.0) I=I-1
      PRINT*,'Path for CREX tables:',CROOT(1:I)
C
C*          3.1 READ CREX TABLE B.
C               ------------------
 310  CONTINUE
C
      YTAB=CROOT(1:I)//YTABB
      II=I+7
C
      OPEN(UNIT=38,IOSTAT=IOS,ERR=311,FILE=YTAB(1:II),
     1     FORM='FORMATTED',
     2     recl=160,
     5     STATUS='OLD')
C
      J=0
      GO TO 312
C
 311  CONTINUE
C
      CLOSE(38)
C
      print*,'Open error on ',YTAB(1:II)
C
      KERR=9    
      CALL CREXERR(KERR)
      RETURN

C
 312  CONTINUE
C
      J=J+1
      READ(38,ERR=400,IOSTAT=IOS,END=329,
     1  FMT='(1x,A,1x,A64,47x,A24,I3,7x,I3)')
     2  CNCREXBR,CREXNAME(J),CREXUNIT(J),NCREXBS(J),NCREXBDW(J)
C
      CNCREXBR(1:1)='0'
C
      READ(CNCREXBR,'(I6.6)',IOSTAT=IOS) NCREXBR(J)
      IF(IOS.NE.0) THEN
         print*,'Internal read error.'
         print*,CNCREXBR
         KERR=35
         RETURN
      end if
C
C
      ICLASS=NCREXBR(J)/1000
      IYYY  =NCREXBR(J)-ICLASS*1000+1
C
      ICLASS=ICLASS+1
      NCREXP(ICLASS,IYYY)=J

      GO TO 312
C
 329  CONTINUE
C
      CLOSE(UNIT=38,IOSTAT=IOS,ERR=420)
C
C*          3.3 READ CREX TABLE D.
CC               ------------------
 330  CONTINUE
C
      YTAD=CROOT(1:I)//YTABD
C
      OPEN(UNIT=40,IOSTAT=IOS,ERR=331,FILE=YTAD(1:II),
     1     FORM='FORMATTED',
     2     recl=160,
     5     STATUS='OLD')
C
      GO TO 332
C
 331  CONTINUE
C
      CLOSE(40)
C
      print*,'Open error on ',YTAD
C
      kerr=9
      CALL CREXERR(KERR)
      return
C
 332  CONTINUE
C
      j=0
 340  continue
c
      YENTRY=' '
      READ(40,'(a)',iostat=ios,end=390) yentry
      if(ios.ne.0) then
         print*,'Read error on CREX table D.'
         KERR=35
         RETURN
      end if
      if(yentry(2:2).eq.'D') yentry(2:2)='3'
c
      j=j+1
      READ(YENTRY,'(1X,I6,1X,I2)',iostat=ios) NCREXDR(J),NCREXDL(J)
      if(ios.ne.0) then
         print*,'Internal read error.'
         print*,YENTRY
         KERR=35
         RETURN
      end if
      IF(J.GT.JP) THEN
         PRINT*,' DIMENSION TOO SMALL J=',J
         kerr=13
         CALL CREXERR(KERR)
         RETURN
      END IF
C
      IF(J.EQ.1) THEN
         IST=1
         NCREXDST(J)=IST
      ELSE
         IST=IST + NCREXDL(J-1)
         NCREXDST(J)=IST
      END IF
C
      IF(NCREXDL(J).GT.1) THEN
         if(YENTRY(12:12).eq.'D') YENTRY(12:12)='3'
         if(YENTRY(12:12).eq.'C') then
            YENTRY(12:12)='2'
c           checkif negative scale is present
            iminus=0
            iminus=index(YENTRY(12:16),'-')
            if(iminus.ne.0) YENTRY(iminus:iminus)='9'
         end if
         if(YENTRY(12:12).eq.'R') YENTRY(12:12)='1'
         if(YENTRY(12:12).eq.'B') YENTRY(12:12)='0'
         READ(YENTRY,'(11X,I6)',iostat=ios) NCREXDSQ(IST)
         if(ios.ne.0) then
            print*,YENTRY
            KERR=35
            CALL CREXERR(KERR)
            RETURN
         end if
c
         IIST=IST
C
         DO 220 JA=1,NCREXDL(J)-1
         IIST=IIST+1
         READ(40,'(A)',END=300,iostat=ios) YENTRY
         if(ios.ne.0) then
            print*,YENTRY
            KERR=35
            CALL CREXERR(KERR)
            RETURN
         end if
c
         if(YENTRY(12:12).eq.'D') YENTRY(12:12)='3'
         if(YENTRY(12:12).eq.'C') YENTRY(12:12)='2'
         if(YENTRY(12:12).eq.'R') YENTRY(12:12)='1'
         if(YENTRY(12:12).eq.'B') YENTRY(12:12)='0'
         READ(YENTRY,'(11X,I6)',iostat=ios) NCREXDSQ(IIST)
         if(ios.ne.0) then
            print*,YENTRY
            KERR=35
            CALL CREXERR(KERR)
            RETURN
         end if
 220     CONTINUE
C
      ELSE
         if(YENTRY(12:12).eq.'D') YENTRY(12:12)='3'
         if(YENTRY(12:12).eq.'C') YENTRY(12:12)='2'
         if(YENTRY(12:12).eq.'R') YENTRY(12:12)='1'
         if(YENTRY(12:12).eq.'B') YENTRY(12:12)='0'
         READ(YENTRY,'(11X,I6)',iostat=ios) NCREXDSQ(IST)
         if(ios.ne.0) then
            print*,'Internal read error.'
            print*,YENTRY
            KERR=35
            RETURN
         end if
      END IF
C
      go to 340
c
 390  continue
C
      CLOSE(UNIT=40,IOSTAT=IOS,ERR=620)
C
C
      RETURN
C     ----------------------------------------------------------------
 400  CONTINUE
C
      KERR=6
      PRINT*,'CREXETAB: IOS ',IOS
      CALL CREXERR(KERR)
      OFIRST=.FALSE.
      RETURN
C
 410  CONTINUE
C
      KERR=9
      PRINT*,'CREXETAB: IOS ',IOS
      CALL CREXERR(KERR)
      OFIRST=.FALSE.
      RETURN
C
 420  CONTINUE
C
      KERR=10
      PRINT*,'CREXETAB: IOS ',IOS
      CALL CREXERR(KERR)
      OFIRST=.FALSE.
      RETURN
C     ----------------------------------------------------------------
 500  CONTINUE
C
      KERR=7
      PRINT*,'CREXETAB: IOS ',IOS
      CALL CREXERR(KERR)
      OFIRST=.FALSE.
      RETURN
C
 510  CONTINUE
C
      KERR=9
      PRINT*,'CREXETAB: IOS ',IOS
      CALL CREXERR(KERR)
      OFIRST=.FALSE.
      RETURN
C
 520  CONTINUE
C
      KERR=11
      PRINT*,'CREXETAB: IOS ',IOS
      CALL CREXERR(KERR)
      OFIRST=.FALSE.
      RETURN
C     -----------------------------------------------------------------
 600  CONTINUE
C
      KERR=8
      PRINT*,'CREXETAB: IOS ',IOS
      CALL CREXERR(KERR)
      OFIRST=.FALSE.
      RETURN
C
 610  CONTINUE
C
      KERR=9
      PRINT*,'CREXETAB: IOS ',IOS
      CALL CREXERR(KERR)
      OFIRST=.FALSE.
      RETURN
C
 620  CONTINUE
C
      KERR=12
      PRINT*,'CREXETAB: IOS ',IOS
      CALL CREXERR(KERR)
      OFIRST=.FALSE.
      RETURN
C     -----------------------------------------------------------------
C
      END
