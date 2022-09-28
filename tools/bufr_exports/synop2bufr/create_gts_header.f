       PROGRAM CREATE_GTS_HEADER
c
c**** *CREATE_GTS_HEADER*
c
c
c     PURPOSE.
c     --------
c         Create GTS Meteorologibal message
c
c
c**   INTERFACE.
c     ----------
c
c          NONE.
c
c     METHOD.
c     -------
c
c          NONE.
c
c
c     EXTERNALS.
c     ----------
c
c
c     REFERENCE.
c     ----------
c
c          NONE.
c
c     AUTHOR.
c     -------
c
c          MILAN DRAGOSAVAC    *ECMWF*       2008/12/18
c
c
c     MODIFICATIONS.
c     --------------
c
c          NONE.
c
c

      IMPLICIT NONE
c


C  BUFR


      integer    JSUP
      integer    JSEC0
      integer    JSEC1
      integer    JSEC2
      integer    JSEC3
      integer    JSEC4
      integer    JELEM
      integer    JSUBS
      integer    JCVAL
      integer    JBUFL
      integer    JBPW
      integer    JTAB
      integer    JCTAB
      integer    JCTST
      integer    JCTEXT
      integer    JWORK
      integer    JKEY
      integer    JBYTE
      integer    KELEM,KVALS
      real*8     RVIND
      integer    NVIND
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2=4096 ,JSEC3=    4,
     1        JSEC4=   2,JELEM=320000,JSUBS=400,JCVAL=150 ,JBUFL=300000,
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200,
     3          JWORK=4096000,JKEY=46,JBYTE=440000)
C
C
      PARAMETER (KELEM=320000)
      PARAMETER (KVALS=4096000)


      PARAMETER (RVIND=1.7D38)
      PARAMETER (NVIND=2147483647)

      integer   KBUFF(JBUFL)
      integer   KSUP(JSUP)
      integer   KSEC0(JSEC0)
      integer   KSEC1(JSEC1)
      integer   KSEC2(JSEC2)
      integer   KSEC3(JSEC3)
      integer   KSEC4(JSEC4)
      integer   KEY(JKEY)
      integer   KREQ(2)
      integer   KTDLST(JELEM)
      integer   KTDEXP(JELEM)
      integer   KDATA(200)

      integer   KBUFL

      real*8    VALUES(KVALS)

      character*64   CNAMES(KELEM)
      character*24   CUNITS(KELEM)
      character*80   CVALS(KELEM)

      integer                     KTDLEN,KDLENG
      integer                     KPMISS, KPRUS, NOKEY, NOFL, ILEN
      integer                     KERR

c
c 
c
      CHARACTER*256 CARG(10)
c
      CHARACTER*1 ETX
      CHARACTER*2 CII
      CHARACTER*3 CBBB,CRCRLF,SEQUENCE
      CHARACTER*6 CDATE,CDATEOLD
      CHARACTER*4 AHR
      CHARACTER*6 TTAAII
      CHARACTER*4 CCCC, SOHCRCRLF
      CHARACTER*256 CINFILE
      CHARACTER*256 COUTFILE
      CHARACTER*120000 YBUFR,YOUT
      CHARACTER*8 TOTALSIZE

      INTEGER NARG,IARGC,J,IN,IO,IUNIT,IRET,IUNIT1,INNN,IERR,N,II
      INTEGER KBUFR(JBUFL)
      INTEGER KBUFS(JBUFL)

      EQUIVALENCE(YBUFR,KBUFS)
      EQUIVALENCE(YOUT,KBUFR)
c     ------------------------------------------------------------------
c*          1. INITIALIZE CONSTANTS AND VARIABLES.
c              -----------------------------------
 100  CONTINUE
 
c     General abbreviated header is T1T2A1A2ii


      CRCRLF=char(13)//char(13)//char(10)
      SOHCRCRLF=char(1)//char(13)//char(13)//char(10)
      ETX=char(3)
 
c     GET INPUT AND OUTPUT FILE NAME.
 
      NARG=IARGC()
 
      DO J=1,NARG
      CALL GETARG(J,CARG(J))
      END DO

      cinfile=' '
      coutfile=' '
      cbbb=' '
      cccc=' '
      ahr=' '
      cdate=' '

      DO J=1,NARG,2
      IF(CARG(J).EQ.'-i') THEN
         CINFILE=CARG(J+1)
         IN=index(CINFILE,' ')
         IN=IN-1
      ELSEIF(CARG(J).EQ.'-o') THEN
         COUTFILE=CARG(J+1)
         IO=index(COUTFILE,' ')
         IO=IO-1
      ELSEIF(CARG(J).EQ.'-t') THEN
         TTAAII=CARG(J+1)
      ELSEIF(CARG(J).EQ.'-c') THEN
         CCCC=CARG(J+1)
      ELSEIF(CARG(J).EQ.'-b') THEN
         CBBB=CARG(J+1)
      END IF
      END DO
 
      IF(CINFILE .EQ.' '.OR. COUTFILE .EQ.' '.OR. TTAAII .EQ.' '
     1   .OR. CCCC.EQ.' ') THEN
         PRINT*,'USAGE -- create_gts_header -i infile -o outfile',
     1   ' -t T1T2A1A2ii -c CCCC -b BBB'
         STOP
      END IF
 
c*          1.2 OPEN FILE CONTAINING BUFR DATA.
 120  CONTINUE

      CALL PBOPEN(IUNIT,CINFILE(1:IN),'R',IRET)
      IF(IRET.EQ.-1) STOP 'OPEN FAILED ON INPUT FILE'
      IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'

      CALL PBOPEN(IUNIT1,COUTFILE(1:IO),'W',IRET)
      IF(IRET.EQ.-1) STOP 'OPEN FAILED ON OUTPUT FILE'
      IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'
 
c     ----------------------------------------------------------------- 
 
      INNN=0
      CDATEOLD=' '
 
c*          3.  READ BUFR MESSAGE.
c               ------------------
 300  CONTINUE
 
      IERR=0
      KBUFL=0
 
      CALL PBBUFR(IUNIT,KBUFS,JBYTE*4,KBUFL,IERR) 
      IF(IERR.EQ.-1) THEN
         PRINT*,'NUMBER OF MESSAGES    ',N
         STOP 'EOF'
      END IF
      IF(IERR.EQ.-2) STOP 'FILE HANDLING PROBLEM' 
      IF(IERR.EQ.-3) STOP 'ARRAY TOO SMALL FOR PRODUCT'
 
      INNN=INNN+1
      if(INNN.GT.999) INNN=1
      write(SEQUENCE(1:3),'(I3.3)') INNN
      N=N+1
c     PRINT*,'----------------------------------',N,' ',KBUFL
c
c     -----------------------------------------------------------------
c*          4. EXPAND BUFR MESSAGE.
c              --------------------
 400  CONTINUE
c
      CALL BUS012( KBUFL,KBUFS,KSUP,KSEC0,KSEC1,KSEC2,IERR)
      IF(IERR.NE.0) THEN
         PRINT*,'ERROR IN BUS012: ',IERR
         PRINT*,' BUFR MESSAGE NUMBER ',N,' CORRUPTED.'
         IERR=0
         GO TO 300
      END IF
c
      write(CDATE(1:2),'(I2.2)') KSEC1(11)
      write(CDATE(3:4),'(I2.2)') KSEC1(12)
      write(CDATE(5:6),'(I2.2)') KSEC1(13)
      if(CDATE.eq.CDATEOLD) THEN
         II=II+1
      else
         CDATEOLD=CDATE
      end if
      write(CII(1:2),'(i2.2)') ii
      yout(9:10)='00'
      yout(11:14)=SOHCRCRLF
      yout(15:17)=SEQUENCE
      yout(18:20)=CRCRLF
      yout(21:26)=TTAAII
C     yout(25:26)=CII
      yout(27:27)=' '
      yout(28:31)=CCCC
      yout(32:32)=' '
      yout(33:38)=CDATE
      if(CBBB.ne.' ') THEN
         yout(39:39)=' '
         yout(40:42)=CBBB
         yout(43:45)=CRCRLF
         yout(46:46+KBUFL-1)=YBUFR(1:KBUFL)
         yout(46+KBUFL:)=CRCRLF//ETX
         ILEN=KSEC0(2)+39
         write(TOTALSIZE,'(i8.8)') ILEN
         yout(1:8)=TOTALSIZE
         ILEN=ILEN+10
      else
         yout(39:41)=CRCRLF
         yout(42:42+KBUFL-1)=YBUFR(1:KBUFL)
         ilen=42+KBUFL-1
         yout(42+KBUFL:)=CRCRLF//ETX
         ILEN=KSEC0(2)+35
         write(TOTALSIZE,'(i8.8)') ILEN
         yout(1:8)=TOTALSIZE
         ILEN=ILEN+10
      end if
c
      CALL PBWRITE(IUNIT1,KBUFR,ILEN,IERR)
      IF(IERR.LT.0) THEN
         PRINT*,'ERROR WRITING INTO TARGET FILE.'
         CALL EXIT(2)
      END IF
      PRINT*,'RECORD WRITTEN INTO FILE '
c
       GO TO 300
      END 
