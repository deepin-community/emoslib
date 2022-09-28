      PROGRAM BUFRKEY
C
C**** *BUFRKEY*
C
C
C     PURPOSE.
C     --------
C         PRINTS BUFR SECTION 2 RDB KEY CONTENT.
C
C
C**   INTERFACE.
C     ----------
C
C          NONE.
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
C         CALL BUSEL
C         CALL BUFREX
C         CALL BUFREN
C         CALL BUPRS0
C         CALL BUPRS1
C         CALL BUPRS2
C         CALL BUPRS3
C         CALL BUPRT
C         CALL BUUKEY
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/09/87.
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
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2=4096 ,JSEC3= 4,
     1       JSEC4=   2,JELEM=160000,JSUBS=400,JCVAL=150 ,JBUFL=512000,
#ifdef JBPW_64
     2       JBPW =  64,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200,
#else
     2       JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200,
#endif
     3       JWORK=4096000,JKEY=46,JBYTE=512000)
C
      PARAMETER (KELEM=160000)
      PARAMETER (KVALS=4096000)
C 
      DIMENSION KBUFF(JBUFL)
      DIMENSION KSUP(JSUP)  ,KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION KSEC2(JSEC2),KSEC3(JSEC3),KSEC4(JSEC4)
      DIMENSION KEY  (JKEY),KREQ(2)
C
      REAL*8 VALUES(KVALS),VALUE(KVALS),VALS(KVALS),RQV(KELEM)
      REAL*8 RVIND,EPS
C
      DIMENSION KTDLST(KELEM),KTDEXP(KELEM),KRQ(KELEM)
      DIMENSION KDATA(200),KBOXR(KELEM*4)
C
      CHARACTER*256 CF,CARG(2)
      CHARACTER*64 CNAMES(KELEM),CBOXN(KELEM*4)
      CHARACTER*24 CUNITS(KELEM),CBOXU(KELEM*4)
      CHARACTER*80 CVALS(KVALS)
      CHARACTER*80 CVAL(KVALS)
      CHARACTER*80 YENC
C
cs      EXTERNAL GETARG
C                                                                       
C     ------------------------------------------------------------------
C*          1. INITIALIZE CONSTANTS AND VARIABLES.
C              -----------------------------------
 100  CONTINUE
C
C     MISSING VALUE INDICATOR
C 
      NBYTPW=JBPW/8
      RVIND=1.7D38
      NVIND=2147483647
      IOBS=0
      EPS=1.0D-8
      NPACK=0
C
C     INPUT FILE NAME
C
C
C     GET INPUT AND OUTPUT FILE NAME.
C
      NARG=IARGC()
C
      IF(NARG.NE.2) THEN
         PRINT*,'USAGE -- bufr_key -i infile '
         STOP
      END IF
C
      DO 101 J=1,NARG
      CALL GETARG(J,CARG(J))
 101  CONTINUE
C
      IF(CARG(1).EQ.'-i') THEN
         CF=CARG(2)
      ELSE
          PRINT*,'USAGE -- bufr_key -i infile '
          STOP
      END IF
C
      II=INDEX(CF,' ')
      KRQL=0
      NR=0
      KREQ(1)=0
      KREQ(2)=0
      DO 103 I=1,KELEM
      RQV(I)=RVIND
      KRQ(I)=NVIND
 103  CONTINUE
C
C*          1.2 OPEN FILE CONTAINING BUFR DATA.
C               -------------------------------
 120  CONTINUE
C
      IRET=0 
      CALL PBOPEN(IUNIT,CF(1:II),'R',IRET)
      IF(IRET.EQ.-1) STOP 'OPEN FAILED'
      IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'
C
C     ----------------------------------------------------------------- 
C*          3.  READ BUFR MESSAGE.
C               ------------------
 300  CONTINUE
C
      IERR=0
      KBUFL=0
C
      IRET=0
      CALL PBBUFR(IUNIT,KBUFF,JBYTE,KBUFL,IRET) 
      IF(IRET.EQ.-1) THEN
C         IF(N.NE.0) GO TO 600
         PRINT*,'NUMBER OF SUBSETS     ',IOBS
         PRINT*,'NUMBER OF MESSAGES    ',N
         STOP 'EOF'
      END IF
      IF(IRET.EQ.-2) STOP 'FILE HANDLING PROBLEM' 
      IF(IRET.EQ.-3) STOP 'ARRAY TOO SMALL FOR PRODUCT'
C
      N=N+1
C      PRINT*,'----------------------------------',N
      KBUFL=KBUFL/NBYTPW+1
      IF(N.LT.NR) GO TO 300
C
C     -----------------------------------------------------------------
C*          4. EXPAND BUFR MESSAGE.
C              --------------------
 400  CONTINUE
C
      CALL BUS012(KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KSEC2,KERR)
      IF(KERR.NE.0) THEN
         PRINT*,'ERROR IN BUS012: ',KERR
         PRINT*,' BUFR MESSAGE NUMBER ',N,' CORRUPTED.'
         KERR=0
         GO TO 300
      END IF
C
 440  CONTINUE
C
C              AT ECMWF SECTION 2 CONTAINS RDB KEY.
C              SO UNPACK KEY
C
      IF(KSEC1(9).NE.96.OR.KSEC1(10).NE.4.OR.
     1   KSEC1(11).NE.8) THEN
         PRINT*,'----------------------------------',N

      CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)
C
C              PRINT KEY
C
      CALL BUPRS2(KSUP ,KEY)
      END IF
C
      GO TO 300
C     -----------------------------------------------------------------
C
 900  CONTINUE
C
      CALL PBCLOSE(IUNIT,IRET)
      CALL PBCLOSE(IUNIT1,IRET)
C
      END
