      PROGRAM SPLITBUFR
C
C**** *SPLITBUFR*
C
C
C     PURPOSE.
C     --------
C           SPLITS FILE CONTAINING BUFR DATA INTO FILES
C           ACCORDING TO BUFR SUBTYPES. THE CREATED FILE 
C           NAMES ARE "ST" FOLLOWED BY BUFR SUBTYPE NUMBER.
C           (st001 FOR SYNOP LAND)
C
C
C**   INTERFACE.
C     ----------
C
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
C         CALL BUFREX
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/03/2009
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
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2=4096 ,JSEC3=  4,
     1      JSEC4=  2,JELEM=160000,JSUBS=400,JCVAL=150,JBUFL=512000,
#ifdef JBPW_64
     2          JBPW =  64,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200,
#else
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200,
#endif
     3          JWORK=4096000,JKEY=46,JBYTE=1000000)
C
      PARAMETER (JTYPE=255)
C
      PARAMETER (KELEM=160000)
      PARAMETER (KVALS=4096000)
C 
      DIMENSION KBUFF(JBUFL),KTDLST(JELEM),KTDEXP(JELEM)
      DIMENSION KSUP(JSUP)  ,KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION KSEC2(JSEC2),KSEC3(JSEC3),KSEC4(JSEC4)
      DIMENSION KEY  (JKEY)
C
      REAL*8 VALUES(KVALS)
      REAL*8 RVIND,EPS
      DIMENSION IUNIT1(JTYPE)
C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
C 
      CHARACTER*80 YENC
      CHARACTER*256 CINFILE,CARG(5)
      CHARACTER*5 CFNAME
C
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
      RVIND=1.7E38
      NBYTES=JBPW/8
C
C     INPUT FILE NAME
C
C
C              GET ARGUMENTS
C
C              1)  INPUT FILE NAME
C
      NARG=IARGC()
      IF(NARG.LT.2) THEN
         PRINT*,'USAGE -- bufr_split -i infiles '
         STOP
      END IF
      NFILE=NARG
C
      DO 104 J=1,NARG
      CALL GETARG(J,CARG(J))
 104  CONTINUE

      II=0
      IO=0
      DO 105 J=1,NARG
      IF(CARG(J).EQ.'-i') THEN
         IN=J
      END IF
 105  CONTINUE
      IF(IN.EQ.0) THEN
          PRINT*,'USAGE -- bufr_split -i infiles '
          STOP
      END IF
C
      IST=IN+1
      IEND=NARG
C
      CFNAME(1:2)='st'
      DO 102 I=1,JTYPE
      WRITE(CFNAME(3:5),'(I3.3)') I
      CALL PBOPEN(IUNITC,CFNAME,'W',IRET)
      IUNIT1(I)=IUNITC
 102  CONTINUE

C
      NR=0
C
C*          1.2 OPEN FILE CONTAINING BUFR DATA.
C               -------------------------------
 120  CONTINUE
C
      DO 121 IZ=IST,IEND
C
      CINFILE=' '
      CINFILE=CARG(IZ)
      IZZ=INDEX(CINFILE,' ')
      IZZ=IZZ-1
C
      IRET=0 
      CALL PBOPEN(IUNIT,CINFILE(1:IZZ),'R',IRET)
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
      CALL PBBUFR(IUNIT,KBUFF,JBYTE,KBUF,IRET) 
      IF(IRET.EQ.-1) THEN
         CALL PBCLOSE(IUNIT,IERR)
         GO TO 121
      END IF
      IF(IRET.EQ.-2) STOP 'FILE HANDLING PROBLEM' 
      IF(IRET.EQ.-3) STOP 'ARRAY TOO SMALL FOR PRODUCT'
C
      N=N+1
      KBUFL=KBUF/NBYTES+1
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
         CALL EXIT(2)
      END IF
C
      KSEC17=KSEC1(7)
C
      IF(KSEC1(7).GE.1.AND.KSEC1(7).LE.4) THEN
         KEL=KELEM
         IF(KSUP(6).GT.1) KEL=JWORK/KSUP(6)
C
         CALL BUFREX(KBUFL,KBUFF,KSUP,KSEC0 ,KSEC1,KSEC2 ,
     1              KSEC3 ,KSEC4,
     2              KEL,CNAMES,CUNITS,KVALS,VALUES,CVALS,IERR)
C
         IF(IERR.NE.0) THEN
            IF(IERR.EQ.45) GO TO 300
C            CALL EXIT(2)
         END IF
C
         CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
         IF(KERR.NE.0) CALL EXIT(2)
C
         KSEC17=KSEC1(7)
         IF(KTDLST(1).EQ.307007) THEN
            KSEC17=5
            DO  I=1,KTDLEN
            IF(KTDLST(I).EQ.013022) KSEC17=6
            IF(KTDLST(I).EQ.013021) KSEC17=7
            IF(KTDLST(I).EQ.013020) KSEC17=8
            END DO

         ELSE
            DO  I=1,KTDLEN
            IF(KTDLST(I).EQ.013022) KSEC17=16
            IF(KTDLST(I).EQ.013021) KSEC17=17
            IF(KTDLST(I).EQ.013020) KSEC17=18
            END DO
         END IF
      END IF
C
      IUN=-99
      DO I=1,255
        IF(KSEC17.EQ.I) THEN
           IUN=IUNIT1(I) 
           GO TO 500
        END IF
      END DO
C
 500  CONTINUE
      IF(IUN.lt.0) THEN
         PRINT*,'SUBTYPE ',KSEC17,'FOUND.'
         print*,'unit number of iunit1(1)=',iunit1(1)
         GO TO 300
      END IF
C
C           6.3 WRITE PACKED BUFR MESSAGE INTO FILE.
C               ------------------------------------
 630  CONTINUE
C
      IERR=0
      CALL PBWRITE(IUN,KBUFF,KBUF,IERR)
      IF(IRET.EQ.-1) THEN
         PRINT*,'ERROR DURING WRITE ON OUTPUT FILE'
         CALL EXIT(2)
      END IF
C
      GO TO 300
C
 121  CONTINUE
C     -----------------------------------------------------------------
C
 900  CONTINUE
C
      PRINT*,'NUMBER OF MESSAGES PROCESSED ',N
C
      END
