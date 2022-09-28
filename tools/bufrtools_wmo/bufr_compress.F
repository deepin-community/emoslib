      PROGRAM BUCOMPRESS
C
C**** *BUFR*
C
C
C     PURPOSE.
C     --------
C     
C           PACKS BUFR MULTI SUBSET DATA IN THE COMPRESSED FORM
C           THE NUMBER OF SUBSETS TO BE COMPRESSED IS DEFINED IN 
C           THE NAMLIST FILE.
C
C
C**   INTERFACE.
C     ----------
C
C          BUCOMPRESS OUTFILE NAMELIST_FILENAME
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
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200,
     3          JWORK=4096000,JKEY=46,JBYTE=512000)
C
      PARAMETER (JTYPE=256)
      PARAMETER (KELEM=20000)
      PARAMETER (KVALS=80000)
C 
      DIMENSION KBUFF(JBUFL)
      DIMENSION KBUFR(JBUFL)
      DIMENSION KSUP(JSUP)  ,KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION KSEC2(JSEC2),KSEC3(JSEC3),KSEC4(JSEC4)
      DIMENSION KEY  (JKEY),KREQ(2)
C
      REAL*8 VALUES(KVALS),VALUE(KVALS),VALS(KVALS)
      REAL*8 RVIND,EPS
C
      DIMENSION KTDLST(KELEM),KTDEXP(KELEM)
      DIMENSION KDATA(200)
      DIMENSION ICOMP(JTYPE)
C
      CHARACTER*256 CARG(4),COUT,CFIN
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
      CHARACTER*80 CVAL(KVALS)
      CHARACTER*80 YENC
C
      CHARACTER*5 CFNAME(JTYPE)
C
C      DATA CFNAME/'st001','st002','st003','st004','st005',
C     1            'st009','st011','st012','st013','st014',
C     2            'st019','st021','st022','st023','st051',
C     2            'st061','st062','st063','st071','st072',
C     3            'st073','st082','st083','st084','st085',
C     4            'st091','st092','st095','st101','st102',
C     5            'st103','st104','st105','st106','st121',
C     6            'st122','st123','st124','st125','st127',
C     7            'st128','st131','st132','st133','st142',
C     8            'st144','st145','st164'/
C
C      DATA ICOMP/200,200,200,200,200,
C     1           200,200,200,200,200,
C     2           200,200,200,200,052,
C     2           052,052,052,052,052,
C     3           052,050,050,050,050,
C     4           001,001,001,001,001,
C     5           001,001,001,001,001,
C     5           361,001,001,001,001,
C     7            -1,001,200,200,200,
C     8           100/
C
       DATA CFNAME/JTYPE*' '/
      NAMELIST /COMPRESS/ CFNAME,ICOMP
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
      NVIND=2147483647
      NREAD=0
      NSUBS=0
      NW=0
      ITOTAL=0
      NBYTES=JBPW/8
C
C
C     GET INPUT AND OUTPUT FILE NAME.
C
      NARG=IARGC()
C
      IF(NARG.LT.4) THEN
         PRINT*,'USAGE -- bufr_compress -i namelist -o outfile'
         STOP
      END IF
C
      DO 101 J=1,NARG
      CALL GETARG(J,CARG(J))
 101  CONTINUE
C
      DO 102 J=1,NARG,2
      IF(CARG(J).EQ.'-i') THEN
         CFIN=CARG(J+1)
      ELSEIF(CARG(J).EQ.'-o') THEN
         COUT=CARG(J+1)
      ELSE
          PRINT*,'USAGE -- bufr_compress -i namelist -o outfile'
          STOP
      END IF
102   CONTINUE
C
      KRQL=0
      NR=0
      KREQ(1)=0
      KREQ(2)=0
C
C*          1.2 OPEN NAMELIST AND OUTPUT FILE
C               -----------------------------
 120  CONTINUE
C
C               OPEN NAMELIST
C
      OPEN(UNIT=60,FILE=CFIN,ERR=125,IOSTAT=IOS,
     1                 STATUS='OLD',
     2                 FORM='FORMATTED')
C
      GO TO 126
C
 125  CONTINUE
C
      PRINT*,'OPEN ERRO ON NAMELIST FILE.'
      CALL MY_EXIT(2)
C
 126  CONTINUE
C
      READ(60,COMPRESS)
      CLOSE(60)
C
C     CHECK HOW MANY FILES IN THE NAMELIST
C
      NFILES=256
C     DO 127 I=1,JTYPE
C     IF(CFNAME(I).EQ.' ') THEN
C        NFILES=I-1
C        GO TO 128
C     END IF
C127  CONTINUE
C
 128  CONTINUE
C
C               OPEN OUTPUT FILE
C
      PRINT*,'NUMBER OF FILES TO PROCESS=',NFILES
      II=INDEX(COUT,' ')
      II=II-1
      CALL PBOPEN(IUNIT1,COUT(1:II),'A',IRET)
      IF(IRET.EQ.-1) STOP 'OPEN FAILED ON OUTPUT FILE'
      IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'
C
C               OPEN INPUT FILES
C
      DO 121 IO=1,NFILES
C
      IRET=0 
      CALL PBOPEN(IUNIT,CFNAME(IO),'R',IRET)
      IF(IRET.EQ.-1) THEN
        GO TO 121
      END IF
      IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'
C
C
C     ----------------------------------------------------------------- 
C*          2. SET REQUEST FOR EXPANSION.
C              --------------------------
 200  CONTINUE
C
      ICODE=0
      NCOM=ICOMP(IO)
      OCOMP=.TRUE.
C
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
         IF(N.NE.0) GO TO 600
         GO TO 122
      END IF
      IF(IRET.EQ.-2) STOP 'FILE HANDLING PROBLEM' 
      IF(IRET.EQ.-3) STOP 'ARRAY TOO SMALL FOR PRODUCT'
C
      NREAD=NREAD+1
      KBUFL=KBUFL/NBYTES+1
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
      KEL=KELEM
      IF(KSUP(6).GT.1) KEL=JWORK/KSUP(6)
C
      CALL BUFREX(KBUFL,KBUFF,KSUP,KSEC0 ,KSEC1,KSEC2 ,KSEC3 ,KSEC4,
     1            KEL,CNAMES,CUNITS,KVALS,VALUES,CVALS,IERR)
C
      IF(IERR.NE.0) THEN
         IF(IERR.EQ.2) THEN
            IERR=0
         ELSE
            CALL MY_EXIT(2)
         END IF
      END IF
C
      NSUBS=NSUBS+KSEC3(3)
C
C     -----------------------------------------------------------------
C*          5. COLLECT DATA FOR REPACKING.
C              ---------------------------
 500  CONTINUE
C      
C               FIRST GET DATA DESCRIPTORS
C
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
      IF(KERR.NE.0) CALL MY_EXIT(2)
C
C     CHECK IF ANOUGH SPACE FOR COMPRESSION
C
C
C     CHECK FOR MAXIMUM NUMBER OF SUBSETS TO BE PACKED
C
      IF(NCOM.LT.0) THEN
         NCOM=JWORK/KTDEXL
         WRITE(*,'(I4,A,I3)')
     1   NCOM,' SUB-SETS COMPRESSED FOR SUB-TYPE ',KSEC1(7)
      ELSE
         IF(NCOM*KTDEXL.GT.JWORK) THEN
         PRINT*,'TOO MANY SUBSETS TO BE PACKED.'
         PRINT*,'MAXIMUM NUMBER OF SUBSETS TO BE PACKED =',JWORK/KTDEXL
         CALL MY_EXIT(2)
         END IF
      END IF
C
      DO 503 J=1,KSUP(6)
      N=N+1
C
      DO 502 I=1,KSUP(5)
      IV=I+(J-1)*KEL
      IN=I+(N-1)*KSUP(5)
C
      IF(CUNITS(I).EQ.'CCITTIA5') THEN
         IPOS =VALUES(IV)/1000.
         ICHAR=NINT(VALUES(IV)-IPOS*1000)
         KKK=KKK+1
         VALUE(IN)=KKK*1000+ICHAR
         CVAL(KKK)=CVALS(IPOS)
      ELSE
         VALUE(IN)=VALUES(IV)
      END IF
 502  CONTINUE
      IF(N.EQ.NCOM) THEN
        JLAST=J
        GO TO 600
      END IF
 503  CONTINUE
C
      IF(N.EQ.NCOM)    GO TO 600
C
      GO TO 300
C     -----------------------------------------------------------------
C*          6. PACK BUFR MESSAGE BACK INTO BUFR.
C              ---------------------------------
 600  CONTINUE
C
    
      KKK=0
      KSEC3(3)=N
      KSEC1(8)=1
      KSEC3(4)=128                    ! NO COMPRESSION
      IF(KSEC3(3).GT.1) KSEC3(4)=192  ! COMPRESSION
      N=0
      KBUFL=JBUFL
C
C     GET REPLICATION FACTORS
C
      KK=0
      DO 601 K=1,KSUP(5)
      IF(KTDEXP(K).EQ.31001.OR.KTDEXP(K).EQ.31002.OR.
     1   KTDEXP(K).EQ.31011.OR.KTDEXP(K).EQ.31012) THEN
         KK=KK+1
         KDATA(KK)=NINT(VALUES(K))
      END IF
 601  CONTINUE
C
      KDLEN=2
      IF(KK.NE.0) KDLEN=KK
C
C*          6.2 ENCODE DATA INTO BUFR MESSAGE.
C               ------------------------------
 620  CONTINUE
C
C       CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)
C
C      PACK RDB KEY
C
C
       KLM=KSUP(5)
C
       CALL SETLALO(KSEC1(7),KLA,KLO,KERR)
       IF(KERR.NE.0) CALL MY_EXIT(2)
C
       CALL FMMH( KLA,KLO,KLM,VALUE,KSEC3,RMINLAT,RMINLON,
     1           RMAXLAT,RMAXLON)
C       CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)
C       IF(KERR.NE.0) THEN
C          CALL MY_EXIT(2)
C       END IF
C
       MINLAT=NINT(RMINLAT*100000)+9000000
       MAXLAT=NINT(RMAXLAT*100000)+9000000
       MINLON=NINT(RMINLON*100000)+18000000
       MAXLON=NINT(RMAXLON*100000)+18000000
C
       KEY(10)=MINLON
       KEY(11)=MINLAT
       KEY(12)=MAXLON
       KEY(13)=MAXLAT
       KEY(14)=KSEC3(3)
C
C       CALL BUPKEY(KEY,KSEC1,KSEC2,KERR)
C       IF(KERR.NE.0) THEN
C          PRINT*,'FILE IN PROCESS IS ',CFNAME(IO)
C          CALL MY_EXIT(2)
C       END IF
C
       CALL BUFREN( KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
     1              KTDLEN,KTDLST,KDLEN,KDATA,KSUP(5),
     2              KVALS,VALUE,CVAL,KBUFL,KBUFR,KERR)
C
      IF(KERR.NE.0) THEN
         PRINT*,'ERROR DURING ENCODING.'
         CALL MY_EXIT(2)
      END IF
C
C           6.3 WRITE PACKED BUFR MESSAGE INTO FILE.
C               ------------------------------------
 630  CONTINUE
C
      ILEN=KBUFL*NBYTES
C     
      IERR=0
      CALL PBWRITE(IUNIT1,KBUFR,ILEN,IERR)
      IF(IERR.LT.0)  THEN
         PRINT*,'PBWRITE: WRITE ERROR ',IERR
         CALL MY_EXIT(2)
      END IF
C
      ITOTAL=ITOTAL+KSEC3(3)
C
      IF(IRET.EQ.-1) THEN
         NW=NW+1
         GO TO 122
      END IF
C
      NW=NW+1
C
      N=0
      IF(JLAST.LT.KSUP(6)) THEN
C
C        MOVE THE REST OF DATA INTO VALUE ARRAY
C
         DO 505 J=JLAST+1,KSUP(6)
         N=N+1
         DO 504 I=1,KSUP(5)
         IV=I+(J-1)*KEL
         IN=I+(N-1)*KSUP(5)
C
         IF(CUNITS(I).EQ.'CCITTIA5') THEN
            IPOS =VALUES(I)/1000.
            ICHAR=NINT(VALUES(IV)-IPOS*1000)
            KKK=KKK+1
            VALUE(IN)=KKK*1000+ICHAR
            CVAL(KKK)=CVALS(IPOS)
         ELSE
            VALUE(IN)=VALUES(IV)
         END IF
 504     CONTINUE
 505     CONTINUE
      END IF
C
      IF(NCOM.LE.KSUP(6)-JLAST) THEN
         KSUP(6)=KSUP(6)-JLAST
         N=0
        GO TO 500
      END IF
C
      GO TO 300
C
 122  CONTINUE
C
      IRET=0
      CALL PBCLOSE(IUNIT,IRET)
C
 121  CONTINUE
C
C     -----------------------------------------------------------------
 900  CONTINUE
C
       PRINT*,'NUMBER OF MESSAGES READ        ',NREAD
       PRINT*,'NUMBER OF MESSAGES WRITTEN     ',NW
       PRINT*,'NUMBER OF SUBSETS              ',NSUBS
       PRINT*,'NUMBER OF OBSERVATIONS WRITTEN ',ITOTAL
C
      CALL PBCLOSE(IUNIT1,IRET)
C
      END
