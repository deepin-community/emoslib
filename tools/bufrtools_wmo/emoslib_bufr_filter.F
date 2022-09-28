      PROGRAM GETBUFR
C
C**** *GETBUFR*
C
C
C     PURPOSE.
C     --------
C         FILTER BUFR MESSAGES FROM INPUT FILE
C         ACCORDING TO REQUEST.
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
C          THE INPUT FILE IS READ ONE MESSAGE AT A TIME.
C          BUFR MESSAGE IS EXPANDED AND CHECKED AGAINST
C          REQUEST. IF BUFR MESSAGE IS NEEDED IT IS WRITTEN
C          INTO THE OUTPUT FILE. IF THERE IS MORE THAN ONE
C          SUBSET IN THE BUFR MESSAGE, ONLY REQUESTED ARE REPACKED
C          AND WRITTEN INTO THE OUTPUT FILE.
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
C          M. DRAGOSAVAC    *ECMWF*       06/12/93.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 4096,JSEC3=  4,
#ifdef JBPW_64
     1      JSEC4=   2,JBUFL=512000,JBPW =  64,JKEY=46, JELEM=160000,
#else
     1      JSEC4=   2,JBUFL=512000,JBPW =  32,JKEY=46, JELEM=160000,
#endif
     2      JWORK=4096000)
C
      PARAMETER (KELEM=160000)
      PARAMETER (KVALS=4096000)
C
C
      COMMON /COMRQB/ CTIME,CAREA,CBLOCK,CIDN,CTYPE,CSBTYPE,CRDBT,
     1                CMDBT,CQC,CIDENTS(99),CMETHOD,CSENSOR,CPRODUCT,
     2                CINSTR
C
      COMMON /COMIRQC/ ITR,IDATES(2),ITIMES(2),IAR,FLATS(4),
     1                 IBL,IBLOCK(9999),ITP,ITYPES(9999),
     2                 ISBT,ISBTYPES(9999),IRDBT,IRDBTS(9999),
     3                 IMDBT,IMDBTS(9999),IQC,IQCS(9999),IDN,
     4                 IMT,IMETHOD(9999),ISE,ISENSOR(9999),
     5                 IPR,IPRODUCT(9999),INS,INSTR(9999)
C
      DIMENSION KBUFF(JBUFL)
      DIMENSION KBUFR(JBUFL)
      DIMENSION KSUP(JSUP)  ,KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION KSEC2(JSEC2),KSEC3(JSEC3),KSEC4(JSEC4)
      DIMENSION KEY(JKEY),KEY1(JKEY),KEY2(JKEY)
      DIMENSION KSEC11(JSEC1), KSEC12(JSEC1),KSEC31(JSEC3),KSEC32(JSEC3)
C
      REAL*8  VALUES(KVALS),VALUES1(KVALS),VALUES2(KVALS)
      REAL*8 RVIND
C
      DIMENSION KTDLST(KELEM),KTDEXP(KELEM)
      DIMENSION KDATA(4000)
C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
      CHARACTER*9  CIDENTS
      CHARACTER*16000 CRECORD
      CHARACTER*256 CARG(8)
C
      CHARACTER*255 CRQFILE,CINFILE,COUTFILE,CREMFILE
      CHARACTER*4   CTIME,CAREA,CBLOCK,CIDN,CTYPE,CSBTYPE
      CHARACTER*4   CRDBT,CMDBT,CQC,CKIND,CMETHOD,CSENSOR,CPRODUCT
      CHARACTER*4   CINSTR
C
cs      EXTERNAL GETARG
C
C
C     ------------------------------------------------------------------
C*          1. GET FILE NAMES.
C              ---------------
 100  CONTINUE
C
      IREAD=0
      ISING=0
      IMULTI=0
      IFILTER=0
      ITOTAL=0
      irem=0
C
      NBYTES=JBPW/8
      RVIND=1.7E38
      JBYTE=JBUFL*NBYTES
      ITR=0
      IAR=0
      IBL=0
      ITP=0
      ISBT=0
      IRDBT=0
      IMDBT=0
      IQC=0
      IDN=0
      IPR=0
C
      CRQFILE=' '
      CINFILE=' '
      COUTFILE=' '
      CREMFILE=' '
      ocremfile=.false.
C
C              GET ARGUMENTS
C
C              1)  REQUEST FILE NAME
C              2)  INPUT FILE NAME
C              3)  OUTPUT FILE NAME
C
      NARG=IARGC()
      IF(NARG.lt.6) THEN
         PRINT*,
     X'USAGE -- emoslib_bufr_filter -r request -i infile -o outfile'
         PRINT*,
     X'                             -t remfile'
         STOP
      END IF
      DO 101 J=1,NARG
      CARG(J)=' '
      CALL GETARG(J,CARG(J))
 101  CONTINUE
C
      DO 102 J=1,NARG,2
      IF(CARG(J).EQ.'-i') THEN
         CINFILE=CARG(J+1)
         icin=index(CINFILE,' ')
         icin=icin-1
      ELSEIF(CARG(J).EQ.'-o') THEN
         COUTFILE=CARG(J+1)
         iout=index(COUTFILE,' ')
         iout=iout-1
      ELSEIF(CARG(J).EQ.'-r') THEN
         CRQFILE=CARG(J+1)
         icrq=index(CRQFILE,' ')
         icrq=icrq-1
      ELSEIF(CARG(J).EQ.'-t') THEN
         CREMFILE=CARG(J+1)
         icrem=index(CREMFILE,' ')
         icrem=icrem-1
         ocremfile=.true.
      ELSE
         PRINT*,
     X'USAGE -- emoslib_bufr_filter -r request -i infile -o outfile'
         PRINT*,
     X'                             -t remfile'
         STOP
      END IF
 102  CONTINUE
C
C
C*          1.1 OPEN IN REQUEST
C               ---------------
 110  CONTINUE
C
      OPEN(47,FILE=CRQFILE(1:icrq),IOSTAT=IOS,ERR=111,
     1        STATUS='OLD')
C
      CALL PBOPEN(IUN,COUTFILE(1:iout),'W',IRET)
      IF(IRET.NE.0) THEN
         PRINT*,'OPEN ERROR ',IOS,' ON FILE ',COUTFILE
         CALL EXIT(2)
      END IF
C
      GO TO 120
C
 111  CONTINUE
C
      PRINT*,'OPEN ERROR ',IOS,' ON FILE ',CRQFILE
      CALL EXIT(2)
C
C
C*          1.2 READ IN REQUEST
C               ---------------
 120  CONTINUE
C
C               TIME RANGE
C
      READ(47,'(A)',IOSTAT=IOS,ERR=121,END=130) CRECORD
      CKIND=CRECORD(1:4)
C
      IF(CKIND.EQ.'TIME') THEN
         READ(CRECORD,'(A4,1X,I4,2(1X,I8,I6))',
     1         IOSTAT=IOS,ERR=121)
     1   CTIME,ITR,(IDATES(I),ITIMES(I),I=1,ITR)
         PRINT*,CTIME,ITR,(IDATES(I),ITIMES(I),I=1,ITR)
         GO TO 120
      END IF
C
C               AREA
C
      IF(CKIND.EQ.'AREA') THEN
        READ(CRECORD,'(A4,1X,I4,4(3X,F12.5))',
     1         IOSTAT=IOS,ERR=121)
     1  CAREA,IAR,(FLATS(I),I=1,IAR)
        PRINT*,CAREA,IAR,(FLATS(I),I=1,IAR)
        GO TO 120
      END IF
C
C               BLOCK
C
      IF(CKIND.EQ.'BLOK') THEN
        READ(CRECORD,'(A4,1X,I4,30(13X,I2))',
     1         IOSTAT=IOS,ERR=121)
     1  CBLOCK,IBL,(IBLOCK(I),I=1,IBL)
        PRINT*,CBLOCK,IBL,(IBLOCK(I),I=1,IBL)
        GO TO 120
      END IF
C
C
C               METHOD
C
      IF(CKIND.EQ.'METH') THEN
        READ(CRECORD,'(A4,1X,I4,30(13X,I2))',
     1         IOSTAT=IOS,ERR=121)
     1  CMETHOD,IMT,(IMETHOD(I),I=1,IMT)
        PRINT*,CMETHOD,IMT,(IMETHOD(I),I=1,IMT)
        GO TO 120
      END IF
C
C               SENSOR

      IF(CKIND.EQ.'SENS') THEN
        READ(CRECORD,'(A4,1X,I4,30(12X,I3))',
     1         IOSTAT=IOS,ERR=121)
     1  CSENSOR,ISE,(ISENSOR(I),I=1,ISE)
c        READ(CRECORD,*) (ISENSOR(I),I=1,ISE)
        PRINT*,CSENSOR,ISE,(ISENSOR(I),I=1,ISE)
        GO TO 120
      END IF

C               INSTRUMENT

      IF(CKIND.EQ.'INST') THEN
        READ(CRECORD,'(A4,1X,I4,30(12X,I3))',
     1         IOSTAT=IOS,ERR=121)
     1  CINSTR,INS,(INSTR(I),I=1,INS)
c        READ(CRECORD,*) (INSTR(I),I=1,INS)
        PRINT*,CINSTR,INS,(INSTR(I),I=1,INS)
        GO TO 120
      END IF

C
C               PRODUCT TYPE
C
      IF(CKIND.EQ.'PRTP') THEN
        READ(CRECORD,'(A4,1X,I4,30(13X,I2))',
     1         IOSTAT=IOS,ERR=121)
     1  CPRODUCT,IPR,(IPRODUCT(I),I=1,IPR)
c        READ(CRECORD,*) (IPRODUCT(I),I=1,IPR)
        PRINT*,CPRODUCT,IPR,(IPRODUCT(I),I=1,IPR)
        GO TO 120
      END IF

C
C               IDENT
C
      IF(CKIND.EQ.'IDEN') THEN
        READ(CRECORD,'(A4,1X,I4,30(1X,A9,5X))',
     1         IOSTAT=IOS,ERR=121)
     1  CIDN,IDN,(CIDENTS(I),I=1,IDN)
        PRINT*,CIDN,IDN,(CIDENTS(I),I=1,IDN)
        GO TO 120
      END IF
C
C               TYPES
C
      IF(CKIND.EQ.'TYPE') THEN
        READ(CRECORD,'(A4,1X,I4,30(12X,I3))',
     1         IOSTAT=IOS,ERR=121)
     1  CTYPE,ITP,(ITYPES(I),I=1,ITP)
        PRINT*,CTYPE,ITP,(ITYPES(I),I=1,ITP)
        GO TO 120
      END IF
C
C               SUBTYPES
C
      IF(CKIND.EQ.'STYP') THEN
        READ(CRECORD,'(A4,1X,I4,30(12X,I3))',
     1         IOSTAT=IOS,ERR=121)
     1  CSBTYPE,ISBT,(ISBTYPES(I),I=1,ISBT)
        write(*,'(A4,1X,I4,30(12X,I3))') CSBTYPE,ISBT,
     1                          (ISBTYPES(I),I=1,ISBT)
        GO TO 120
      END IF
C
C               RDB TIME
C
      IF(CKIND.EQ.'RDBT') THEN
        READ(CRECORD,'(A4,1X,I4,30(7X,I8))',
     1         IOSTAT=IOS,ERR=121)
     1  CRDBT,IRDBT,(IRDBTS(I),I=1,IRDBT)
        PRINT*,CRDBT,IRDBT,(IRDBTS(I),I=1,IRDBT)
        GO TO 120
      END IF
C
C               MDB TIME
C
      IF(CKIND.EQ.'MDBT') THEN
        READ(CRECORD,'(A4,1X,I4,30(7X,I8))',
     1         IOSTAT=IOS,ERR=121)
     1  CMDBT,IMDBT,(IMDBTS(I),I=1,IMDBT)
        PRINT*,CMDBT,IMDBT,(IMDBTS(I),I=1,IMDBT)
        GO TO 120
      END IF
C
C               LOWEST QUALITY CONTROL
C
      IF(CKIND.EQ.'LQCT') THEN
        READ(CRECORD,'(A4,1X,I4,2(12X,I3))',
     1         IOSTAT=IOS,ERR=121)
     1  CQC,IQC,(IQCS(I),I=1,IQC)
        PRINT*,CQC,IQC,(IQCS(I),I=1,IQC)
        GO TO 120
      END IF
C
C      GO TO 130
C
 121  CONTINUE
C
      PRINT*,'READ ERROR ',IOS,' ON ',CRQFILE(1:I),' FILE.'
      CALL EXIT(2)
C---------------------------------------------------------------------
C*          1.3 OPEN FILE CONTAINING BUFR DATA.
C               -------------------------------
 130  CONTINUE
C
      IRET=0
C
      CALL PBOPEN(IUNIT,CINFILE(1:icin),'R',IRET)
      IF (IRET.EQ.-1 .OR. IRET .EQ. -2 .OR. IRET.EQ.-3) THEN
        IF (IRET.EQ.-1) PRINT*,'OPEN FAILED'
        IF (IRET.EQ.-2) PRINT*,'INVALID FILE NAME'
        IF (IRET.EQ.-3) PRINT*,'INVALID OPEN MODE SPECIFIED'
        CALL EXIT(2)
      END IF
c
      IF (CREMFILE.NE.' ') THEN
        CALL PBOPEN(IUNIT2,CREMFILE(1:icrem),'W',IRET)
        IF (IRET.EQ.-1 .OR. IRET .EQ. -2 .OR. IRET.EQ.-3) THEN
          IF (IRET.EQ.-1) PRINT*,'OPEN FAILED'
          IF (IRET.EQ.-2) PRINT*,'INVALID FILE NAME'
          IF (IRET.EQ.-3) PRINT*,'INVALID OPEN MODE SPECIFIED'
          CALL EXIT(2)
        END IF
      END IF

C
C     -----------------------------------------------------------------
C*          2.  READ BUFR MESSAGE.
C               ------------------
 200  CONTINUE
C
      IERR=0
      KBUFL=0
c     DO 201 I=1,JBUFL
c     KBUFF(I)=0
c201  CONTINUE
C
      IRET=0
      CALL PBBUFR(IUNIT,KBUFF,JBYTE,KBUFL,IRET)
      IF(IRET.EQ.-1) THEN
         PRINT*,'NUMBER OF MESSAGES READ          ',IREAD
         PRINT*,'NUMBER OF OBSERVATIONS READ      ',ITOTAL
         PRINT*,'NUMBER OF SINGLE SUBSET MESSAGES ',ISING
         PRINT*,'NUMBER OF MULTISUBSET MESSAGES   ',IMULTI
         PRINT*,'NUMBER OF FILTERED OBSERVATIONS  ',IFILTER
         PRINT*,'EOF'
         STOP
      ELSEIF (IRET.EQ.-2) THEN
        PRINT*,'FILE HANDLING PROBLEM'
        CALL EXIT(2)
      ELSEIF (IRET.EQ.-3) THEN
        PRINT*,'ARRAY TOO SMALL FOR PRODUCT'
        CALL EXIT(2)
      END IF
C
      IREAD=IREAD+1
      JLEN=KBUFL
      KBUFL=KBUFL/NBYTES
C
C
C     -----------------------------------------------------------------
C*          2.1 EXPAND BUFR MESSAGE SECTION 0,1,2
C               ---------------------------------
 210  CONTINUE
C
      IERR=0
      CALL BUS012(KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KSEC2,IERR)
      IF(IERR.NE.0) GO TO 200
C
      ITOTAL=ITOTAL+KSUP(6)
C
C            2.1.1  UNPACK KEY
C
      CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)
C
C              PRINT KEY
C
C      CALL BUPRS2(KSUP ,KEY)
C
C*           2.2 CHECK IF MESSAGE CONTAIN SUBSETS
C                --------------------------------
 220  CONTINUE
C
c     IF(KSUP(6).EQ.1.AND.KSEC1(7).NE.87.and.
c    1   KSEC1(7).NE.82.and.KSEC1(7).NE.83.and.
c    2   KSEC1(7).NE.206.AND.KSEC1(7).NE.55.and.
c    3   KSEC1(7).NE.54.AND.KSEC1(7).NE.250) THEN
C
C        CHECK IF MESSAGE IS REQUESTED
C
c        OREQ=.FALSE.
c        CALL KEYCHK(KSEC1,KEY,OREQ)
C
C        WRITE MESSAGE ON OUTPUT
C
c        IF(OREQ)  THEN
c          CALL PBWRITE(IUN,KBUFF,JLEN,IRET)
c          IF(IRET.EQ.-1) THEN
c             WRITE(*,'(A,A)') 'WRITE ERROR ON ',COUTFILE(1:K)
c             CALL EXIT(2)
c          END IF
c          IFILTER=IFILTER+1
c          ISING=ISING+1
C
c        ELSE
c          IF(OCREMFILE) THEN
c          CALL PBWRITE(IUNIT2,KBUFF,JLEN,IRET)
c          IF(IRET.EQ.-1) THEN
c             WRITE(*,'(A,A)') 'WRITE ERROR ON ',CREMFILE(1:K)
c             CALL EXIT(2)
c          END IF
c          IREM=IREM+1
c          END IF
c        END IF
C
c        GO TO 200
C
c     END IF
C----------------------------------------------------------------
C
C            2.3 MULTI-SUBSET MESSAGE
C
 230  CONTINUE
C
      IF(KSUP(6).GT.1)  THEN
         KEL=KVALS/KSUP(6)
         IF(KEL.GT.JELEM) KEL=JELEM
      ELSE
         KEL=KELEM
      END IF
C
      CALL BUFREX(KBUFL,KBUFF,KSUP,KSEC0 ,KSEC1,KSEC2 ,KSEC3 ,KSEC4,
     1            KEL,CNAMES,CUNITS,KVALS,VALUES,CVALS,IERR)
      IF(IERR.NE.0) THEN
         PRINT*,'ERROR AFTER READING NUMBER OF MESSAGES ',IREAD
         CALL EXIT(2)
      END IF
C
C*          2.4 PRINT EXPANDED BUFR MESSAGE.
C               ----------------------------
 240  CONTINUE
C
C      CALL BUPRS0(KSEC0)
C      CALL BUPRS1(KSEC1)
C      CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)
C      CALL BUPRS2(KSUP ,KEY)
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
      IF(KERR.NE.0) THEN
        PRINT*,'ERROR AFTER READING NUMBER OF MESSAGES ',IREAD
        CALL EXIT(2)
      ENDIF
C      CALL BUPRS3(KSEC3,KTDLEN,KTDLST,KTDEXL,KTDEXP,KEL,CNAMES)
C      IST=1
C      IEND=KSUP(6)
C      CALL BUPRT(ICODE,IST,IEND,KEL,CNAMES,CUNITS,CVALS,
C     1           KVALS,VALUES,KSUP,KSEC1,IERR)
C
C
C            2.5 COLLECT REQUESTED MESSAGES.
C
 250  CONTINUE
C
      IERR=0
C
      OREQ=.FALSE.
      CALL MESSCHCK(OREQ,KEY,KTDEXL,KTDEXP,
     1              KSUP,KSEC0 ,KSEC1,KSEC2 ,KSEC3 ,KSEC4,
     1              KEL,CNAMES,CUNITS,KVALS,VALUES,CVALS,
     3                    ksec11,ksec12,ksec31,ksec32,
     4                    key1,key2,values1,values2)
C
C     IF(.NOT.OREQ.AND.OCREMFILE) THEN
      IF(ksec31(3).eq.0.AND.OCREMFILE) THEN
C
           CALL PBWRITE(IUNIT2,KBUFF,JLEN,IRET)
           IF(IRET.EQ.-1) THEN
              WRITE(*,'(A,A)') 'WRITE ERROR ON ',CREMFILE(1:K)
              CALL EXIT(2)
           END IF
           IREM=IREM+1
         GO TO 200
C
      END IF
C     -----------------------------------------------------------------
C*          3. PACK BUFR MESSAGE BACK INTO BUFR.
C              ---------------------------------
 300  CONTINUE
C
      KBUFL=JBUFL
C
C     GET REPLICATION FACTORS
C
      if(ksec31(3).ne.0) then
         KK=0
         DO 301 K=1,KSUP(5)
         IF(KTDEXP(K).EQ.31001.OR.KTDEXP(K).EQ.31002.OR.
     2      KTDEXP(K).EQ.31000.OR.
     1      KTDEXP(K).EQ.31011.OR.KTDEXP(K).EQ.31012) THEN
            KK=KK+1
            KDATA(KK)=NINT(VALUES1(K))
         END IF
 301     CONTINUE
C
         KDLEN=2
         IF(KK.NE.0) KDLEN=KK
C
C*          3.2 ENCODE DATA INTO BUFR MESSAGE.
C               ------------------------------
 320  CONTINUE
C
C        Create key if multi subset data
C
         CALL BUPKEY(KEY1,KSEC11,KSEC2,IERR)
         IERR=0
         CALL BUFREN( KSEC0,KSEC11,KSEC2,KSEC31,KSEC4,
     1                KTDLEN,KTDLST,KDLEN,KDATA,KEL,
     2                KVALS,VALUES1,CVALS,KBUFL,KBUFR,IERR)
C
         IF(IERR.NE.0) then
            CALL EXIT(2)
         end if
C
         ILNGTH=KBUFL*NBYTES
C
C        IF(OREQ) THEN
            CALL PBWRITE(IUN,KBUFR,ILNGTH,IRET)
            IF(IRET.EQ.-1) THEN
               PRINT*,'WRITE ERROR ON FILE ',COUTFILE(1:K)
               CALL EXIT(2)
            END IF
            IFILTER=IFILTER+KSEC3(3)
c        END IF
C
         IMULTI=IMULTI+1
c
      END IF
C
      IF(KSEC32(3).NE.0.AND. OCREMFILE) THEN

         KK=0
         DO  K=1,KSUP(5)
         IF(KTDEXP(K).EQ.31001.OR.KTDEXP(K).EQ.31002.OR.
     2      KTDEXP(K).EQ.31000.OR.
     1      KTDEXP(K).EQ.31011.OR.KTDEXP(K).EQ.31012) THEN
            KK=KK+1
            KDATA(KK)=NINT(VALUES2(K))
         END IF
         END DO
C
         KDLEN=2
         IF(KK.NE.0) KDLEN=KK
C
C*          3.2 ENCODE DATA INTO BUFR MESSAGE.
C               ------------------------------
C
C        Create key if multi subset data
C
         CALL BUPKEY(KEY2,KSEC12,KSEC2,IERR)
         IERR=0
         CALL BUFREN( KSEC0,KSEC12,KSEC2,KSEC32,KSEC4,
     1                KTDLEN,KTDLST,KDLEN,KDATA,KEL,
     2                KVALS,VALUES2,CVALS,KBUFL,KBUFR,IERR)
C
         IF(IERR.NE.0) then
            CALL EXIT(2)
         end if
C
         ILNGTH=KBUFL*NBYTES
C
c        IF(OREQ) THEN
            CALL PBWRITE(IUNIT2,KBUFR,ILNGTH,IRET)
            IF(IRET.EQ.-1) THEN
               PRINT*,'WRITE ERROR ON FILE ',COUTFILE(1:K)
               CALL EXIT(2)
            END IF
c        END IF
C
      END IF
C
      GO TO 200
C
 321  CONTINUE
C
      WRITE(*,'(A)') 'ERROR WRITING INTO ',COUTFILE
      CALL EXIT(2)
C     -----------------------------------------------------------------
C
 400  CONTINUE
C
      END
      SUBROUTINE KEYCHK(KSEC1,KEY,OREQ)
C
C**** *KEYCHK*
C
C
C     PURPOSE.
C     --------
C         FILTER BUFR MESSAGES FROM INPUT FILE
C         ACCORDING TO REQUEST.
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
C         RDB KEY IN UNPACKED FORM IS USED TO CHECK AGAINST
C         USER REQUEST. IF REQUST IS MEETED LOGICAL VARIABLE
C         IS SET TO TRUE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       06/12/93.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      COMMON /COMRQB/ CTIME,CAREA,CBLOCK,CIDN,CTYPE,CSBTYPE,CRDBT,
     1                CMDBT,CQC,CIDENTS(99),CMETHOD,CSENSOR,CPRODUCT,
     2                CINSTR
C
      COMMON /COMIRQC/ ITR,IDATES(2),ITIMES(2),IAR,FLATS(4),
     1                 IBL,IBLOCK(9999),ITP,ITYPES(9999),
     2                 ISBT,ISBTYPES(9999),IRDBT,IRDBTS(9999),
     3                 IMDBT,IMDBTS(9999),IQC,IQCS(9999),IDN,
     4                 IMT,IMETHOD(9999),ISE,ISENSOR(9999),
     5                 IPR,IPRODUCT(9999),INS,INSTR(9999)
C
      CHARACTER*4 CTIME,CAREA,CBLOCK,CTYPE,CSBTYPE,CRDBT,CMDBT,CQC
      CHARACTER*4 CIDN,CMETHOD,CSENSOR,CPRODUCT,CINSTR
      CHARACTER*9 CIDENTS,CIDENT
      DIMENSION KSEC1(*),KEY(*)
      CHARACTER*14 CDATE_FIRST,CDATE_LAST,CDATE_CURRENT
C
C     -----------------------------------------------------------------
C*                  1. RDB KEY DEFINITION
C
 100  CONTINUE
C
C
C            KEY( 1)-- LENGTH OF SECTION 2 (BYTES)
C            KEY( 2)-- RDB TYPE
C            KEY( 3)-- RDB SUBTYPE
C            KEY( 4)-- YEAR
C            KEY( 5)-- MONTH
C            KEY( 6)-- DAY
C            KEY( 7)-- HOUR
C            KEY( 8)-- MINUTE
C            KEY( 9)-- SECOND
C            KEY(10)-- LONGITUDE1
C            KEY(11)-- LATITUDE1
C            KEY(12)-- LONGITUDE2
C            KEY(13)-- LATITUDE2
C            KEY(14)-- NUMBER OF SUBSETS
C            KEY(15)-- IDENT (NUMERIC)
C            KEY(16)-- IDENT ( CCITTIA5) ONE CHARACTER
C            KEY(17)-- IDENT ( CCITTIA5) ONE CHARACTER
C            KEY(18)-- IDENT ( CCITTIA5) ONE CHARACTER
C            KEY(19)-- IDENT ( CCITTIA5) ONE CHARACTER
C            KEY(20)-- IDENT ( CCITTIA5) ONE CHARACTER
C            KEY(21)-- IDENT ( CCITTIA5) ONE CHARACTER
C            KEY(22)-- IDENT ( CCITTIA5) ONE CHARACTER
C            KEY(23)-- IDENT ( CCITTIA5) ONE CHARACTER
C            KEY(24)-- IDENT ( CCITTIA5) ONE CHARACTER
C            KEY(25)-- TOTAL BUFR MESSAGE LENGTH
C            KEY(26)-- DAY    (RDB INSERTION)
C            KEY(27)-- HOUR   (RDB INSERTION)
C            KEY(28)-- MINUTE (RDB INSERTION)
C            KEY(29)-- SECOND (RDB INSERTION)
C            KEY(30)-- DAY    (MDB INSERTION)
C            KEY(31)-- HOUR   (MDB INSERTION)
C            KEY(32)-- MINUTE (MDB INSERTION)
C            KEY(33)-- SECOND (MDB INSERTION)
C            KEY(34)-- CORRECTION NUMBER
C            KEY(35)-- PART
C            KEY(36)-- 0
C            KEY(37)-- CORRECTION NUMBER
C            KEY(38)-- PART
C            KEY(39)-- 0
C            KEY(40)-- CORRECTION NUMBER
C            KEY(41)-- PART
C            KEY(42)-- 0
C            KEY(43)-- CORRECTION NUMBER
C            KEY(44)-- PART
C            KEY(45)-- 0
C            KEY(46)-- THE LOWEST Q/C % CONFIDENCE
C
C     -----------------------------------------------------------------
C
C*               1.1 GET TIME,LATITUDE,LONGITUDE,IDENT,TYPE,SUBTYPE
C                    AND RECEPTION TIMES
C
 110  CONTINUE
C
C                    TIME
C
          write(cdate_current(1:4),'(i4.4)') KEY(4)
          write(cdate_current(5:6),'(i2.2)') KEY(5)
          write(cdate_current(7:8),'(i2.2)') KEY(6)
          write(cdate_current(9:10),'(i2.2)') KEY(7)
          write(cdate_current(11:12),'(i2.2)') KEY(8)
          cdate_current(13:14)='00'
c         write(cdate_current(13:14),'(i2.2)') KEY(9)
c
          write(cdate_first(1:8),'(i8.8)') IDATES(1)
          write(cdate_first(9:14),'(i6.6)') ITIMES(1)
c
          write(cdate_last(1:8),'(i8.8)') IDATES(2)
          write(cdate_last(9:14),'(i6.6)') ITIMES(2)
c
C
C                    LAT AND LONG
C
      RKLAT=(KEY(11)-9000000)/100000.
      RKLON=(KEY(10)-18000000)/100000.
      if(KEY(11).eq.0) RKLAT=-90.0
      if(KEY(10).eq.0) RKLON=-180.0
C
C                    IDENT
C
 105  CONTINUE
C
      CIDENT=' '
      IF(KEY(3).EQ.51.OR.KEY(3).EQ.61.OR.
     1   KEY(3).EQ.62.OR.KEY(3).EQ.63.OR.
     1   KEY(3).EQ.71.OR.KEY(3).EQ.72.OR.
     1   KEY(3).EQ.73.OR.KEY(3).EQ.82.OR.
     1   KEY(3).EQ.83.OR.KEY(3).EQ.84.OR.
     1   KEY(3).EQ.85.OR.KEY(3).EQ.121.OR.
     1   KEY(3).EQ.86.OR.KEY(3).EQ.87.OR.
     1   KEY(3).EQ.88.OR.KEY(3).EQ.89.OR.
     1   KEY(3).EQ.122.OR.KEY(3).EQ.123.OR.
     1   KEY(3).EQ.124.OR.KEY(3).EQ.125.OR.
     1   KEY(3).EQ.65.OR.KEY(3).EQ.75) THEN
         WRITE(CIDENT(1:3),'(I3)',IOSTAT=IOS) KEY(15)
         IF(IOS.NE.0) THEN
            PRINT*,'INTERNAL WRITE ERROR.'
            CALL EXIT(2)
         END IF
C     print*,'cident=',CIDENT(1:3)
         IF(CIDENT(1:1).EQ.' ') THEN
            CIDENT(1:1)=CIDENT(2:2)
            CIDENT(2:2)=CIDENT(3:3)
            CIDENT(3:3)=' '
         END IF
         IDL=INDEX(CIDENT,' ')
         IDL=IDL-1
      ELSE
         J=0
         DO 101 I=16,24
         J=J+1
         CIDENT(J:J)=CHAR(KEY(I))
 101     CONTINUE
         IDL=INDEX(CIDENT,' ')
         IDL=IDL-1
C         IF(IDL.EQ.0) THEN
C            J=15
C            DO 102 I=16,24
C            IF(KEY(I).EQ.32) THEN
C               GO TO 102
C            ELSE
C               J=J+1
C               KEY(J)=KEY(I)
C               KEY(I)=32
C            END IF
C 102      CONTINUE
C          GO TO 105
C          END IF
      END IF
C
C                    TYPE AND SUBTYPE
C
      IKTYPE=KEY(2)
      IKSBTYPE=KEY(3)
C
C                    LOWEST QUALITY CONTROL
      IKQC=KEY(46)
C
C                    RECEPTION TIME
C
      IKRDB=KEY(26)*10**6+KEY(27)*10**4+KEY(28)*10**2+KEY(29)
      IKMDB=KEY(30)*10**6+KEY(31)*10**4+KEY(32)*10**2+KEY(33)
C     ------------------------------------------------------------------
C*                   2. CHECK MATCHING
C
 200  CONTINUE
C
C     CHECK TIME
C
      IF(ITR.NE.0) THEN
          IF(ITR.EQ.1) THEN
             if(cdate_current.ne.cdate_first) then
                OREQ=.FALSE.
                RETURN
             END IF
          ELSE
             if(cdate_current.lt.cdate_first.or.
     1          cdate_current.gt.cdate_last) then
                OREQ=.FALSE.
                RETURN
             END IF
          END IF
      END IF
C
C     CHECK AREA
C
      IF(IAR.NE.0) THEN
         IF(RKLAT.GT.FLATS(1).OR.RKLAT.LT.FLATS(3)) THEN
            OREQ=.FALSE.
            RETURN
         ELSE
            IF(FLATS(4).LT.FLATS(2)) THEN
               RLO3=180.00000
               RLO4=-180.00000
               IF(RKLON.LT.FLATS(2).OR.RKLON.GT.RLO3) THEN
                  IF(RKLON.LT.RLO4.OR.RKLON.GT.FLATS(4)) THEN
                     OREQ=.FALSE.
                     RETURN
                  END IF
               END IF
            ELSE
               IF(RKLON.LT.FLATS(2).OR.RKLON.GT.FLATS(4)) THEN
                  OREQ=.FALSE.
                  RETURN
               END IF
            END IF
         END IF
      END IF
C
C     CHECK BLOCK
C
      IF(IBL.NE.0) THEN
         IKBLOCK=(KEY(16)-48)*10+KEY(17)-48
         OBLOCK=.FALSE.
         DO 202 I=1,IBL
         IF(IKBLOCK.EQ.IBLOCK(I)) OBLOCK=.TRUE.
 202     CONTINUE
         IF(.NOT.OBLOCK) THEN
            OREQ=.FALSE.
            RETURN
         END IF
      END IF
C
C     IDENT
C
      IF(IDN.NE.0) THEN
         OCIDENT=.FALSE.
         DO 203 I=1,IDN
         IF(CIDENT(1:IDL).EQ.CIDENTS(I)(1:IDL)) OCIDENT=.TRUE.
 203     CONTINUE
         IF(.NOT.OCIDENT) THEN
            OREQ=.FALSE.
            RETURN
         END IF
      END IF
C
C     TYPE
C
      IF(ITP.NE.0) THEN
         OTYPE=.FALSE.
         DO 204 I=1,ITP
         IF(KSEC1(6).EQ.ITYPES(I)) OTYPE=.TRUE.
 204     CONTINUE
         IF(.NOT.OTYPE) THEN
           OREQ=.FALSE.
           RETURN
         END IF
      END IF
C
C     SUBTYPE
C
      IF(ISBT.NE.0) THEN
         OSBT=.FALSE.
         DO 205 I=1,ISBT
         IF(ISBTYPES(I).EQ.KSEC1(7)) OSBT=.TRUE.
 205     CONTINUE
         IF(.NOT.OSBT) THEN
           OREQ=.FALSE.
           RETURN
         END IF
      END IF
C
C     LOWEST QUALITY
C
      IF(IQC.NE.0) THEN
         IF(IQC.EQ.1) THEN
            IF(IKQC.NE.IQCS(1)) THEN
               OREQ=.FALSE.
               RETURN
            END IF
         ELSE
            IF(IKQC.LT.IQCS(1).OR.IKQC.GT.IQCS(2)) THEN
               OREQ=.FALSE.
               RETURN
            END IF
         END IF
      END IF
C
C     RDB TIME
C
      IF(IRDBT.NE.0) THEN
         IF(IRDBT.EQ.1) THEN
            IF(IKRDB.NE.IRDBTS(1)) THEN
               OREQ=.FALSE.
               RETURN
            END IF
         ELSE
            IF(IKRDB.LT.IRDBTS(1).OR.IKRDB.GT.IRDBTS(2)) THEN
               OREQ=.FALSE.
               RETURN
            END IF
         END IF
      END IF
C
C     MDB TIME
C
      IF(IMDBT.NE.0) THEN
         IF(IMDBT.EQ.1) THEN
            IF(IKMDB.NE.IMDBTS(1)) THEN
               OREQ=.FALSE.
               RETURN
            END IF
         ELSE
            IF(IKMDB.LT.IMDBTS(1).OR.IKMDB.GT.IMDBTS(2)) THEN
               OREQ=.FALSE.
               RETURN
            END IF
         END IF
      END IF

C
C     COMPUTATIONAL METHOD
C
      IF(IMT.NE.0) THEN
         OREQ=.FALSE.
         RETURN
      END IF
C
C     PRODUCT TYPE
C
      IF(IPR.NE.0) THEN
         OREQ=.FALSE.
         RETURN
      END IF
C     -----------------------------------------------------------------
      OREQ=.TRUE.
C
      RETURN
C
 401  CONTINUE
C
      PRINT*,'INTERNAL READ ERROR.'
      CALL EXIT(2)
      END
      SUBROUTINE MESSCHCK(OREQ,KEY,KTDEXL,KTDEXP,
     1                    KSUP,KSEC0 ,KSEC1,KSEC2 ,KSEC3 ,
     1                    KSEC4,KEL,CNAMES,CUNITS,KVALS,VALUES,CVALS,
     3                    ksec11,ksec12,ksec31,ksec32,
     4                    key1,key2,values1,values2)
C
C**** *MESSCHK*
C
C
C     PURPOSE.
C     --------
C         FILTER BUFR MESSAGES FROM INPUT FILE
C         ACCORDING TO REQUEST.
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
C          IN THE CASE OF MULTI-SUBSET BUFR MESSAGES TO FIND REQUESTED
C          OBSERVATION IT IS NEEDED TO USE FULLY EXPANDED MESSAGE.
C          EVERY SUBSET IS CHECKED AGAINST REQUEST. NEEDED MESSAGES ARE
C          COLLECTED AND RETURNED, AND LOGICAL VARIABLE OREQ SET TO
C          TRUE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       06/12/93.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      COMMON /COMRQB/ CTIME,CAREA,CBLOCK,CIDN,CTYPE,CSBTYPE,CRDBT,
     1                CMDBT,CQC,CIDENTS(99),CMETHOD,CSENSOR,CPRODUCT,
     2                CINSTR
C
      COMMON /COMIRQC/ ITR,IDATES(2),ITIMES(2),IAR,FLATS(4),
     1                 IBL,IBLOCK(9999),ITP,ITYPES(9999),
     2                 ISBT,ISBTYPES(9999),IRDBT,IRDBTS(9999),
     3                 IMDBT,IMDBTS(9999),IQC,IQCS(9999),IDN,
     3                 IMT,IMETHOD(9999),ISE,ISENSOR(9999),
     5                 IPR,IPRODUCT(9999),INS,INSTR(9999)
C
      CHARACTER*4 CTIME,CAREA,CBLOCK,CTYPE,CSBTYPE,CRDBT,CMDBT,CQC
      CHARACTER*4 CIDN,CMETHOD , CSENSOR, CPRODUCT,CINSTR
      CHARACTER*9 CIDENTS,CIDENT
      DIMENSION KSEC0(*),KSEC1(*),KSUP(*),KSEC2(*),KSEC3(*),KSEC4(*)
      REAL*8 VALUES(*),VALUES1(*),VALUES2(*)
      DIMENSION KTDEXP(*),KEY(*)
      CHARACTER*(*) CVALS(*),CNAMES(*),CUNITS(*)
      DIMENSION KSEC11(*),KSEC12(*),KSEC31(*),KSEC32(*)
      DIMENSION KEY1(*),KEY2(*)
      CHARACTER*14 CDATE_FIRST,CDATE_LAST,CDATE_CURRENT
C
C     -----------------------------------------------------------------
C
C                1. FIND TIME,LAT,LONG,IDENT,TYPE,SUBTYPE E.T.C.
C
 100  CONTINUE
C
      NN1=0                   ! NUMBER OF SUBSETS
      NN2=0
      ksec31(3)=0
      ksec32(3)=0
C
      IPAID=0                ! AICRAFT ID
      IPSID=0                ! SATELLITE ID
      IPBUOY=0               ! BUOY/PLATFORM ID
      IPARN=0                ! AIRCRAFT REGISTRATION NUMBER
      IPSHIP=0               ! SHIP CALL SIGN
      IPBLOK=0               ! BLOCK NUMBER
      IPSTAT=0               ! STATION NUMBER
      IPYEAR=0
      IPMONTH=0
      IPDAY=0
      IPHOUR=0
      IPMINUTE=0
      IPSECOND=0
      IPSECMIC=0             ! 004007 seconds with micro second precission
      IPLAT=0
      IPLON=0
      IMETH=0                ! Wind computational method
      ISENS=0                ! Satellite sensor indicator
      IPROD=0
      INSTRUM=0              ! Satellite instrument 002019
C
      OREQ1=.FALSE.
C
      do i=1,36
        IF(KTDEXP(I).EQ.001006) then
           if(IPAID.eq.0) IPAID=I
        end if
        IF(KTDEXP(I).EQ.001007) then
           if(IPSID.eq.0) IPSID=I
        end if
        IF(KTDEXP(I).EQ.001005) then
           if(IPBUOY.eq.0) IPBUOY=I
        end if
        IF(KTDEXP(I).EQ.001008) then
           if(IPARN.eq.0) IPARN=I
        end if
        IF(KTDEXP(I).EQ.001011) then
           if(IPSHIP.eq.0) IPSHIP=I
        end if
        IF(KTDEXP(I).EQ.001001) then
           if(IPBLOK.eq.0)  IPBLOK=I
        end if
        IF(KTDEXP(I).EQ.001002) then
           if(IPSTAT.eq.0) IPSTAT=I
        end if
        IF(KTDEXP(I).EQ.004001) then
           if(IPYEAR.eq.0) IPYEAR=I
        end if
        IF(KTDEXP(I).EQ.004002) then
           if(IPMONTH.eq.0) IPMONTH=I
        end if
        IF(KTDEXP(I).EQ.004003) then
            if(IPDAY.eq.0) IPDAY=I
        end if
        IF(KTDEXP(I).EQ.004004) then
           if(IPHOUR.eq.0) IPHOUR=I
        end if
        IF(KTDEXP(I).EQ.004005) then
           if(IPMINUTE.eq.0) IPMINUTE=I
        end if
        IF(KTDEXP(I).EQ.004006) then
           if(IPSECOND.eq.0) IPSECOND=I
        end if
        IF(KTDEXP(I).EQ.004007) then
           if(IPSECMIC.eq.0) IPSECMIC=I
        end if
        IF(KTDEXP(I).EQ.005001) THEN
           IF(IPLAT.EQ.0) IPLAT=I
        END IF
        IF(KTDEXP(I).EQ.006001) THEN
           IF(IPLON.EQ.0) IPLON=I
        END IF
        IF(KTDEXP(I).EQ.005002) THEN
           IF(IPLAT.EQ.0) IPLAT=I
        END IF
        IF(KTDEXP(I).EQ.006002) THEN
           IF(IPLON.EQ.0) IPLON=I
        END IF
        IF(KTDEXP(I).EQ.002023) then
           if(IMETH.eq.0) IMETH=I
        end if
        IF(KTDEXP(I).EQ.002048) then
           if(ISENS.eq.0)ISENS=I
        end if
        IF(KTDEXP(I).EQ.002019) then
           IF(INSTRUM.EQ.0) INSTRUM=I
        END IF
        IF(KTDEXP(I).EQ.002172) then
           if(IPROD.eq.0) IPROD=I
        END IF
      end do
C
C
C
C                    IDENT
C
      CIDENT=' '
C
C                    TYPE AND SUBTYPE
C
      IKTYPE=KEY(2)
      IKSBTYPE=KEY(3)
C
C
C                    LOWEST QUALITY CONTROL
      IKQC=KEY(46)
C
C                    RECEPTION TIME
C
      IKRDB=KEY(26)*10**6+KEY(27)*10**4+KEY(28)*10**2+KEY(29)
      IKMDB=KEY(30)*10**6+KEY(31)*10**4+KEY(32)*10**2+KEY(33)
C     ------------------------------------------------------------------
C*                   2. CHECK MATCHING
C
 200  CONTINUE
C
C
C     TYPE
C
      IF(ITP.NE.0) THEN
         OTYPE=.FALSE.
         DO 204 IX=1,ITP
         IF(KSEC1(6).EQ.ITYPES(IX)) OTYPE=.TRUE.
 204     CONTINUE
         IF(.NOT.OTYPE) THEN
           OREQ=.FALSE.
           RETURN
         ELSE
           OREQ=.TRUE.
         END IF
      END IF
C
C     SUBTYPE
C
      IF(ISBT.NE.0) THEN
         OSBT=.FALSE.
         DO 205 IX=1,ISBT
         IF(ISBTYPES(IX).EQ.KSEC1(7)) OSBT=.TRUE.
 205     CONTINUE
         IF(.NOT.OSBT) THEN
           OREQ=.FALSE.
           RETURN
         ELSE
           OREQ=.TRUE.
         END IF
      END IF
C
C     LOWEST QUALITY
C
      IF(IQC.NE.0) THEN
         IF(IQC.EQ.1) THEN
            IF(IKQC.NE.IQCS(1)) THEN
               OREQ=.FALSE.
               RETURN
            ELSE
               OREQ=.TRUE.
            END IF
         ELSE
            IF(IKQC.LT.IQCS(1).AND.IKQC.GT.IQCS(2)) THEN
               OREQ=.FALSE.
               RETURN
            ELSE
               OREQ=.TRUE.
            END IF
         END IF
      END IF
C
C     RDB TIME
C
      IF(IRDBT.NE.0) THEN
         IF(IRDBT.EQ.1) THEN
            IF(IKRDB.NE.IRDBTS(1)) THEN
               OREQ=.FALSE.
               RETURN
            ELSE
               OREQ=.TRUE.
            END IF
         ELSE
            IF(IKRDB.LT.IRDBTS(1).OR.IKRDB.GT.IRDBTS(2)) THEN
               OREQ=.FALSE.
               RETURN
            ELSE
               OREQ=.TRUE.
            END IF
         END IF
      END IF
C
C     MDB TIME
C
      IF(IMDBT.NE.0) THEN
         IF(IMDBT.EQ.1) THEN
            IF(IKMDB.NE.IMDBTS(1)) THEN
               OREQ=.FALSE.
               RETURN
            ELSE
               OREQ=.TRUE.
            END IF
         ELSE
            IF(IKMDB.LT.IMDBTS(1).OR.IKMDB.GT.IMDBTS(2)) THEN
               OREQ=.FALSE.
               RETURN
            ELSE
               OREQ=.TRUE.
            END IF
         END IF
      END IF
C
C
      DO 201 I=1,KSEC3(3)
C
C     CHECK TIME
C

      OREQ=.true.
      OREQ1=.false.
c
      IKEL=(I-1)*KEL
      IF(ITR.NE.0) THEN
          IY=IPYEAR+IKEL
          IM=IPMONTH+IKEL
          ID=IPDAY+IKEL
          IH=IPHOUR+IKEL
          IMM=IPMINUTE+IKEL
          IF(IPSECOND.EQ.0) THEN
             IS=0
             IF(IPSECMIC.NE.0) THEN
                IS=IPSECMIC+IKEL
             END IF
          ELSE
          IS=IPSECOND+IKEL
          END IF
          write(cdate_current(1:4),'(i4.4)') NINT(VALUES(IY))
          write(cdate_current(5:6),'(i2.2)') NINT(VALUES(IM))
          write(cdate_current(7:8),'(i2.2)') NINT(VALUES(ID))
          write(cdate_current(9:10),'(i2.2)') NINT(VALUES(IH))
          write(cdate_current(11:12),'(i2.2)') NINT(VALUES(IMM))
          if(IS.eq.0) then
             cdate_current(13:14)='00'
          else
          write(cdate_current(13:14),'(i2.2)') NINT(VALUES(IS))
          end if
c
          write(cdate_first(1:8),'(i8.8)') IDATES(1)
          write(cdate_first(9:14),'(i6.6)') ITIMES(1)
c
          write(cdate_last(1:8),'(i8.8)') IDATES(2)
          write(cdate_last(9:14),'(i6.6)') ITIMES(2)
c
          IF(ITR.EQ.1) THEN
             if(cdate_current.ne.cdate_first) then
                OREQ=.FALSE.
                GO TO 2011
             else
                OREQ1=.TRUE.
             END IF
          ELSE
             if(cdate_current.lt.cdate_first.or.
     1          cdate_current.gt.cdate_last) then
                   OREQ=.FALSE.
                   GO TO 2011
             else
                OREQ1=.TRUE.
             END IF
          END IF
      END IF
C
C     CHECK AREA
C
      IF(IAR.NE.0.and.IPLAT.ne.0.and.IPLON.ne.0) THEN
         ILA=IPLAT+IKEL
         ILO=IPLON+IKEL
         RKLAT=VALUES(ILA)
         RKLON=VALUES(ILO)
C
         IF(RKLAT.GT.FLATS(1).OR.RKLAT.LT.FLATS(3).OR.
     1      RKLON.LT.FLATS(2).OR.RKLON.GT.FLATS(4)) THEN
            OREQ=.FALSE.
            GO TO 2011
         ELSE
             OREQ1=.TRUE.
         END IF
      END IF
C
C     CHECK BLOCK
C
      IF(IBL.NE.0.and.IPBLOK.ne.0) THEN
         IB=IPBLOK+IKEL
         IKBLOCK=NINT(VALUES(IB))
         OBLOCK=.FALSE.
         DO 202 IX=1,IBL
         IF(IKBLOCK.EQ.IBLOCK(IX)) OBLOCK=.TRUE.
 202     CONTINUE
         IF(.NOT.OBLOCK) THEN
            OREQ=.FALSE.
            GO TO 2011
         ELSE
            OREQ1=.TRUE.
         END IF
      END IF
C
C     CHECK COMPUTATIONAL METHOD
C
      IF(IMT.NE.0.AND.IMETH.NE.0) THEN
         IM=IMETH+IKEL
         IKMETHOD=NINT(VALUES(IM))
         OMETHOD=.false.
         do  ix=1,IMT
         IF(IKMETHOD.eq.IMETHOD(ix)) OMETHOD=.true.
         end do
         IF(.NOT.OMETHOD) THEN
            OREQ=.FALSE.
            GO TO 2011
         ELSE
            OREQ1=.TRUE.
         END IF
       END IF
C
C     CHECK SATELLITE SENSOR INDICATOR
C
      IF(ISE.NE.0.AND.ISENS.NE.0) THEN
         IM=ISENS+IKEL
         IKSENSOR=NINT(VALUES(IM))
         OSENSOR=.false.
         do  ix=1,ISE
         IF(IKSENSOR.eq.ISENSOR(ix)) OSENSOR=.true.
         end do
         IF(.NOT.OSENSOR) THEN
            OREQ=.FALSE.
            GO TO 2011
         ELSE
            OREQ1=.TRUE.
         END IF
       END IF
C
C
C     CHECK SATELLITE INSTRUMENT INDICATOR
C
      IF(INS.NE.0.AND.INSTRUM.NE.0) THEN
         IM=INSTRUM+IKEL
         IKINSTRUM=NINT(VALUES(IM))
         OINSTR=.false.
         do  ix=1,INS
         IF(IKINSTRUM.eq.INSTR(ix)) OINSTR=.true.
         end do
         IF(.NOT.OINSTR) THEN
            OREQ=.FALSE.
            GO TO 2011
         ELSE
            OREQ1=.TRUE.
         END IF
       END IF

C
C     CHECK SATELLITE PRODUCT TYPE
C
      IF(IPR.NE.0.AND.IPROD.NE.0) THEN
         IM=IPROD+IKEL
         IKPRODUCT=NINT(VALUES(IM))
         OPRODUCT=.false.
         do  ix=1,IPR
         IF(IKPRODUCT.eq.IPRODUCT(ix)) OPRODUCT=.true.
         end do
         IF(.NOT.OPRODUCT) THEN
            OREQ=.FALSE.
            GO TO 2011
         ELSE
            OREQ1=.TRUE.
         END IF
       END IF


C
C     IDENT
C
      IF(IDN.NE.0) THEN
         IF(IPAID.NE.0) THEN
            II=IPAID+IKEL
            K=NINT(VALUES(II)/1000)
            IDL=NINT(VALUES(II)-K*1000)
            CIDENT(1:IDL)=CVALS(K)(1:IDL)
         END IF
         IF(IPSID.NE.0) THEN
            II=IPSID+IKEL
            WRITE(CIDENT(1:3),'(I3)',iostat=ios)
     1                          NINT(VALUES(II))
            IF(IOS.NE.0) THEN
               PRINT*,'INTERNAL WRITE ERROR.'
               CALL EXIT(2)
            END IF
cs          print*,'cident1=',CIDENT(1:3)
            IF(CIDENT(1:1).EQ.' '.AND.CIDENT(2:2).EQ.' ') THEN
              CIDENT(1:1)=CIDENT(3:3)
              CIDENT(2:2)=' '
              CIDENT(3:3)=' '
            ELSEIF(CIDENT(1:1).EQ.' ') THEN
              CIDENT(1:1)=CIDENT(2:2)
              CIDENT(2:2)=CIDENT(3:3)
              CIDENT(3:3)=' '
            END IF
cs          print*,'cident2=',CIDENT(1:3)
            IDL=3
         END IF
         IF(IPBUOY.NE.0) THEN
            II=IPBUOY+IKEL
            WRITE(CIDENT(1:9),'(I9)',IOSTAT=IOS)
     1                          NINT(VALUES(II))
            IF(IOS.NE.0) THEN
               PRINT*,'INTERNAL WRITE ERROR.'
               CALL EXIT(2)
            END IF
            IDL=9
         END IF
         IF(IPARN.NE.0) THEN
            II=IPARN+IKEL
            K=NINT(VALUES(II)/1000)
            IDL=NINT(VALUES(II)-K*1000)
            CIDENT(1:IDL)=CVALS(K)(1:IDL)
         END IF
         IF(IPSHIP.NE.0) THEN
            II=IPSHIP+IKEL
            K=NINT(VALUES(II)/1000)
            IDL=NINT(VALUES(II)-K*1000)
            CIDENT(1:IDL)=CVALS(K)(1:IDL)
         END IF
         IF(IPBLOK.NE.0.AND.IPSTAT.NE.0) THEN
            IBB=IPBLOK+IKEL
            IBS=IPSTAT+IKEL
            IBLST=NINT(VALUES(IBB)*1000+VALUES(IBS))
            WRITE(CIDENT(1:5),'(I5.5)',IOSTAT=IOS) IBLST
            IF(IOS.NE.0) THEN
               PRINT*,'INTERNAL WRITE ERROR.'
               CALL EXIT(2)
            END IF
            IDL=5
         END IF
C
         OCIDENT=.FALSE.
         IF(IDL.NE.0) THEN
            DO 203 IX=1,IDN
            IF(CIDENT(1:IDL).EQ.CIDENTS(IX)(1:IDL)) OCIDENT=.TRUE.
 203        CONTINUE
         END IF
         IF(.NOT.OCIDENT) THEN
            OREQ=.FALSE.
            GO TO 2011
         ELSE
            OREQ1=.TRUE.
         END IF
      END IF
C
C     -----------------------------------------------------------------
 2011 continue

      if(OREQ) then
         if(itr.eq.0.and.
     1      iar.eq.0.and.
     2      ibl.eq.0.and.
     3      imt.eq.0.and.
     4      ise.eq.0.and.
     4      ipr.eq.0.and.
     5      idn.eq.0) OREQ1=.true.
      end if
c
      IF(OREQ1.AND.OREQ) then
C
         NN1=NN1+1
c        PRINT*,'SUBSET MATCHED ',NN1
         DO  K=1,KSUP(5)
           IKK=K+(NN1-1)*KEL
           IKC=K+(I-1)*KEL
           VALUES1(IKK)=VALUES(IKC)
         END DO
      ELSE
         NN2=NN2+1
c        PRINT*,'SUBSET not MATCHED ',NN2
         DO  K=1,ksup(5)
           IKK=K+(NN2-1)*KEL
           IKC=K+(I-1)*KEL
           VALUES2(IKK)=VALUES(IKC)
         END DO
      END IF
C
 201  CONTINUE
C
         KSEC31(3)=NN1
         KSEC31(4)=128
         IF(NN1.GT.1) KSEC31(4)=192

         KSEC32(3)=NN2
         KSEC32(4)=128
         IF(NN2.GT.1) KSEC32(4)=192
C
C              3. CREATE NEW SECTION 1 AND THE RDB KEY.
C                 -------------------------------------
 300  CONTINUE
C
      do k=1,40
        ksec11(k)=ksec1(k)
        ksec12(k)=ksec1(k)
      end do
      do k=1,46
        key1(k)=key(k)
        key2(k)=key(k)
      end do
c
      if(nn1.ne.0) then
      KSEC11(5)=128
      RYYY=1900.
      IF(NINT(VALUES1(IPYEAR)).GE.2001) RYYY=2000.
      KSEC11( 9)=NINT(VALUES1(IPYEAR)-RYYY)
      KSEC11(10)=NINT(VALUES1(IPMONTH))
      KSEC11(11)=NINT(VALUES1(IPDAY))
      KSEC11(12)=NINT(VALUES1(IPHOUR))
      KSEC11(13)=NINT(VALUES1(IPMINUTE))
c
      CALL BUCRKEY(KEL,KTDEXP,KSUP,KSEC11,KSEC31,KEY1,
     1                     VALUES1,CVALS,KERR)
      end if
c
      if(nn2.ne.0) then
      KSEC12(5)=128
      RYYY=1900.
      IF(NINT(VALUES2(IPYEAR)).GE.2001) RYYY=2000.
      KSEC12( 9)=NINT(VALUES2(IPYEAR)-RYYY)
      KSEC12(10)=NINT(VALUES2(IPMONTH))
      KSEC12(11)=NINT(VALUES2(IPDAY))
      KSEC12(12)=NINT(VALUES2(IPHOUR))
      KSEC12(13)=NINT(VALUES2(IPMINUTE))
c
      CALL BUCRKEY(KEL,KTDEXP,KSUP,KSEC12,KSEC32,KEY2,
     1                     VALUES2,CVALS,KERR)
      end if


      KEY1(46)=70
      KEY2(46)=70
C
c     OREQ=.false.
c     IF(OREQ1) OREQ=.true.
      RETURN

C
 401  CONTINUE
C
      PRINT*,'INTERNAL READ ERROR.'
      CALL EXIT(2)
      END
