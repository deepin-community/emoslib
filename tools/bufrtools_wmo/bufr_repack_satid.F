      PROGRAM BUFRREPACK
C
C**** *BUFRREPACK*
C
C
C     PURPOSE.
C     --------
C         REPACKS BUFR  DATA.
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
C          M. DRAGOSAVAC    *ECMWF*       15/02/95.
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
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2=4096,JSEC3= 4,
     1      JSEC4=   2,JELEM=160000,JSUBS=400,JCVAL=150 ,JBUFL=512000,
#ifdef JBPW_64
     2          JBPW =  64,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200,
#else
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200,
#endif
     3          JWORK=4096000,JKEY=46,JBYTE=512000)
C
      PARAMETER (KELEM=80000)
      PARAMETER (KVALS=360000)
C 
      DIMENSION KBUFF(JBUFL)
      DIMENSION KBUFR(JBUFL)
      DIMENSION KSUP(JSUP)  ,KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION KSEC2(JSEC2),KSEC3(JSEC3),KSEC4(JSEC4)
      DIMENSION KEY  (JKEY),KREQ(2)
C
      REAL*8 VALUES(KVALS),VALUE(KVALS),RQV(KELEM)
      REAL*8 RVIND,EPS
      DIMENSION KTDLST(KELEM),KTDEXP(KELEM),KRQ(KELEM)
      DIMENSION KDATA(200)
      DIMENSION IOUT(12800)
C
      CHARACTER*256 CF(100),COUT,CFIN
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
      CHARACTER*80 YENC
      CHARACTER*256 CARG(10)
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
      ITLEN=6400
      ITL=0
      JZ=0
      NW=0
      NBYTES=JBPW/8
      RVIND=1.7D38
      NVIND=2147483647
      IOBS=0
      KRQL=0
      NR=2555
      KREQ(1)=0
      KREQ(2)=0
      DO 102 I=1,KELEM
      RQV(I)=RVIND
      KRQ(I)=NVIND
 102  CONTINUE
C
C     INPUT FILE NAMES
C
      NARG=IARGC()
      IF(NARG.LT.4) THEN
         PRINT*,'USAGE -- bufr_repack -i infile -o outfile'
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
      ELSEIF(CARG(J).EQ.'-o') THEN
         IO=J
      END IF
 105  CONTINUE
      IF(IO.EQ.0.OR.IN.EQ.0) THEN
         PRINT*,'USAGE -- bufr_repack -i infile -o outfile'
         STOP
      END IF
C
      COUT=CARG(IO+1)
C
      IF(IO.LT.IN) THEN
         IST=IN+1
         IEND=NARG
      ELSE
         IST=IN+1
         IEND=IO-1
      END IF
C
      JJ=INDEX(COUT,' ')
C
      CALL PBOPEN(IUNIT1,COUT(1:JJ),'W',IRET)
      IF(IRET.EQ.-1) STOP 'OPEN FAILED ON BUFR.DAT'
      IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'
C
      DO 101 II=IST,IEND
      CFIN=CARG(II) 
      ILN=INDEX(CFIN,' ')
C
C*          1.2 OPEN FILE CONTAINING BUFR DATA.
C               -------------------------------
 120  CONTINUE
C
      IRET=0 
      CALL PBOPEN(IUNIT,CFIN(1:ILN),'R',IRET)
      IF(IRET.EQ.-1) STOP 'OPEN FAILED'
      IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'
C
C     ----------------------------------------------------------------- 
C*          2. SET REQUEST FOR EXPANSION.
C              --------------------------
 200  CONTINUE
C
      OPRT=.FALSE.
      OENC=.TRUE.
      NCOM=1
      OCOMP=.FALSE.
      NR=0
      OSEC3=.FALSE.
C
C*          2.1 SET REQUEST FOR PARTIAL EXPANSION.
C               ----------------------------------
 210  CONTINUE
C
C     SET VARIABLE TO PACK BIG VALUES AS MISSING VALUE INDICATOR
C
      KPMISS=1
      KPRUS=0
      NOKEY=0
      CALL BUPRQ(KPMISS,KPRUS,NOKEY)
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
          GO TO 900
      END IF
      IF(IRET.EQ.-2) STOP 'FILE HANDLING PROBLEM' 
      IF(IRET.EQ.-3) STOP 'ARRAY TOO SMALL FOR PRODUCT'
C
      N=N+1
      IKBUFL=KBUFL
      KBUFL=KBUFL/NBYTES+1
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
      IF(KSUP(6).GT.1) THEN
         KEL=KVALS/KSUP(6)
         if(KEL .gt.kelem) kel=kelem
      ELSE
         KEL=KELEM
      END IF
C
      CALL BUFREX(KBUFL,KBUFF,KSUP,KSEC0 ,KSEC1,KSEC2 ,KSEC3 ,KSEC4,
     1            KEL,CNAMES,CUNITS,KVALS,VALUES,CVALS,IERR)
C
      IF(IERR.NE.0) THEN
         IF(IERR.EQ.45) GO TO 300
         CALL EXIT(2)
      END IF
      IOBS=IOBS+KSEC3(3)
C
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
      IF(KERR.NE.0) CALL EXIT(2)
C
C*          4.1 PRINT CONTENT OF EXPANDED DATA.
C               -------------------------------
 410  CONTINUE
C
      IF(.NOT.OPRT) GO TO 500
      IF(.NOT.OSEC3) GO TO 450
C
C*          4.2 PRINT SECTION ZERO OF BUFR MESSAGE.
C               -----------------------------------
 420  CONTINUE
C

      CALL BUPRS0(KSEC0)
C
C*          4.3 PRINT SECTION ONE OF BUFR MESSAGE.
C               -----------------------------------
 430  CONTINUE
C
      CALL BUPRS1(KSEC1)
C
C
C*          4.4 PRINT SECTION TWO OF BUFR MESSAGE.
C               -----------------------------------
 440  CONTINUE
C
C              AT ECMWF SECTION 2 CONTAINS RDB KEY.
C              SO UNPACK KEY
C
      CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)
C
C              PRINT KEY
C
      CALL BUPRS2(KSUP ,KEY)
C
C*          4.5 PRINT SECTION 3 OF BUFR MESSAGE.
C               -----------------------------------
 450  CONTINUE
C
C               FIRST GET DATA DESCRIPTORS
C
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
      IF(KERR.NE.0) CALL EXIT(2)
C
C               PRINT  CONTENT
C
      IF(OSEC3) THEN
         CALL BUPRS3(KSEC3,KTDLEN,KTDLST,KTDEXL,KTDEXP,KEL,CNAMES)
      END IF
C
C*         4.6 PRINT SECTION 4 (DATA).
C              -----------------------
 460  CONTINUE
C
C          IN THE CASE OF MANY SUBSETS DEFINE RANGE OF SUBSETS
C
      IF(.NOT.OO) THEN
      WRITE(*,'(A,$)') ' STARTING SUBSET TO BE PRINTED : '
      READ(*,'(BN,I4)')   IST
      WRITE(*,'(A,$)') ' ENDING SUBSET TO BE PRINTED : '
      READ(*,'(BN,I4)')   IEND
      OO=.FALSE.
      END IF
C
C              PRINT DATA
C
      ICODE=0
      CALL BUPRT(ICODE,IST,IEND,KEL,CNAMES,CUNITS,CVALS,
     1           KVALS,VALUES,KSUP,KSEC1,IERR)
C
C              RESOLVE BIT MAPS 
C
      IF(IEND.GT.KSEC3(3)) IEND=KSEC3(3)
C
      DO 461 IK=IST,IEND
C 
      CALL BUBOX(IK,KSUP,KEL,KTDEXP,CNAMES,CUNITS,KVALS,VALUES,
     1           KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU,IERR)
C
      CALL BUPRTBOX(KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU)
C
 461  CONTINUE
C
C     -----------------------------------------------------------------
C*          5. COLLECT DATA FOR REPACKING.
C              ---------------------------
 500  CONTINUE
C      
      IF(.NOT.OENC) GO TO 300
C
C               FIRST GET DATA DESCRIPTORS
C
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
      IF(KERR.NE.0) CALL EXIT(2)
C
C     -----------------------------------------------------------------
C*          6. PACK BUFR MESSAGE BACK INTO BUFR.
C              ---------------------------------
 600  CONTINUE
C
    
      KKK=0
      KBUFL=JBUFL
C
C     GET REPLICATION FACTORS
C
      KK=0
      DO 601 K=1,KSUP(5)
      IF(KTDEXP(K).EQ.31001.OR.KTDEXP(K).EQ.31002.OR.
     1   KTDEXP(K).EQ.31000) THEN
         KK=KK+1
         KDATA(KK)=NINT(VALUES(K))
      END IF
 601  CONTINUE
C
      KDLEN=2
      IF(KK.NE.0) KDLEN=KK
C
C     --------------------------------
C     |Modification to sections error|
C     --------------------------------
      if(ksec1(7).eq.54.or.ksec1(7).eq.55.or.ksec1(7).eq.155) then
         do i=1,ksec3(3)
         ij=7+(i-1)*kel
         if(nint(values(ij)).eq.205) then
            values(ij)=206.
         elseif(nint(values(ij)).eq.206) then
            values(ij)=205.
         elseif(nint(values(ij)).eq.201) then
            values(ij)=203.
         end if
         end do
C

      else
         CALL PBWRITE(IUNIT1,KBUFF,IKBUFL,IERR)
         IF(IERR.LT.0) THEN
         PRINT*,'ERROR WRITING INTO TARGET FILE.'
         CALL EXIT(2)
         end if
C
          NW=NW+1
C
          GO TO 300
      end if
C
C
C*          6.2 ENCODE DATA INTO BUFR MESSAGE.
C               ------------------------------
 620  CONTINUE
C
      IF(KSEC1(5).NE.0) THEN
         CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)
         IF(KERR.NE.0) THEN
            PRINT*,'BUUKEY: ERROR ',KERR
            CALL EXIT(2)
         END IF
      END IF
C
      IF(KSUP(6).EQ.0) THEN
         PRINT*,'ZERO SUBSETS'
         CALL EXIT(2)
      END IF
C
C     GET INFORMATION FOR RDB KEY
C
      ksec2(1)=52
      key(1)=52
      KEY(15)=nint(values(7))
C 
C     CALL BUCREKEY(KEL,KTDEXP,KSUP,KSEC1,KSEC3,KEY,
C    1              VALUES,CVALS,KERR)
C     IF(KERR.NE.0) THEN
C        PRINT*,'ERROR IN BUCREKEY.'
C        CALL EXIT(2)
C     END IF
C
C     PACK NEW RDB KEY
C
C
      CALL BUPKEY(KEY,KSEC1,KSEC2,KERR)
      IF(KERR.NE.0) CALL EXIT(2)
C
      KSEC3(4)=128
      IF(KSEC3(3).GT.1) KSEC3(4)=192
C
      CALL BUFREN( KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
     1             KTDLEN,KTDLST,KDLEN,KDATA,KEL,        !KSUP(5),
     2             KVALS,VALUES,CVALS,KBUFL,KBUFR,KERR)
C
      IF(KERR.GT.0) THEN
         PRINT*,'ERROR DURING ENCODING.'
         CALL EXIT(2)
      END IF
C
C           6.3 WRITE PACKED BUFR MESSAGE INTO FILE.
C               ------------------------------------
 630  CONTINUE
C
      IKBUFL=KBUFL*4
      CALL PBWRITE(IUNIT1,KBUFR,IKBUFL,IERR)
      IF(IERR.LT.0) THEN
         PRINT*,'ERROR WRITING INTO TARGET FILE.'
         CALL EXIT(2)
      END IF
C
      NW=NW+1
C
      GO TO 300
C     -----------------------------------------------------------------
C
 810  CONTINUE
C
      WRITE(*,'(1H ,A)') 'OPEN ERROR ON INPUT FILE'
      GO TO 900
C      
 800  CONTINUE
C
      IF(IRET.EQ.-1) THEN
         PRINT*,'NUMBER OF RECORDS PROCESSED ',N
         PRINT*,'NUMBER OF RECORDS CONVERTED ',NW
c
      ELSE
         PRINT*,' BUFR : ERROR= ',IERR
      END IF
C
 900  CONTINUE
C
      PRINT*,'NUMBER OF RECORDS PROCESSED ',N
      PRINT*,'NUMBER OF RECORDS CONVERTED ',NW
C
      CALL PBCLOSE(IUNIT,IRET)
 101  CONTINUE
C
      CALL PBCLOSE(IUNIT1,IRET)
C
      END
      SUBROUTINE CUTZERO(CHARPAR,KMIN)
C
C**** CUTZERO - SUBROUTINE TO REMOVE ZERO CHARACTERS.
C
C**   PURPOSE
C     -------
C
C     TO REMOVE ZERO-FILL CHARACTERS FROM THE END OF A CHARACTER
C     VARIABLE.
C
C     INTERFACE
C     ---------
C
C     CALL CUTZERO(CHARPAR,KMIN)
C
C          CHARPAR  - CHARACTER VARIABLE WHICH MAY HAVE ZEROS AT THE
C                     END OF THE VALID CHARACTERS, WHICH NEED TO BE
C                     REMOVED;
C                     RETURNED WITH THE ZERO CHARACTERS CONVERTED TO
C                     BLANK.
C
C          KMIN     - INTEGER VARIABLE INDICATING A MINIMUM NUMBER OF
C                     CHARACTERS AT THE BEGINNING OF THE STRING WHICH
C                     MUST NOT BE CHANGED
C
C     THUS:
C
C     CHARPAR='ABC0000'
C     CALL CUTZERO(CHARPAR,4)
C
C     WOULD RETURN THE VALUE 'ABC0   ' IN CHAR, AND WOULD NOT
C     ALTER THE FIRST 4 CHARACTERS.
C
C     METHOD
C     ------
C
C     THE STRIG IS TESTED FOR THE EXISTANCE OF A ZERO CHARACTER.
C     IF NONE IS FOUND, NO CHANGE TAKES PLACE.
C     IF ONE OR MORE ZERO CHARACTERS ARE PRESENT, THE END OF THE
C     STRING IS LOCATED. WORKING BACKWARDS FROM THE END TO THE
C     KMIN-1 POSITION, CHARACTERS ARE TESTED FOR ZERO. IF A ZERO
C     IS FOUND, IT IS REPLACED BY BLANK. IF A NON-ZERO IS FOUND,
C     THE REPLACEMENT LOOP TERMINATES.
C
C     MODIFICATIONS
C     -------------
C
C     ORIGINAL VERSION - 25.01.95 - REX GIBSON - ECMWF.
C
      CHARACTER*(*) CHARPAR
      CHARACTER*1   YZERO
      INTEGER LEN
C
C     -----------------------------------------------------------
C
C*     1.     FIND AND REPLACE THE ZERO CHARACTERS.
C
  100 CONTINUE
      YZERO=CHAR(0)
      I1=INDEX(CHARPAR,'0')
      IF (I1.GT.0) THEN
         I2=MAX(I1,KMIN+1)
         I3=INDEX(CHARPAR,' ')-1
         IF (I3.LE.0) THEN
            I3=LEN(CHARPAR)
         ENDIF
         DO 112 J=I3,I2,-1
         IF (CHARPAR(J:J).EQ.'0') THEN
             CHARPAR(J:J)=' '
         ELSEIF (CHARPAR(J:J).EQ.YZERO) THEN
             GO TO 112
         ELSE
             GO TO 114
         ENDIF
  112    CONTINUE
C
  114    CONTINUE
      ENDIF
C
C     -----------------------------------------------------------
C
C*     2.     RETURN.
C
  200 CONTINUE
C
      END
