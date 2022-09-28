      PROGRAM BUFRNTM
C
C**** *BUFRNTM*
C
C
C     PURPOSE.
C     --------
C        CONVERT BUFR EDITION N TO BUFR EDITION M MESSAGES.
C        BY DEFAULT CONVERSION WILL BE THE LATEST.
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
C          M. DRAGOSAVAC    *ECMWF*       15/09/94
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
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2=4096 ,JSEC3= 4,
     1       JSEC4=   2,JELEM=160000,JSUBS=400,JCVAL=150 ,JBUFL=512000,
#ifdef JBPW_64
     2          JBPW =  64,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200,
#else
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200,
#endif
     3          JWORK=4096000,JKEY=46,JBYTE=512000)
C
      PARAMETER (KELEM=160000)
      PARAMETER (KVALS=4096000)
      PARAMETER (ISUBEN=5)
C 
      DIMENSION KBUFF(JBUFL)
      DIMENSION KBUFR(JBUFL)
      DIMENSION KSUP(JSUP),KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION KSEC1_TEMP(JSEC1+2)
      DIMENSION KSEC2(JSEC2),KSEC3(JSEC3),KSEC4(JSEC4)
      DIMENSION KEY  (JKEY)
C
      REAL*8  VALUES(KVALS),VALUE(KVALS),VALS(KVALS)
      REAL*8  RVIND,EPS
      DIMENSION KTDLST(KELEM),KTDEXP(KELEM)
      DIMENSION KTDLST1(KELEM),KTDEXP1(KELEM)
      DIMENSION KDATA(200)
      DIMENSION ISUBTYPES(ISUBEN)
C
      CHARACTER*256 CF,CF1
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
      CHARACTER*80 CVAL(KVALS)
      CHARACTER*80 YENC
      CHARACTER*9  CSHIP
      CHARACTER*256 CINFILE,COUTFILE,CARG(10)
      CHARACTER*2 CEDITION,CBMTVN,CVNMTU,CVNLTU
C
      DATA ISUBTYPES/2,4,12,14,104/
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
      print*,'The following subtypes will be skipped by bufr_ntm:'
      print*,'2,4,12,14,104'
c
      RVIND=1.7D38
      EPS=10.D-8
      NREAD=0 
      NBYTPW=JBPW/8
      N=0
      IERROR=0
C
C     GET INPUT AND OUTPUT FILE NAME.
C
      NARG=IARGC()
C
      IF(NARG.LT.4) THEN
          PRINT*,'USAGE -- bufr_ntm -i infile -o outfile'
          PRINT*,'OTHER OPTIONS:'
          PRINT*,'               -e BUFR EDITION NUMBER'
          PRINT*,'               -f VERSION NUMBER OF MASTER TABLE USED'
          PRINT*,'               -g VERSION NUMBER OF LOCAL TABLE USED'
          PRINT*,'               -h TOLERATED NUMBER OF ERRORS'
          PRINT*,'               DEFAULT - LAST AVAILABLE'
          call exit(2)
      END IF
C
      DO 101 J=1,NARG
      CALL GETARG(J,CARG(J))
 101  CONTINUE
C
      CINFILE=' '
      COUTFILE=' '
      CEDITION=' '
      CVNMTU=' '
      CVNLTU=' '
      CERROR=' '
C
      DO 102 J=1,NARG,2
      IF(CARG(J).EQ.'-i') THEN
         CINFILE=CARG(J+1)
      ELSEIF(CARG(J).EQ.'-o') THEN
         COUTFILE=CARG(J+1)
      ELSEIF(CARG(J).EQ.'-e') THEN
         CEDITION=CARG(J+1)
      ELSEIF(CARG(J).EQ.'-f') THEN
         CVNMTU=CARG(J+1)
      ELSEIF(CARG(J).EQ.'-g') THEN
         CVNLTU=CARG(J+1)
      ELSEIF(CARG(J).EQ.'-h') THEN
         CERROR=CARG(J+1)
      ELSE
          PRINT*,'USAGE -- bufr_ntm  -i infile -o outfile'
          PRINT*,'OTHER OPTIONS:'
          PRINT*,'               -e BUFR EDITION NUMBER'
          PRINT*,'               -f VERSION NUMBER OF MASTER TABLE USED'
          PRINT*,'               -g VERSION NUMBER OF LOCAL TABLE USED'
          PRINT*,'               -h TOLERATED NUMBER OF ERRORS'
          PRINT*,'               DEFAULT - LAST AVAILABLE'
          CALL EXIT(2)
      END IF
 102  CONTINUE
C
      IZ=INDEX(CEDITION,' ')
      IF(IZ.EQ.1) THEN
C
C        THIS IS THE LAST AVAILABLE BUFR EDITION NUMBER
C
         IBED=3
         PRINT*,'DEFAULT EDITION NUMBER                 -->',IBED
      ELSE
         READ(CEDITION,'(I2)') IBED
      END IF
      IZ=INDEX(CVNMTU,' ')
      IF(IZ.EQ.1) THEN
C
C        THIS IS THE LAST AVAILABLE VERSION NUMBER OF MASTER TABLE USED
C
         IVNMTU=13
         PRINT*,'DEFAULT VERSION OF MASTER TABLES USED  -->',IVNMTU
      ELSE
         READ(CVNMTU,'(I2)') IVNMTU
      END IF
C
      IZ=INDEX(CVNLTU,' ')
      IF(IZ.EQ.1) THEN
C   
C        THIS IS THE LAST AVAILABLE VERSION NUMBER OF LOCAL TABLE USED
C
         IVNLTU=1
         PRINT*,'DEFAULT VERSION OF LOCAL TABLES USED   -->',IVNLTU
      ELSE
         READ(CVNLTU,'(I2)') IVNLTU
      END IF
C
      IZ=INDEX(CERROR,' ')
      IF(IZ.EQ.1) THEN
         NERROR=0
      ELSE    
         READ(CERROR,'(I3)') NERROR
      END IF
C
      II=INDEX(CINFILE,' ')
      JJ=INDEX(COUTFILE,' ')
      IF(II.EQ.1.OR.JJ.EQ.1) THEN
          PRINT*,'USAGE -- BUFR_NTM  -I INFILE -O OUTFILE'
          PRINT*,'OTHER OPTIONS:'
          PRINT*,'               -E BUFR EDITION NUMBER'
          PRINT*,'               -F VERSION NUMBER OF MASTER TABLE USED'
          PRINT*,'               -G VERSION NUMBER OF LOCAL TABLE USED'
          PRINT*,'               -h TOLERATED NUMBER OF ERRORS'
          PRINT*,'               DEFAULT - LAST AVAILABLE'
          CALL EXIT(2)
      END IF
      II=II-1
      JJ=JJ-1
      KRQL=0
      NW=0
      NR=0
C
C*          1.2 OPEN FILE CONTAINING BUFR DATA.
C               -------------------------------
 120  CONTINUE
C
      IRET=0 
      CALL PBOPEN(IUNIT,CINFILE(1:II),'R',IRET)
      IF(IRET.EQ.-1) then
         PRINT*,'OPEN FAILED'
         CALL EXIT(2)
      END IF
      IF(IRET.EQ.-2) THEN
         PRINT*,'INVALID FILE NAME'
         CALL EXIT(2)
      END IF
      IF(IRET.EQ.-3) THEN
        PRINT*,'INVALID OPEN MODE SPECIFIED'
        CALL EXIT(2)
      END IF
C
      CALL PBOPEN(IUNIT1,COUTFILE(1:JJ),'W',IRET)
      IF(IRET.EQ.-1) THEN
         PRINT*,'OPEN FAILED ON BUFR.DAT'
         CALL EXIT(2)
      END IF
      IF(IRET.EQ.-2) THEN
         PRINT*,'INVALID FILE NAME'
         CALL EXIT(2)
      END IF
      IF(IRET.EQ.-3) THEN
         PRINT*,'INVALID OPEN MODE SPECIFIED'
         CALL EXIT(2)
      END IF
C
C     ----------------------------------------------------------------- 
C*          2. SET REQUEST FOR EXPANSION.
C              --------------------------
 200  CONTINUE
C
      OPRT=.FALSE.
      ICODE=0
      OENC=.TRUE.
      OCOMP=.FALSE.
      NR=1
C
 201  CONTINUE
C
      OSEC3=.FALSE.
C
C*          2.1 SET REQUEST FOR PARTIAL EXPANSION.
C               ----------------------------------
 210  CONTINUE
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
          PRINT*,'NUMBER OF ORIGINAL  MESSAGES  ',NREAD
          PRINT*,'NUMBER OF CONVERTED MESSAGES  ',NW
          GO TO 900
      END IF
      IF(IRET.EQ.-2) THEN
         PRINT*,'FILE HANDLING PROBLEM' 
         CALL EXIT(2)
      END IF
      IF(IRET.EQ.-3) THEN
         PRINT*,'ARRAY TOO SMALL FOR PRODUCT'
         CALL EXIT(2)
      END If
C
      NREAD=NREAD+1
      N=N+1
      NBYTES=KBUFL
      KBUFL=KBUFL/NBYTPW+1
      IF(NREAD.LT.NR) GO TO 300
C
C     -----------------------------------------------------------------
C*          4. EXPAND BUFR MESSAGE.
C              --------------------
 400  CONTINUE
C
      KERR=0
      CALL BUS012(KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KSEC2,KERR)
      IF(KERR.NE.0) THEN
         PRINT*,'ERROR IN BUS012: ',KERR
         PRINT*,' BUFR MESSAGE NUMBER ',N,' CORRUPTED.'
         IERROR=IERROR+1
         IF(IERROR.GT.NERROR)  CALL MY_EXIT(2)
         GO TO 300
      END IF
C
C     CHECK IF SUBTYPE NEEDED FOR ANALYSIS
C
      DO IS=1,ISUBEN   
      IF(KSEC1(7).EQ.ISUBTYPES(IS)) GO TO 300
      END DO
C
      IERR=0
      KEL=KELEM
      IF(KSUP(6).GT.1) THEN
         KEL=KVALS/KSUP(6)
         IF(KEL.GT.KELEM) KEL=KELEM
      END IF
C
      CALL BUFREX(KBUFL,KBUFF,KSUP,KSEC0 ,KSEC1,KSEC2 ,
     1            KSEC3 ,KSEC4,KEL,CNAMES,CUNITS,KVALS,
     2            VALUES,CVALS,IERR)
C
      IF(IERR.NE.0)  THEN
         PRINT*,'BUFREX: ERROR ', IERR
         IERROR=IERROR+1
         IF(IERROR.GT.NERROR)  CALL MY_EXIT(2)
         GO TO 300
      END IF
C
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
      IF(KERR.NE.0) THEN
       PRINT*,'BUSEL : ERROR ',KERR
       CALL MY_EXIT(2)
      END IF
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
C     -----------------------------------------------------------------
C*          5. COLLECT DATA FOR REPACKING.
C              ---------------------------
 500  CONTINUE
C      
      IF(.NOT.OENC) GO TO 300
C
C               FIRST GET DATA DESCRIPTORS
C
      KERR=0
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
      IF(KERR.NE.0) THEN
         PRINT*,'BUSEL : ERROR ',KERR
         CALL MY_EXIT(2)
      END IF
C
      KERR=0
      CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)
      IF(KERR.NE.0) THEN
         PRINT*,'BUUKEY:ERROR ',KERR
         CALL MY_EXIT(2)
      END IF
C
C     ERS1 DATA ORIGINATING CENTRE IS 97
C
      KSEC1(3)=98
C
C     HIGH RESOLUTION TOVS 120 KM GOT WRONG SUBTYPE
C     AND RDB TYPE
C
      ISUBTYPE=0
      IF(KSEC1(2).EQ.0.AND.KSEC1(6).EQ.3.AND.KSEC1(7).EQ.0) THEN
         KSEC1(3)=98
         KSEC1(7)=51
         KSEC1(6)=3
         KEY(2)=2
         KEY(3)=51
         ISUBTYPE=51
      END IF
C
C     IF HIGH LEVEL STATION CHANGE SUBTYPE TO 5 LOCALLY
C
      IF(KTDLST(1).EQ.307007) THEN
         KSEC17= KSEC1(7)
         KSEC1(7)=5
      END IF
C
C     SOME SHIP CALL SIGNS HAVE ZERO APPEND
C
      IF(KSEC1(7).GE.11.AND.KSEC1(7).LE.14.OR.KSEC1(7).EQ.19.OR.
     1   KSEC1(7).EQ.92.OR.KSEC1(7).EQ.102) THEN
         IF(CVALS(1)(9:9).EQ.'0') THEN
            CSHIP=' '
            CALL CUTZERO(CVALS(1),1)
            DO 505 IC=1,9
            CSHIP(IC:IC)=CHAR(KEY(IC+15))
 505        CONTINUE
            CALL CUTZERO(CSHIP,1)
            DO 506 IC=1,9
            KEY(IC+15)=ICHAR(CSHIP(IC:IC))
 506        CONTINUE 
         END IF
      END IF
C
      IF(KSEC1(7).GE.132.AND.KSEC1(7).LE.133) THEN
          IF(CVALS(1)(9:9).EQ.'0') THEN
             CSHIP=' '
             CALL CUTZERO(CVALS(1),1)
             DO 507 IC=1,9
             CSHIP(IC:IC)=CHAR(KEY(IC+15))
 507         CONTINUE
             CALL CUTZERO(CSHIP,1)
             DO 508 IC=1,9
             KEY(IC+15)=ICHAR(CSHIP(IC:IC))
 508         CONTINUE
          END IF
      END IF
C
      IERR=0
      if(KSEC1(12).eq.24) then
         print*,'Date hour is 24'
      end if
      CALL DATE_TIME_CHECK(KTDEXP,KSEC1,KSEC3,KEY,KEL,VALUES,IERR)
      IF(IERR.NE.0) THEN
         IERR=0
         GO TO 300
      END IF

      KERR=0
C
C            EDITION 0,1 TO >=2
C
      IF(KSEC0(3).LE.1.AND.IBED.GT.1) THEN
         CALL BUCON0(KSEC1(7),KSUP,KTDLEN,KTDLST,
     1              KTDLST1,KTDEXL,KTDEXP,KEL,KVALS,
     2              VALUES,VALUE,KDLEN,KDATA,KERR)
C
C            EDITION 2 TO 2
C
      ELSEIF(KSEC0(3).EQ.2.AND.IBED.EQ.2) THEN
         CALL BUCON3(KSEC1(7),KSUP,KTDLEN,KTDLST,
     1               KTDLST1,KTDEXL,KTDEXP,KEL,KVALS,
     2               VALUES,VALUE,KDLEN,KDATA,KERR)
C
C            EDITION 2 TO 3
C
      ELSEIF(KSEC0(3).EQ.2.AND.IBED.EQ.3) THEN
         CALL BUCON3(KSEC1(7),KSUP,KTDLEN,KTDLST,
     1               KTDLST1,KTDEXL,KTDEXP,KEL,KVALS,
     2               VALUES,VALUE,KDLEN,KDATA,KERR)
C
C            EDITION 3 TO 2
C
      ELSEIF(KSEC0(3).EQ.3.AND.IBED.EQ.2) THEN
         CALL BUCON3(KSEC1(7),KSUP,KTDLEN,KTDLST,
     1               KTDLST1,KTDEXL,KTDEXP,KEL,KVALS,
     2               VALUES,VALUE,KDLEN,KDATA,KERR)
C
C            EDITION 3 TO 3
C
      ELSEIF(KSEC0(3).EQ.3.AND.IBED.EQ.3) THEN
         CALL BUCON3(KSEC1(7),KSUP,KTDLEN,KTDLST,
     1               KTDLST1,KTDEXL,KTDEXP,KEL,KVALS,
     2               VALUES,VALUE,KDLEN,KDATA,KERR)
      ELSE
         PRINT*,'THIS CONVERSION IS NOT SUPPORTED.'
         CALL MY_EXIT(2)
      END IF
C
      IF(KERR.NE.0) THEN
         PRINT*,'BAD CONVERSION FOR SUBTYPE -',KSEC1(7)
         PRINT*,'MESSAGE NUMBER IS ',N
         CALL MY_EXIT(2)
      END IF
C
C     CREATE RDB KEY FOR TOVS 120 km
C
      if(ksec1(7).eq.51) then
         if(isubtype.eq.51) then
           do iii=26,46
           key(iii)=0
           end do
           key(46)=70
         end if
         call BUCRKEY(KEL,KTDEXP,KSUP,KSEC1,KSEC3,KEY,
     1                     VALUE,CVALS,KERR)
      end if
C
C     CHANGE BACK SYNOP HIGH LEVEL STATION SUBTYPE
C
      IF(KSEC1(7).EQ.5) KSEC1(7)=KSEC17
C
C     FIX old drop sondes
C
      if(ksec1(7).eq.103.and.ksec0(3).eq.0) then
         if(key(4).gt.2000) then
            ksec1( 9)=key(4)-2000
         else   
            ksec1( 9)=key(4)-1900.
         end if
         ksec1(10)=key(5)
         ksec1(11)=key(6)
         ksec1(12)=key(7)
         ksec1(13)=key(8)
         value( 6)=key(4)
         value( 7)=key(5)
         value( 8)=key(6)
         value( 9)=key(7)
         value(10)=key(8)
         value(11)=(key(11)-9000000)/100000.
         value(12)=(key(10)-18000000)/100000.
         cvals(1)=char(key(16))//char(key(17))//
     1            char(key(18))//char(key(19))//
     2            char(key(20))//char(key(21))//
     3            char(key(22))//char(key(23))//
     4            char(key(24))
      end if
C
      if(ksec1(7).eq.91) then
c        some pilots got hour diff in the key and sec1 
         key(4)=int(values(5))
         key(5)=int(values(6))
         key(6)=int(values(7))
         key(7)=int(values(8))
         key(8)=int(values(9))
      end if
c
c     Fix oceanographic data 131,132,133
c
      if(ksec1(7).ge.131.and.ksec1(7).le.133) then
c        some section 1 dates are wrong
         if(key(4).gt.2000) then
            ksec1( 9)=key(4)-2000
         else   
            ksec1( 9)=key(4)-1900.
         end if
         ksec1(10)=key(5)
         ksec1(11)=key(6)
         ksec1(12)=key(7)
         ksec1(13)=key(8)

      end if

C
C     -----------------------------------------------------------------
C*          6. PACK BUFR MESSAGE BACK INTO BUFR.
C              ---------------------------------
 600  CONTINUE
C
    
      KKK=0
      KSEC1(2)=IBED
      KSEC1(8)=IVNLTU
      KSEC1(15)=IVNMTU
      KSEC1(3)=98
      if(ksec1(1).gt.18.and.KSEC0(3).le.2) then
         do iii=1,15
          ksec1_temp(iii)=ksec1(iii)
         end do
C
         ksec1_temp(16)=0   ! Originating sub-centre
         ksec1_temp(17)=0
C
C        move for 2 bytes local info
C
         do iii=16,jsec1
         ksec1_temp(iii+2)=ksec1(iii)
         end do
C
         do iii=1,jsec1
         ksec1(iii)=ksec1_temp(iii)
         end do
C
         ksec1(1)=ksec1(1) 
      else
         KSEC1(16)=0
      end if
C
      KSEC0(3)=IBED
C     KSEC3(4)=128            ! NO COMPRESSION
C     IF(KSEC3(3).GT.1) KSEC3(4)=192
C
C*          6.2 ENCODE DATA INTO BUFR MESSAGE.
C               ------------------------------
 620  CONTINUE
C
      KERR=0
      KEY(1)=52
      KSEC1(14)=0
      KSEC2(1)=52
      KERR=0
      CALL BUPKEY(KEY,KSEC1,KSEC2,KERR)
      IF(KERR.NE.0) THEN
        PRINT*,'BUPKEY: ERROR',KERR
        CALL MY_EXIT(2)
      END IF
C
      KERR=0
      KBUFL=JBUFL
      CALL BUFREN( KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
     1             KTDLEN,KTDLST1,KDLEN,KDATA,KEL,
     2             KVALS,VALUE,CVALS,KBUFL,KBUFR,KERR)
C
      IF(KERR.GT.0) THEN
         PRINT*,'MESSAGE NUMBER ',N
         PRINT*,'BUFR SUBTYPE ',KSEC1(7)
         IERROR=IERROR+1
         IF(IERROR.GT.NERROR) CALL MY_EXIT(2)
         GO TO 300
      END IF
C
C           6.3 WRITE PACKED BUFR MESSAGE INTO FILE.
C               ------------------------------------
 630  CONTINUE
C
      ILEN=KBUFL*NBYTPW
      KBUFL=0
      CALL PBWRITE(IUNIT1,KBUFR,ILEN,IRET)
      IF(IRET.EQ.-1) THEN
         PRINT*,'ERROR DURING WRITE INTO OUTPUT FILE.'
         CALL MY_EXIT(2)
      END IF
      IRET=0
C
      NW=NW+1
C
      GO TO 300
C     -----------------------------------------------------------------
C
C      
 900  CONTINUE
C
      CALL PBCLOSE(IUNIT,IRET)
      CALL PBCLOSE(IUNIT1,IRET)
C
      END
      SUBROUTINE BUCON0(KST,KSUP,KTDLEN,KTDLST,KTDLST1,KTDEXL,KTDEXP,
     1                  KELEM,KVALS,VALUES,VALUE,KDLEN,KDATA,KERR)
C
C**** *BUCON0*
C
C
C     PURPOSE.
C     --------
C         CONVERTS BUFR EDITION 0 TO EDITION 2 DATA.
C         
C
C
C**   INTERFACE.
C     ----------
C
C              *CALL BUCONV(KST,KTDLEN,KTDLST,KTDLST1,KTDEXL,KTDEXP,
C                           KELEM,KVALS,VALUES,VALUE,KERR)*
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
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/09/94
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
      DIMENSION KTDLST(*),KTDEXP(*)
      DIMENSION KTDLST1(*),KSUP(*),KDATA(*)
      REAL*8 VALUES(*),VALUE(*)
      REAL*8 RVIND,EPS
C
C     ------------------------------------------------------------------
C*          1. INITIALIZE CONSTANTS AND VARIABLES.
C              -----------------------------------
 100  CONTINUE
C
C     MISSING VALUE INDICATOR
C
      RVIND=1.7D38
      EPS=10.D-8
C
      IF(KST.EQ.1.OR.KST.EQ.3.OR.KST.EQ.5) THEN
C
C        SYNOP LAND SUBTYPES 1 AND 3 AND 5 HIGH LEVEL STATION
C
C
C        CHANGE OPERATOR
C
         J=0
C
         DO 101 I=1,KTDLEN
C
         IF(KTDLST(I).GT.262000.AND.KTDLST(I).LT.300000) THEN
            J=J+1
            KTDLST1(J)=222000
         ELSEIF(KTDLST(I).EQ.262000) THEN
               GO TO 101
         ELSEIF(KTDLST(I).GT.101000.AND.KTDLST(I).LE.101049) THEN
            J=J+1
            IF(KST.LE.3) KTDLST1(J)=101049
            IF(KST.EQ.5) KTDLST1(J)=101033
         ELSEIF(KTDLST(I).EQ.063191) THEN
            J=J+1
            KTDLST1(J)=001201
         ELSEIF(KTDLST(I).EQ.063192) THEN
            J=J+1
            KTDLST1(J)=033007
         ELSEIF(KTDLST(I).EQ.031192) THEN
            J=J+1
            KTDLST1(J)=031031
            J=J+1
            KTDLST1(J)=001031
         ELSE
            J=J+1
            KTDLST1(J)=KTDLST(I)
         END IF
C
 101     CONTINUE
C
         KTDLEN=J
C
C        FIND FIRST AND LAST POINTER TO DATA PRESENT INDICATOR
C
         ODPF=.FALSE.
C
         DO 102 I=1,KTDEXL
         IF(KTDEXP(I).EQ.31192) THEN
            IF(.NOT.ODPF) IDPF=I
            ODPF=.TRUE.
         ELSE
            IF(ODPF) THEN
               IDPL=I-1
               GO TO 103
            END IF
         END IF
 102     CONTINUE
C
 103     CONTINUE
C
C        FIND START OF % CONFIDENCE
C
         DO 104 I=IDPL,KTDEXL
         IF(KTDEXP(I).EQ.63192) THEN
            IPCF=I
            GO TO 105
         END IF
 104     CONTINUE
C
 105     CONTINUE
C
C        MAKE DATA SUITABLE FOR COMPRESSION
C
         J=0
         KDLEN=0
         IPC=IPCF-1
C
         DO 106 I=1,KTDEXL
         J=J+1
         IF(KTDEXP(I).EQ.031001.OR.KTDEXP(I).EQ.031002) THEN
            KDLEN=KDLEN+1
            KDATA(KDLEN)=NINT(VALUES(I))
            GO TO 106
         END IF
C
         IF(KTDEXP(I).EQ.063191) THEN    ! GENERATING APPLICATION
            VALUE(J)=98.                 ! GENERATING CENTRE
            J=J+1
            VALUE(J)=1.
            GO TO 106
         END IF
C
         IF(KTDEXP(I).EQ.031192) THEN
            VALUE(J)=0.    ! DATA PRESENT
            GO TO 106
         END IF
C
         IF(KTDEXP(I).EQ.063192) THEN            ! % CONF
            DO 107 K=IDPF,IDPL
            IF(VALUES(K).GT.0.) THEN
               VALUE(J)=RVIND
               J=J+1
            ELSE
               IPC=IPC+1
               VALUE(J)=VALUES(IPC)
               J=J+1
            END IF
 107        CONTINUE
            GO TO 110
         END IF
C
         IF(KTDEXP(I).GE.13019.AND.KTDEXP(I).LE.13023) THEN
            IF(ABS(VALUES(I)-RVIND)/RVIND.LT.EPS) THEN
               VALUE(J)=VALUES(I)
            ELSE
               VALUE(J)=VALUES(I)*1000.
            END IF
         ELSE
            VALUE(J)=VALUES(I)
         END IF
C
 106     CONTINUE
C
 110     CONTINUE
C
         KERR=0
         RETURN
      END IF
C
      IF(KST.EQ.2.OR.KST.EQ.4) THEN
C
C        SYNOP LAND SECOND RECORD 2,4
C
C        CHANGE OPERATOR
C
         J=0
C
         DO 901 I=1,KTDLEN
C
         IF(KTDLST(I).GT.262000.AND.KTDLST(I).LT.300000) THEN
            J=J+1
            KTDLST1(J)=222000
         ELSEIF(KTDLST(I).EQ.262000) THEN
               GO TO 901
         ELSEIF(KTDLST(I).GT.101000.AND.KTDLST(I).LE.101022) THEN
            J=J+1
            IF(KST.EQ.2) KTDLST1(J)=101022
            IF(KST.EQ.4) KTDLST1(J)=101022
         ELSEIF(KTDLST(I).EQ.063191) THEN
            J=J+1
            KTDLST1(J)=001201
         ELSEIF(KTDLST(I).EQ.063192) THEN
            J=J+1
            KTDLST1(J)=033007
         ELSEIF(KTDLST(I).EQ.031192) THEN
            J=J+1
            KTDLST1(J)=031031
            J=J+1
            KTDLST1(J)=001031
         ELSEIF(KTDLST(I).EQ.14011) THEN
            J=J+1
            KTDLST1(J)=14015
         ELSEIF(KTDLST(I).EQ.14032) THEN
            J=J+1
            KTDLST1(J)=14031
         ELSEIF(KTDLST(I).EQ.20063) THEN
            J=J+1
            KTDLST1(J)=20192
         ELSE
            J=J+1
            KTDLST1(J)=KTDLST(I)
         END IF
C
 901     CONTINUE
C
         KTDLEN=J
C
C        FIND FIRST AND LAST POINTER TO DATA PRESENT INDICATOR
C
         ODPF=.FALSE.
C
         DO 902 I=1,KTDEXL
         IF(KTDEXP(I).EQ.31192) THEN
            IF(.NOT.ODPF) IDPF=I
            ODPF=.TRUE.
         ELSE
            IF(ODPF) THEN
               IDPL=I-1
               GO TO 903
            END IF
         END IF
 902     CONTINUE
C
 903     CONTINUE
C
C        FIND START OF % CONFIDENCE
C
         DO 904 I=IDPL,KTDEXL
         IF(KTDEXP(I).EQ.63192) THEN
            IPCF=I
            GO TO 905
         END IF
 904     CONTINUE
C
 905     CONTINUE
C
C        MAKE DATA SUITABLE FOR COMPRESSION
C
         J=0
         KDLEN=0
         IPC=IPCF-1
C
         DO 906 I=1,KTDEXL
         J=J+1
         IF(KTDEXP(I).EQ.031001.OR.KTDEXP(I).EQ.031002) THEN
            KDLEN=KDLEN+1
            KDATA(KDLEN)=NINT(VALUES(I))
         END IF
C
         IF(KTDEXP(I).EQ.063191) THEN    ! GENERATING APPLICATION
            VALUE(J)=98.                 ! GENERATING CENTRE
            J=J+1
            VALUE(J)=1.
            GO TO 906
         END IF
C
         IF(KTDEXP(I).EQ.031192) THEN
            VALUE(J)=0.    ! DATA PRESENT
            GO TO 906
         END IF
C
         IF(KTDEXP(I).EQ.063192) THEN            ! % CONF
            DO 907 K=IDPF,IDPL
            IF(VALUES(K).GT.0.) THEN
               VALUE(J)=RVIND
               J=J+1
            ELSE
               IPC=IPC+1
               VALUE(J)=VALUES(IPC)
               J=J+1
            END IF
 907        CONTINUE
            GO TO 910
         END IF
C
         IF(KTDEXP(I).GE.13019.AND.KTDEXP(I).LE.13023) THEN
            IF(ABS(VALUES(I)-RVIND)/RVIND.LT.EPS) THEN
               VALUE(J)=VALUES(I)
            ELSE
               VALUE(J)=VALUES(I)*1000.
            END IF
         ELSE
            VALUE(J)=VALUES(I)
         END IF
C
 906     CONTINUE
C
 910     CONTINUE
C
         KERR=0
         RETURN
      END IF
C
      IF(KST.EQ.09.OR.KST.EQ.11.OR.
     1   KST.EQ.13.OR.KST.EQ.19) THEN
C
C        SYNOP SHIP 9,11,13,19
C        ---------------------
C
C        CHANGE OPERATOR
C
         J=0
C
         DO 201 I=1,KTDLEN
C
         IF(KTDLST(I).GT.262000.AND.KTDLST(I).LT.300000) THEN
            J=J+1
            KTDLST1(J)=222000
         ELSEIF(KTDLST(I).EQ.262000) THEN
               GO TO 201
         ELSEIF(KTDLST(I).GT.101000.AND.KTDLST(I).LT.101033) THEN
            J=J+1
            KTDLST1(J)=101033
         ELSEIF(KTDLST(I).EQ.063191) THEN
            J=J+1
            KTDLST1(J)=001201
         ELSEIF(KTDLST(I).EQ.063192) THEN
            J=J+1
            KTDLST1(J)=033007
         ELSEIF(KTDLST(I).EQ.031192) THEN
            J=J+1
            KTDLST1(J)=031031
            J=J+1
            KTDLST1(J)=001031
         ELSE
            J=J+1
            IF(KTDLST(I).EQ.308004) THEN
               KTDLST1(J)=KTDLST(I)
               J=J+1
               KTDLST1(J)=12005
            ELSE
               KTDLST1(J)=KTDLST(I)
            END IF
         END IF
C
 201     CONTINUE
C
         KTDLEN=J
C
C
C        FIND FIRST AND LAST POINTER TO DATA PRESENT INDICATOR
C
         ODPF=.FALSE.
C
         DO 202 I=1,KTDEXL
         IF(KTDEXP(I).EQ.31192) THEN
            IF(.NOT.ODPF) IDPF=I
            ODPF=.TRUE.
         ELSE
            IF(ODPF) THEN
               IDPL=I-1
               GO TO 203
            END IF
         END IF
 202     CONTINUE
C
 203     CONTINUE
C
C        FIND START OF % CONFIDENCE
C
         DO 204 I=IDPL,KTDEXL
         IF(KTDEXP(I).EQ.63192) THEN
            IPCF=I
            GO TO 205
         END IF
 204     CONTINUE
C
 205     CONTINUE
C
C        MAKE DATA SUITABLE FOR COMPRESSION
C
         J=0
         KDLEN=0
         IPC=IPCF-1
C
         DO 206 I=1,KTDEXL
         J=J+1
         IF(KTDEXP(I).EQ.031001.OR.KTDEXP(I).EQ.031002) THEN
            KDLEN=KDLEN+1
            KDATA(KDLEN)=NINT(VALUES(I))
         END IF
C
         IF(KTDEXP(I).EQ.063191) THEN    ! GENERATING APPLICATION
            VALUE(J)=0.                  ! ONE MORE DATA PRESENT INDICATOR
            J=J+1 
            VALUE(J)=98.                 ! GENERATING CENTRE
            J=J+1
            VALUE(J)=1.
            GO TO 206
         END IF
C
         IF(KTDEXP(I).EQ.031192) THEN
            VALUE(J)=0.    ! DATA PRESENT
            GO TO 206
         END IF
C
         IF(KTDEXP(I).EQ.063192) THEN            ! % CONF
            DO 207 K=IDPF,IDPL
            IF(VALUES(K).GT.0.) THEN
               VALUE(J)=RVIND
               J=J+1
            ELSE
               IPC=IPC+1
               VALUE(J)=VALUES(IPC)
               J=J+1
            END IF
 207        CONTINUE
            VALUE(J)=RVIND
            GO TO 210
         END IF
C
         IF(KTDEXP(I).EQ.022042) THEN
            VALUE(J)=VALUES(I)
C
C           ADD 012005 DEW POINT TEMPERATURE AT 2M
C
            J=J+1
            VALUE(J)=RVIND
         ELSE
            VALUE(J)=VALUES(I)
         END IF
C
 206     CONTINUE
C
 210     CONTINUE
C
         KERR=0
         RETURN
      END IF
      IF(KST.EQ.12.OR.KST.EQ.14) THEN
C
C         SYNOP SHIP SECOND RECORD
C
C        CHANGE OPERATOR
C
         J=0
C
         DO 701 I=1,KTDLEN
C
         IF(KTDLST(I).GT.262000.AND.KTDLST(I).LT.300000) THEN
            J=J+1
            KTDLST1(J)=222000
         ELSEIF(KTDLST(I).EQ.262000) THEN
               GO TO 701
         ELSEIF(KTDLST(I).GT.101000.AND.KTDLST(I).LE.101039) THEN
            J=J+1
            IF(KST.EQ.12) KTDLST1(J)=101039
            IF(KST.EQ.14) KTDLST1(J)=101039
         ELSEIF(KTDLST(I).EQ.063191) THEN
            J=J+1
            KTDLST1(J)=001201
         ELSEIF(KTDLST(I).EQ.063192) THEN
            J=J+1
            KTDLST1(J)=033007
         ELSEIF(KTDLST(I).EQ.031192) THEN
            J=J+1
            KTDLST1(J)=031031
            J=J+1
            KTDLST1(J)=001031
         ELSEIF(KTDLST(I).EQ.13031) THEN
            J=J+1
            KTDLST1(J)=22001
            J=J+1
            KTDLST1(J)=22011
            J=J+1
            KTDLST1(J)=22021
            J=J+1
            KTDLST1(J)=13031
         ELSEIF(KTDLST(I).EQ.14011) THEN
            J=J+1
            KTDLST1(J)=14015
         ELSEIF(KTDLST(I).EQ.14032) THEN
            J=J+1
            KTDLST1(J)=14031
         ELSEIF(KTDLST(I).EQ.20063) THEN
            J=J+1
            KTDLST1(J)=20192
         ELSE
            J=J+1
            KTDLST1(J)=KTDLST(I)
         END IF
C
 701     CONTINUE
C
         KTDLEN=J
C
C        FIND FIRST AND LAST POINTER TO DATA PRESENT INDICATOR
C
         ODPF=.FALSE.
C
         DO 702 I=1,KTDEXL
         IF(KTDEXP(I).EQ.31192) THEN
            IF(.NOT.ODPF) IDPF=I
            ODPF=.TRUE.
         ELSE
            IF(ODPF) THEN
               IDPL=I-1
               GO TO 703
            END IF
         END IF
 702     CONTINUE
C
 703     CONTINUE
C
C        FIND START OF % CONFIDENCE
C
         DO 704 I=IDPL,KTDEXL
         IF(KTDEXP(I).EQ.63192) THEN
            IPCF=I
            GO TO 705
         END IF
 704     CONTINUE
C
 705     CONTINUE
C
C        MAKE DATA SUITABLE FOR COMPRESSION
C
         J=0
         KDLEN=0
         ISTAT=0
         IPC=IPCF-1
C
         DO 706 I=1,KTDEXL
         J=J+1
         IF(KTDEXP(I).EQ.031001.OR.KTDEXP(I).EQ.031002) THEN
            KDLEN=KDLEN+1
            KDATA(KDLEN)=NINT(VALUES(I))
         END IF
C
         IF(KTDEXP(I).EQ.063191) THEN    ! GENERATING APPLICATION
            VALUE(J)=98.                 ! GENERATING CENTRE
            J=J+1
            VALUE(J)=1.
            GO TO 706
         END IF
C
         IF(KTDEXP(I).EQ.031192) THEN
            ISTAT=ISTAT+1
            IF(ISTAT.EQ.23) THEN
               VALUE(J)=0.
               J=J+1
               VALUE(J)=0.
               J=J+1
               VALUE(J)=0.
               J=J+1
               VALUE(J)=0.
               GO TO 706
            END IF
            VALUE(J)=0.    ! DATA PRESENT
            GO TO 706
         END IF
C
         IF(KTDEXP(I).EQ.063192) THEN            ! % CONF
            IC=0
            DO 707 K=IDPF,IDPL
            IC=IC+1
            IF(IC.EQ.23) THEN
               IF(VALUES(K).GT.0.) THEN
                  VALUE(J)=RVIND
                  J=J+1
                  VALUE(J)=RVIND
                  J=J+1
                  VALUE(J)=RVIND
                  J=J+1
                  VALUE(J)=RVIND
                  J=J+1
                  GO TO 707
               ELSE
                  IPC=IPC+1
                  VALUE(J)=VALUES(IPC)
                  J=J+1
                  VALUE(J)=RVIND
                  J=J+1
                  VALUE(J)=RVIND
                  J=J+1
                  VALUE(J)=RVIND
                  J=J+1
                  GO TO 707
               END IF
            END IF
            IF(VALUES(K).GT.0.) THEN
               VALUE(J)=RVIND
               J=J+1
            ELSE
               IPC=IPC+1
               VALUE(J)=VALUES(IPC)
               J=J+1
            END IF
 707        CONTINUE
            GO TO 710
         END IF
C
         IF(KTDEXP(I).EQ.13031) THEN
            VALUE(J)=RVIND
            J=J+1
            VALUE(J)=RVIND
            J=J+1
            VALUE(J)=RVIND
            J=J+1
            VALUE(J)=VALUES(I)
            GO TO 706
         END IF
C
         IF(KTDEXP(I).GE.22021.AND.KTDEXP(I).LE.22023) THEN
            IF(ABS(VALUES(I)-RVIND)/RVIND.LT.EPS) THEN
               VALUE(J)=VALUES(I)
            ELSE
               VALUE(J)=VALUES(I)/10.
            END IF
            GO TO 706
         END IF
C
         IF(KTDEXP(I).GE.13019.AND.KTDEXP(I).LE.13023) THEN
            IF(ABS(VALUES(I)-RVIND)/RVIND.LT.EPS) THEN
               VALUE(J)=VALUES(I)
            ELSE
               VALUE(J)=VALUES(I)*1000.
            END IF
         ELSE
            VALUE(J)=VALUES(I)
         END IF
C
 706     CONTINUE
C
 710     CONTINUE
C
         KERR=0
         RETURN
      END IF
C
      IF(KST.GE.21.AND.KST.LE.23) THEN
C
C        DRIBU BATHY TESA 21,22,23
C        -------------------------
C
C        CHANGE OPERATOR
C
         J=0
C
         DO 301 I=1,KTDLEN
C
         IF(KTDLST(I).GT.262000.AND.KTDLST(I).LT.300000) THEN
            J=J+1
            KTDLST1(J)=222000
         ELSEIF(KTDLST(I).EQ.262000) THEN
               GO TO 301
         ELSEIF(KTDLST(I).GT.101000.AND.KTDLST(I).LT.101032) THEN
            J=J+1
            KTDLST1(J)=101032
         ELSEIF(KTDLST(I).EQ.063191) THEN
            J=J+1
            KTDLST1(J)=001201
         ELSEIF(KTDLST(I).EQ.063192) THEN
            J=J+1
            KTDLST1(J)=033007
         ELSEIF(KTDLST(I).EQ.031192) THEN
            J=J+1
            KTDLST1(J)=031031
            J=J+1
            KTDLST1(J)=001031
         ELSE
            J=J+1
            KTDLST1(J)=KTDLST(I)
         END IF
C
 301     CONTINUE
C
         KTDLEN=J
C
C        FIND FIRST AND LAST POINTER TO DATA PRESENT INDICATOR
C
         ODPF=.FALSE.
C
         DO 302 I=1,KTDEXL
         IF(KTDEXP(I).EQ.31192) THEN
            IF(.NOT.ODPF) IDPF=I
            ODPF=.TRUE.
         ELSE
            IF(ODPF) THEN
               IDPL=I-1
               GO TO 303
            END IF
         END IF
 302     CONTINUE
C
 303     CONTINUE
C
C        FIND START OF % CONFIDENCE
C
         DO 304 I=IDPL,KTDEXL
         IF(KTDEXP(I).EQ.63192) THEN
            IPCF=I
            GO TO 305
         END IF
 304     CONTINUE
C
 305     CONTINUE
C
C        MAKE DATA SUITABLE FOR COMPRESSION
C
         J=0
         KDLEN=0
         IPC=IPCF-1
C
         DO 306 I=1,KTDEXL
         J=J+1
         IF(KTDEXP(I).EQ.031001.OR.KTDEXP(I).EQ.031002) THEN
            KDLEN=KDLEN+1
            KDATA(KDLEN)=NINT(VALUES(I))
         END IF
C
         IF(KTDEXP(I).EQ.063191) THEN    ! GENERATING APPLICATION
            VALUE(J)=98.                 ! GENERATING CENTRE
            J=J+1
            VALUE(J)=1.
            GO TO 306
         END IF
C
         IF(KTDEXP(I).EQ.031192) THEN
            VALUE(J)=0.    ! DATA PRESENT
            GO TO 306
         END IF
C
         IF(KTDEXP(I).EQ.063192) THEN            ! % CONF
            DO 307 K=IDPF,IDPL
            IF(VALUES(K).GT.0.) THEN
               VALUE(J)=RVIND
               J=J+1
            ELSE
               IPC=IPC+1
               VALUE(J)=VALUES(IPC)
               J=J+1
            END IF
 307        CONTINUE
            GO TO 310
         END IF
C
         VALUE(J)=VALUES(I)
C
 306     CONTINUE
C
 310     CONTINUE
C
         KERR=0
         RETURN
      END IF
      IF(KST.EQ.51.OR.KST.EQ.52) THEN
C
C        MOVE FIRST 12 ELEMENTS
C
         DO 854 J=1,12
         DO 851 I=1,KSUP(6)
         IJ=J+(I-1)*KELEM
         VALUE(IJ)=VALUES(IJ)
 851     CONTINUE
 854     CONTINUE
C
C        INSERT SATELLITE LOCATION COUNTER
C
         DO 856 I=1,KSUP(6)
         IJ=13+(I-1)*KELEM
         VALUE(IJ)=RVIND
 856     CONTINUE
C
C        MOVE THE REST OF THE DATA
C
         JJ=13
         DO 858 J=13,KSUP(5)
         JJ=JJ+1
         DO 857 I=1,KSUP(6)
         IJ=J+(I-1)*KELEM
         IZ=JJ+(I-1)*KELEM
         VALUE(IZ)=VALUES(IJ)
 857     CONTINUE
 858     CONTINUE
C
         DO 853 I=1,12
         KTDLST1(I)=KTDLST(I)
 853     CONTINUE
         KTDLST1(13)=027020
         J=13
         DO 852 I=13,KTDLEN
         J=J+1
         KTDLST1(J)=KTDLST(I)
         IF(J.EQ.21) KTDLST1(J)=010001
         IF(J.GT.24.AND.J.LT.42) THEN
            IF(KTDLST1(J).EQ.010004) KTDLST1(J)=007004
         END IF
 852     CONTINUE
         KTDLEN=KTDLEN+1
         KERR=0
         RETURN
      END IF
C
      IF(KST.GE.61.AND.KST.LE.63.OR.
     1   KST.GE.71.AND.KST.LE.73.OR.
     2   KST.GE.82.AND.KST.LE.85 ) THEN
C
C        SATEM 500 KM 61,62,63 AND TOVS 250 KM. 71,72,73
C        ------------------------------------------------
C        CHANGE OPERATOR
C
         J=0
C
         DO 501 I=1,KTDLEN
C
         IF(KTDLST(I).GT.262000.AND.KTDLST(I).LT.300000) THEN
            J=J+1
            KTDLST1(J)=222000
         ELSEIF(KTDLST(I).EQ.262000) THEN
               GO TO 501
         ELSEIF(KTDLST(I).GT.101000.AND.KTDLST(I).LT.101063) THEN
            J=J+1
            IF(KST.EQ.61) KTDLST1(J)=101063
            IF(KST.EQ.62) KTDLST1(J)=101033
            IF(KST.EQ.63) KTDLST1(J)=101048
            IF(KST.EQ.71) KTDLST1(J)=101063
            IF(KST.EQ.72) KTDLST1(J)=101033
            IF(KST.EQ.73) KTDLST1(J)=101048
            IF(KST.EQ.82) KTDLST1(J)=101015
            IF(KST.EQ.83) KTDLST1(J)=101014
            IF(KST.EQ.84) KTDLST1(J)=101012
            IF(KST.EQ.85) KTDLST1(J)=101014
         ELSEIF(KTDLST(I).EQ.063191) THEN
            J=J+1
            KTDLST1(J)=001201
         ELSEIF(KTDLST(I).EQ.063192) THEN
            J=J+1
            KTDLST1(J)=033007
         ELSEIF(KTDLST(I).EQ.031192) THEN
            J=J+1
            KTDLST1(J)=031031
            J=J+1
            KTDLST1(J)=001031
         ELSE
            J=J+1
            KTDLST1(J)=KTDLST(I)
         END IF
C
 501     CONTINUE
C
         KTDLEN=J
C
C        FIND FIRST AND LAST POINTER TO DATA PRESENT INDICATOR
C
         ODPF=.FALSE.
C
         DO 502 I=1,KTDEXL
         IF(KTDEXP(I).EQ.31192) THEN
            IF(.NOT.ODPF) IDPF=I
            ODPF=.TRUE.
         ELSE
            IF(ODPF) THEN
               IDPL=I-1
               GO TO 503
            END IF
         END IF
 502     CONTINUE
C
 503     CONTINUE
C
C        FIND START OF % CONFIDENCE
C
         DO 504 I=IDPL,KTDEXL
         IF(KTDEXP(I).EQ.63192) THEN
            IPCF=I
            GO TO 505
         END IF
 504     CONTINUE
C
 505     CONTINUE
C
C        MAKE DATA SUITABLE FOR COMPRESSION
C
         J=0
         KDLEN=0
         IPC=IPCF-1
C
         DO 506 I=1,KTDEXL
         J=J+1
         IF(KTDEXP(I).EQ.031001.OR.KTDEXP(I).EQ.031002) THEN
            KDLEN=KDLEN+1
            KDATA(KDLEN)=NINT(VALUES(I))
         END IF
C
         IF(KTDEXP(I).EQ.063191) THEN    ! GENERATING APPLICATION
            DO 5061 I5061=1,KSUP(6)
            IV=J+(I5061-1)*KELEM
 5061       VALUE(IV)=98.                 ! GENERATING CENTRE
            J=J+1
            DO 5062 I5062=1,KSUP(6)
            IV=J+(I5062-1)*KELEM
 5062       VALUE(IV)=1.
            GO TO 506
         END IF
C
         IF(KTDEXP(I).EQ.031192) THEN
            DO 5063 I5063=1,KSUP(6)
            IV=J+(I5063-1)*KELEM
 5063       VALUE(IV)=0.                         ! DATA PRESENT
            GO TO 506
         END IF
C
         IF(KTDEXP(I).EQ.063192) THEN            ! % CONF
            DO 507 K=IDPF,IDPL
            IF(VALUES(K).GT.0.) THEN
               IPC=IPC+1
               DO 5071 I5071=1,KSUP(6)
               IV=J+(I5071-1)*KELEM
               VALUE(IV)=RVIND
 5071          CONTINUE
               J=J+1
            ELSE
               IPC=IPC+1
               DO 5072 I5072=1,KSUP(6)
               IPP=IPC+(I5072-1)*KELEM
               IV =J  +(I5072-1)*KELEM
               VALUE(IV)=VALUES(IPP)
 5072          CONTINUE
               J=J+1
            END IF
 507        CONTINUE
            GO TO 510
         END IF
C
         IF(KTDEXP(I).EQ.13016) THEN
C
C           CHANGE UNITS FOR PRECIPITABLE WATER
C
            DO 5081 I5081=1,KSUP(6)
            IN=I+(I5081-1)*KELEM
            IV=J+(I5081-1)*KELEM
            IF(ABS(VALUES(IN)-RVIND)/RVIND.LT.EPS) THEN
               VALUE(IV)=RVIND
            ELSE
               VALUE(IV)=VALUES(IN)*1000.
            END IF
 5081       CONTINUE
         ELSE
            DO 5082 I5082=1,KSUP(6)
            IN=I+(I5082-1)*KELEM
            IV=J+(I5082-1)*KELEM
 5082       VALUE(IV)=VALUES(IN)
         END IF
C
 506     CONTINUE
C
 510     CONTINUE
C
         KERR=0
         RETURN
      END IF
      IF(KST.EQ.101.OR.KST.eq.91.OR.
     1   KST.GE.131.AND.KST.LE.133) THEN
C
C         TEMP LAND STATIONS AND PILOT 91,92
C         ----------------------------------
         J=0
C
         DO 601 I=1,KTDLEN
C
         IF(KTDLST(I).GT.262000.AND.KTDLST(I).LT.300000) THEN
            J=J+1
            KTDLST1(J)=222000
         ELSEIF(KTDLST(I).EQ.262000) THEN
               GO TO 601
         ELSEIF(KTDLST(I).EQ.063191) THEN
            J=J+1
            KTDLST1(J)=001201
         ELSEIF(KTDLST(I).EQ.063192) THEN
            J=J+1
            KTDLST1(J)=033007
         ELSEIF(KTDLST(I).EQ.031192) THEN
            J=J+1
            KTDLST1(J)=031031
            J=J+1
            KTDLST1(J)=001031
         ELSE
            J=J+1
            KTDLST1(J)=KTDLST(I)
         END IF
C
 601     CONTINUE
C
         KTDLEN=J
C
C        FIND FIRST AND LAST POINTER TO DATA PRESENT INDICATOR
C
         ODPF=.FALSE.
C
         DO 602 I=1,KTDEXL
         IF(KTDEXP(I).EQ.31192) THEN
            IF(.NOT.ODPF) IDPF=I
            ODPF=.TRUE.
         ELSE
            IF(ODPF) THEN
               IDPL=I-1
               GO TO 603
            END IF
         END IF
 602     CONTINUE
C
 603     CONTINUE
C
C        FIND START OF % CONFIDENCE
C
         DO 604 I=IDPL,KTDEXL
         IF(KTDEXP(I).EQ.63192) THEN
            IPCF=I
            GO TO 605
         END IF
 604     CONTINUE
C
 605     CONTINUE
C
C        MAKE DATA SUITABLE FOR COMPRESSION
C
         J=0
         KDLEN=0
         IPC=IPCF-1
C
         DO 606 I=1,KTDEXL
         J=J+1
         IF(KTDEXP(I).EQ.031001.OR.KTDEXP(I).EQ.031002) THEN
            KDLEN=KDLEN+1
            KDATA(KDLEN)=NINT(VALUES(I))
         END IF
C
         IF(KTDEXP(I).EQ.063191) THEN    ! GENERATING APPLICATION
            VALUE(J)=98.                 ! GENERATING CENTRE
            J=J+1
            VALUE(J)=1.
            GO TO 606
         END IF
C
         IF(KTDEXP(I).EQ.031192) THEN
            VALUE(J)=VALUES(I)             !0.    ! DATA PRESENT
            GO TO 606
         END IF
C
         IF(KTDEXP(I).EQ.063192) THEN            ! % CONF
            VALUE(J)=VALUES(I)
            GO TO 606
         END IF
C
         IF(KTDEXP(I).EQ.10003.OR.KTDEXP(I).EQ.10008) THEN
            IF(ABS(VALUES(I)-RVIND)/RVIND.GT.EPS) THEN
               VALUE(J)=(VALUES(I)/9.8)*9.80665
            ELSE
               VALUE(J)=VALUES(I)
            END IF
         ELSE
            VALUE(J)=VALUES(I)
         END IF
C
 606     CONTINUE
C
 610     CONTINUE
C
         KERR=0
         RETURN
      END IF
      IF(KST.EQ.102.or.kst.eq.92) THEN
C
C        TEMP SHIP 102
C        -------------
         J=0
         OSHEAR=.FALSE.
C
         DO 801 I=1,KTDLEN
C
         IF(KTDLST(I).GT.262000.AND.KTDLST(I).LT.300000) THEN
            J=J+1
            KTDLST1(J)=222000
         ELSEIF(KTDLST(I).EQ.262000) THEN
               GO TO 801
         ELSEIF(KTDLST(I).EQ.063191) THEN
            J=J+1
            KTDLST1(J)=001201
         ELSEIF(KTDLST(I).EQ.063192) THEN
            J=J+1
            KTDLST1(J)=033007
         ELSEIF(KTDLST(I).EQ.031192) THEN
            J=J+1
            KTDLST1(J)=031031
            J=J+1
            KTDLST1(J)=001031
         ELSEIF(KTDLST(I).EQ.309014) THEN
            J=J+1
            KTDLST1(J)=309196
         ELSEIF(KTDLST(I).EQ.301023) THEN
            J=J+1
            KTDLST1(J)=301024
         ELSE
            J=J+1
            KTDLST1(J)=KTDLST(I)
            IF(KTDLST(I).EQ.11061.OR.KTDLST(I).EQ.11062) OSHEAR=.TRUE.
         END IF
C
 801     CONTINUE
C
         KTDLEN=J
C
C        FIND FIRST AND LAST POINTER TO DATA PRESENT INDICATOR
C
         ODPF=.FALSE.
C
         DO 802 I=1,KTDEXL
         IF(KTDEXP(I).EQ.31192) THEN
            IF(.NOT.ODPF) IDPF=I
            ODPF=.TRUE.
         ELSE
            IF(ODPF) THEN
               IDPL=I-1
               GO TO 803
            END IF
         END IF
 802     CONTINUE
C
 803     CONTINUE
C
C        FIND START OF % CONFIDENCE
C
         DO 804 I=IDPL,KTDEXL
         IF(KTDEXP(I).EQ.63192) THEN
            IPCF=I
            GO TO 805
         END IF
 804     CONTINUE
C
 805     CONTINUE
C
C        MAKE DATA SUITABLE FOR COMPRESSION
C
         J=0
         KDLEN=0
         ISTAT=0
         IPC=IPCF-1
C
         DO 806 I=1,KTDEXL
         J=J+1
         IF(KTDEXP(I).EQ.031001.OR.KTDEXP(I).EQ.031002) THEN
            KDLEN=KDLEN+1
            KDATA(KDLEN)=NINT(VALUES(I))
            IF(KDLEN.EQ.2.AND..NOT.OSHEAR) THEN
               KDATA(KDLEN)=NINT(VALUES(I))+1
            END IF
            IF(KDLEN.EQ.3.AND.OSHEAR) THEN
               KDATA(KDLEN)=NINT(VALUES(I))+1
            END IF
         END IF
C
         IF(KTDEXP(I).EQ.063191) THEN    ! GENERATING APPLICATION
            VALUE(J)=98.                 ! GENERATING CENTRE
            J=J+1
            VALUE(J)=1.
            GO TO 806
         END IF
C
         IF(KTDEXP(I).EQ.031192) THEN
            ISTAT=ISTAT+1
            IF(ISTAT.EQ.12) THEN
               VALUE(J)=VALUES(I)
               J=J+1
               VALUE(J)=1.
               VALUE(IDPF)=VALUES(IDPF-1)+1
            ELSE
               VALUE(J)=VALUES(I)
            END IF
            GO TO 806
         END IF
C
         IF(KTDEXP(I).EQ.063192) THEN            ! % CONF
            VALUE(J)=VALUES(I)
            GO TO 806
         END IF
C
         IF(KTDEXP(I).EQ.006002) THEN
            VALUE(J)=VALUES(I)
            J=J+1
            VALUE(J)=RVIND                    ! INSERT STATION HEIGHT
            GO TO 806
         END IF
C
         IF(KTDEXP(I).EQ.10003.OR.KTDEXP(I).EQ.10008) THEN
            IF(ABS(VALUES(I)-RVIND)/RVIND.GT.EPS) THEN
               VALUE(J)=(VALUES(I)/9.8)*9.80665
            ELSE
               VALUE(J)=VALUES(I)
            END IF
         ELSE
            VALUE(J)=VALUES(I)
         END IF
C
 806     CONTINUE
C
 810     CONTINUE
C
         KERR=0
         RETURN
      END IF
C
      IF(KST.EQ.103) THEN
C
C        TEMP DROP 103
C        -------------
         J=0
C
         DO 8013 I=1,KTDLEN
C
         IF(KTDLST(I).GT.262000.AND.KTDLST(I).LT.300000) THEN
            J=J+1
            KTDLST1(J)=222000
         ELSEIF(KTDLST(I).EQ.262000) THEN
               GO TO 8013
         ELSEIF(KTDLST(I).EQ.063191) THEN
            J=J+1
            KTDLST1(J)=001032
         ELSEIF(KTDLST(I).EQ.063192) THEN
            J=J+1
            KTDLST1(J)=033007
         ELSEIF(KTDLST(I).EQ.031192) THEN
            J=J+1
            KTDLST1(J)=031031
            J=J+1
            KTDLST1(J)=001031
         ELSEIF(KTDLST(I).EQ.309007) THEN
            J=J+1
            KTDLST1(J)=309194
         ELSE
            J=J+1
            KTDLST1(J)=KTDLST(I)
            IF(KTDLST(I).EQ.11061.OR.KTDLST(I).EQ.11062) OSHEAR=.TRUE.
         END IF
C
 8013    CONTINUE
C
         KTDLEN=J
C
         ODPF=.FALSE.
C
         DO 8023 I=1,KTDEXL
         IF(KTDEXP(I).EQ.31192) THEN
            IF(.NOT.ODPF) IDPF=I
            ODPF=.TRUE.
         ELSE
            IF(ODPF) THEN
               IDPL=I-1
               GO TO 8033
            END IF
         END IF
 8023    CONTINUE
C
 8033    CONTINUE

C
C        FIND START OF % CONFIDENCE
C
         DO 8043 I=IDPL,KTDEXL
         IF(KTDEXP(I).EQ.63192) THEN
            IPCF=I
            GO TO 8053
         END IF
 8043    CONTINUE
C
 8053    CONTINUE
C
         J=0
         KDLEN=0
         j=0
C
         DO 8063 I=1,KTDEXL
         J=J+1
         IF(KTDEXP(I).EQ.031001.OR.KTDEXP(I).EQ.031002) THEN
            KDLEN=KDLEN+1
            KDATA(KDLEN)=NINT(VALUES(I))
            value(j)=NINT(VALUES(I))
            if(KDLEN.eq.2) then
               KDATA(KDLEN)=NINT(VALUES(I))+1
               value(j)=KDATA(KDLEN)
            end if
            if(KDLEN.eq.3) then
               KDATA(KDLEN)=NINT(VALUES(I))
               value(j)=KDATA(KDLEN)-1
            end if
            go to 8063
         END IF

C
         IF(KTDEXP(I).EQ.063191) THEN    ! GENERATING APPLICATION
            VALUE(J)=98.                 ! INSERT GENERATING CENTRE
            J=J+1
            VALUE(J)=1.
            GO TO 8063
         END IF
C
         IF(KTDEXP(I).EQ.031192) THEN
               if(i.eq.idpf) then
                  VALUE(J)=rvind
               elseif(i.eq.(idpf+13)) then
                  VALUE(J)=rvind
                  j=j+1
                  VALUE(J)=rvind
               else
                  VALUE(J)=VALUES(I)
               end if
               GO TO 8063
         END IF
C
         IF(KTDEXP(I).EQ.063192) THEN            ! % CONF
            if(i.eq.ipcf) then
               j=j-1
               GO TO 8063
            else 
               VALUE(J)=VALUES(I)
            end if
            GO TO 8063
         END IF
C
         IF(KTDEXP(I).EQ.10003.OR.KTDEXP(I).EQ.10008) THEN
            IF(ABS(VALUES(I)-RVIND)/RVIND.GT.EPS) THEN
               VALUE(J)=(VALUES(I)/9.8)*9.80665
            ELSE
               VALUE(J)=VALUES(I)
            END IF
         ELSE
            if(j.eq.1) then
               value(j)=rvind
               j=j+1
               value(j)=rvind
            else
               VALUE(J)=VALUES(I)
            end if
         END IF
C
 8063    CONTINUE
C
 8103    CONTINUE
C
         value(1)=1009.
         do i=2,13
         value(i)=rvind
         end do
c
         KERR=0
         RETURN
      END IF
C
C
      IF(KST.EQ.142.OR.KST.EQ.144.OR.KST.EQ.164) THEN
C
C        AIREP DATA 142 , AMDAR DATA 144, PAOB 164
C
C        CHANGE OPERATOR
C
         J=0
C
         DO 401 I=1,KTDLEN
C
         IF(KTDLST(I).GT.262000.AND.KTDLST(I).LT.300000) THEN
            J=J+1
            KTDLST1(J)=222000
         ELSEIF(KTDLST(I).EQ.262000) THEN
               GO TO 401
         ELSEIF(KTDLST(I).GT.101000.AND.KTDLST(I).LE.101018) THEN
            J=J+1
            IF(KST.EQ.164) KTDLST1(J)=101011
            IF(KST.EQ.142) KTDLST1(J)=101018
            IF(KST.EQ.144) KTDLST1(J)=101018
         ELSEIF(KTDLST(I).EQ.063191) THEN
            J=J+1
            KTDLST1(J)=001032
         ELSEIF(KTDLST(I).EQ.063192) THEN
            J=J+1
            KTDLST1(J)=033007
         ELSEIF(KTDLST(I).EQ.031192) THEN
            J=J+1
            KTDLST1(J)=031031
            J=J+1
            KTDLST1(J)=001031
         ELSE
            J=J+1
            KTDLST1(J)=KTDLST(I)
         END IF
C
 401     CONTINUE
C
         KTDLEN=J
C
C        FIND FIRST AND LAST POINTER TO DATA PRESENT INDICATOR
C
         ODPF=.FALSE.
C
         DO 402 I=1,KTDEXL
         IF(KTDEXP(I).EQ.31192.OR.KTDEXP(I).EQ.31031) THEN
            IF(.NOT.ODPF) IDPF=I
            ODPF=.TRUE.
         ELSE
            IF(ODPF) THEN
               IDPL=I-1
               GO TO 403
            END IF
         END IF
 402     CONTINUE
C
 403     CONTINUE
C
C        FIND START OF % CONFIDENCE
C
         DO 404 I=IDPL,KTDEXL
         IF(KTDEXP(I).EQ.63192.OR.KTDEXP(I).EQ.33007) THEN
            IPCF=I
            GO TO 405
         END IF
 404     CONTINUE
C
 405     CONTINUE
C
C        MAKE DATA SUITABLE FOR COMPRESSION
C
         J=0
         KDLEN=0
         IPC=IPCF-1
C
         DO 406 I=1,KTDEXL
         J=J+1
         IF(KTDEXP(I).EQ.031001.OR.KTDEXP(I).EQ.031002) THEN
            KDLEN=KDLEN+1
            KDATA(KDLEN)=NINT(VALUES(I))
         END IF
C
         IF(KTDEXP(I).EQ.063191) THEN    ! GENERATING APPLICATION
            VALUE(J)=98.                 ! GENERATING CENTRE
            J=J+1
            VALUE(J)=1.
            GO TO 406
         END IF
C
         IF(KTDEXP(I).EQ.031192) THEN
            VALUE(J)=0.    ! DATA PRESENT
            GO TO 406
         END IF
C
         IF(KTDEXP(I).EQ.063192) THEN            ! % CONF
            DO 407 K=IDPF,IDPL
            IF(VALUES(K).GT.0.) THEN
               VALUE(J)=RVIND
               J=J+1
            ELSE
               IPC=IPC+1
               VALUE(J)=VALUES(IPC)
               J=J+1
            END IF
 407        CONTINUE
            GO TO 410
         END IF
C
         VALUE(J)=VALUES(I)
C
 406     CONTINUE
C
 410     CONTINUE
C
         KERR=0
         RETURN
      END IF
C
      IF(KST.GE.121.AND.KST.LE.123) THEN
C
C        IRS1 DATA
C
         DO 7501 I=1,KTDLEN
         KTDLST1(I)=KTDLST(I)
 7501    CONTINUE
C
         DO 7502 I=1,KSUP(6)
         II=(I-1)*KELEM 
         DO 7503 J=1,KSUP(5)
         IJ=J+II
         VALUE(IJ)=VALUES(IJ)
 7503    CONTINUE
 7502    CONTINUE
C
         RETURN
      END IF
C
      KERR=1
C
      RETURN
      END
      SUBROUTINE BUCON2(KST,KSUP,KTDLEN,KTDLST,KTDLST1,KTDEXL,KTDEXP,
     1                  KELEM,KVALS,VALUES,VALUE,KDLEN,KDATA,KERR)
C
C**** *BUCON2*
C
C
C     PURPOSE.
C     --------
C         CONVERTS BUFR EDITION N TO EDITION M  DATA.
C         
C
C
C**   INTERFACE.
C     ----------
C
C              *CALL BUCON2(KST,KTDLEN,KTDLST,KTDLST1,KTDEXL,KTDEXP,
C                           KELEM,KVALS,VALUES,VALUE,KERR)*
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
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/09/94
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
      DIMENSION KTDLST(*),KTDEXP(*)
      DIMENSION KTDLST1(*),KSUP(*),KDATA(*)
      REAL*8 VALUES(*),VALUE(*)
      REAL*8 RVIND,EPS
C
C     ------------------------------------------------------------------
C*          1. INITIALIZE CONSTANTS AND VARIABLES.
C              -----------------------------------
 100  CONTINUE
C
C     MISSING VALUE INDICATOR
C
      RVIND=1.7D38
      EPS=10.D-8
C
      IF(KST.EQ.142) THEN
C
C        AIREP OBSERVATION (ERA PROJECT)
C
C
C        CHANGE OPERATOR
C
         J=0
C
         DO 101 I=1,KTDLEN
C
         IF(KTDLST(I).EQ.101000) THEN
            J=J+1
            KTDLST1(J)=101018
         ELSEIF(KTDLST(I).EQ.31002) THEN
               GO TO 101
         ELSE
            J=J+1
            KTDLST1(J)=KTDLST(I)
         END IF
C
 101     CONTINUE
C
         KTDLEN=J
C
C        FIND FIRST AND LAST POINTER TO DATA PRESENT INDICATOR
C
         ODPF=.FALSE.
C
         DO 102 I=1,KTDEXL
         IF(KTDEXP(I).EQ.31031) THEN
            IF(.NOT.ODPF) IDPF=I
            ODPF=.TRUE.
         ELSE
            IF(ODPF) THEN
               IDPL=I-1
               GO TO 103
            END IF
         END IF
 102     CONTINUE
C
 103     CONTINUE
C
C        FIND START OF % CONFIDENCE
C
         DO 104 I=IDPL,KTDEXL
         IF(KTDEXP(I).EQ.033007) THEN
            IPCF=I
            GO TO 105
         END IF
 104     CONTINUE
C
 105     CONTINUE
C
C        MAKE DATA SUITABLE FOR COMPRESSION
C
         J=0
         KDLEN=0
         IPC=IPCF-1
C
         DO 106 I=1,KTDEXL
         J=J+1
C
         IF(KTDEXP(I).EQ.031002) THEN
            J=J-1
            GO TO 106
         END IF
C
         IF(KTDEXP(I).EQ.031031) THEN
            VALUE(J)=0.    ! DATA PRESENT
            GO TO 106
         END IF
C
         IF(KTDEXP(I).EQ.033007) THEN            ! % CONF
            DO 107 K=IDPF,IDPL
            IF(VALUES(K).GT.0.) THEN
               VALUE(J)=70   !RVIND
               J=J+1
            ELSE
               IPC=IPC+1
               VALUE(J)=VALUES(IPC)
               J=J+1
            END IF
 107        CONTINUE
            GO TO 110
         END IF
C
         VALUE(J)=VALUES(I)
C
 106     CONTINUE
C
 110     CONTINUE
C
         KERR=0
         RETURN
      END IF
      IF(KST.GT.0.AND.KST.LE.255) THEN
C
C        FIND % CONF
C
         ICNF=0
         DO 210 I=1,KTDEXL
         IF(KTDEXP(I).EQ.33007) THEN
            ICNF=I
            GO TO 211
         END IF
 210     CONTINUE
C
 211     CONTINUE
         DO 212 J=ICNF+1,KTDEXL
         IF(KTDEXP(J).NE.33007) THEN
            JCNF=J-1
            GO TO 213
         END IF
 212     CONTINUE
C
 213     CONTINUE
C
         DO 201 I=1,KSUP(5)
         DO 202 J=1,KSUP(6)
         IJ=I+(J-1)*KELEM
         IF(ICNF.NE.0) THEN
         IF(I.GE.ICNF.AND.I.LE.JCNF) THEN
         IF(ABS(VALUES(IJ)-RVIND)/RVIND.LT.EPS) VALUES(IJ)=70.
         END IF
         END IF
         VALUE(IJ)=VALUES(IJ)
 202     CONTINUE
 201     CONTINUE
C
         DO 203 I=1,KTDLEN
         KTDLST1(I)=KTDLST(I)
 203     CONTINUE
C
         KDLEN=0
         DO 204 I=1,KTDEXL
         IF(KTDEXP(I).EQ.031001.OR.
     1      KTDEXP(I).EQ.031002) THEN
            KDLEN=KDLEN+1
            KDATA(KDLEN)=NINT(VALUES(I))
         END IF
 204     CONTINUE
         KERR=0
         RETURN
      END IF
C
      KERR=1
      PRINT*,'NO CONVERTION FOR ',KST,' SUBTYPE.'
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
      SUBROUTINE BUCON3(KST,KSUP,KTDLEN,KTDLST,KTDLST1,KTDEXL,KTDEXP,
     1                  KELEM,KVALS,VALUES,VALUE,KDLEN,KDATA,KERR)
C
C**** *BUCON3*
C
C
C     PURPOSE.
C     --------
C         CONVERTS BUFR EDITION N TO EDITION M  DATA.
C
C
C
C**   INTERFACE.
C     ----------
C
C              *CALL BUCON3(KST,KTDLEN,KTDLST,KTDLST1,KTDEXL,KTDEXP,
C                           KELEM,KVALS,VALUES,VALUE,KDLEN,KDATA,KERR)*
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
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/09/94
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
      DIMENSION KTDLST(*),KTDEXP(*)
      DIMENSION KTDLST1(*),KSUP(*),KDATA(*)
      REAL*8 VALUES(*),VALUE(*)
      REAL*8 RVIND,EPS
C
C     ------------------------------------------------------------------
C*          1. INITIALIZE CONSTANTS AND VARIABLES.
C              -----------------------------------
 100  CONTINUE
C
C     MISSING VALUE INDICATOR
C
      RVIND=1.7D38
      EPS=10.D-8
C
      IF(KST.EQ.142.AND.KSUP(6).EQ.1) THEN
C
C        AIREP OBSERVATION (ERA PROJECT)
C
C
C        CHANGE OPERATOR
C
         J=0
C
         DO 101 I=1,KTDLEN
C
         IF(KTDLST(I).EQ.101000) THEN
            J=J+1
            KTDLST1(J)=101018
         ELSEIF(KTDLST(I).EQ.31002) THEN
               GO TO 101
         ELSE
            J=J+1
            KTDLST1(J)=KTDLST(I)
         END IF
C
 101     CONTINUE
C
         KTDLST1(7)=101018
C
         KTDLEN=J
C
C        FIND FIRST AND LAST POINTER TO DATA PRESENT INDICATOR
C
         ODPF=.FALSE.
C
         DO 102 I=1,KTDEXL
         IF(KTDEXP(I).EQ.31031) THEN
            IF(.NOT.ODPF) IDPF=I
            ODPF=.TRUE.
         ELSE
            IF(ODPF) THEN
               IDPL=I-1
               GO TO 103
            END IF
         END IF
 102     CONTINUE
C
 103     CONTINUE
C
C        FIND START OF % CONFIDENCE
C
         DO 104 I=IDPL,KTDEXL
         IF(KTDEXP(I).EQ.033007) THEN
            IPCF=I
            GO TO 105
         END IF
 104     CONTINUE
C
 105     CONTINUE
C
C        MAKE DATA SUITABLE FOR COMPRESSION
C
         J=0
         KDLEN=0
         IPC=IPCF-1
C
         DO 106 I=1,KTDEXL
         J=J+1
C
         IF(KTDEXP(I).EQ.031002) THEN
            J=J-1
            GO TO 106
         END IF
C
         IF(KTDEXP(I).EQ.031031) THEN
            VALUE(J)=0.    ! DATA PRESENT
            GO TO 106
         END IF
C
         IF(KTDEXP(I).EQ.033007) THEN            ! % CONF
            DO 107 K=IDPF,IDPL
            IF(VALUES(K).GT.0.) THEN
               VALUE(J)=70.   !RVIND
               J=J+1
            ELSE
               IPC=IPC+1
               VALUE(J)=VALUES(IPC)
               J=J+1
            END IF
 107        CONTINUE
            GO TO 110
         END IF
C
         VALUE(J)=VALUES(I)
C
 106     CONTINUE
C
 110     CONTINUE
C
         KERR=0
         RETURN
      END IF
C
      IF(KST.EQ.9.OR.KST.EQ.11.OR.KST.EQ.13.OR.KST.EQ.19) THEN
C
C        SYNOP SHIP OBSERVATION (ERA PROJECT)
C        SOME SHIP HAS NOT GOT 012005 AFTER 308004
C
C
C        CHANGE OPERATOR
C
         IF(KTDLST(1).EQ.308004.AND.KTDLST(2).EQ.12005) go to 300
C
         J=0
C
         DO I=1,KTDLEN
C
         IF(KTDLST(I).EQ.308004) THEN
            J=J+1
            KTDLST1(J)=KTDLST(I)
            IF(KTDLST(I+1).NE.012005) THEN
               J=J+1
               KTDLST1(J)=012005
            END IF
         ELSEIF(KTDLST(I).EQ.101032) THEN
            J=J+1
            KTDLST1(J)=101033
         ELSEIF(KTDLST(I).EQ.001201) THEN
            J=J+1
            KTDLST1(J)=001032
         ELSE
            J=J+1
            KTDLST1(J)=KTDLST(I)
         END IF
         END DO
C
         KTDLST1(8)=101033
C
         KTDLEN=J
C
C        FIND FIRST AND LAST POINTER TO DATA PRESENT INDICATOR
C
         ODPF=.FALSE.
C
         DO I=1,KTDEXL
         IF(KTDEXP(I).EQ.31031) THEN
            IF(.NOT.ODPF) IDPF=I
            ODPF=.TRUE.
         ELSE
            IF(ODPF) THEN
               IDPL=I-1
               GO TO 113
            END IF
         END IF
         END DO
C
 113     CONTINUE
C
C        FIND START OF % CONFIDENCE
C
         DO I=IDPL,KTDEXL
         IF(KTDEXP(I).EQ.033007) THEN
            IPCF=I
            GO TO 115
         END IF
         END DO
C
 115     CONTINUE
C
C        MAKE DATA SUITABLE FOR COMPRESSION
C
         J=0
         KDLEN=0
         IPC=IPCF-1
C
         DO  I=1,IDPF-2
         J=J+1
         VALUE(J)=VALUES(I)
         END DO
C
C        FOR 012005 ELEMENT
C
         J=J+1
         VALUE(J)=RVIND
C
C        FOR 222000 OPERATOR
C
         J=J+1
         VALUE(J)=0.0
C
C        DATA PRESENT
C
         DO I=IDPF,IDPL+1
         J=J+1
         VALUE(J)=0.
         END DO
C
C        GENERATING CENTRE AND APPLICATION
         J=J+1
         VALUE(J)=98.
         J=J+1
         VALUE(J)=1.
C
         IPC=IPCF-1
         DO I=IDPF,IDPL
         IF(VALUES(I).GT.0.) THEN
            J=J+1
            VALUE(J)=70.
         ELSE
            IPC=IPC+1
            J=J+1
            VALUE(J)=VALUES(IPC)
         END IF
         END DO
C
C        FOR 012005 ELEMENT
C
         J=J+1
         VALUE(J)=70.
C
         KERR=0
         RETURN
      END IF
C
      IF(KST.EQ.145) THEN
C
C        ERA CREATED ACARS HAVE MISSING VALUES FOR QC AND 
C        ZERO FOR ORIGINATING CENTRE. BIT MAP IS SET WRONG.
C
         ICNF=0
         DO 131 I=1,KTDEXL
         IF(KTDEXP(I).EQ.33007) THEN
            ICNF=I
            GO TO 132
         END IF
 131     CONTINUE
 132     CONTINUE
         DO 133 J=ICNF-1,KTDEXL
         IF(KTDEXP(J).EQ.33007) THEN
            JCNF=J
         END IF
 133     CONTINUE
 134     CONTINUE
C
         DO 231 I=1,KSUP(5)
         DO 232 J=1,KSUP(6)
         IJ=I+(J-1)*KELEM
         IF(ICNF.NE.0) THEN
           IF(I.GE.ICNF.AND.I.LE.JCNF) THEN
             IF(ABS(VALUES(IJ)-RVIND)/RVIND.LT.EPS) VALUES(IJ)=70.
           END IF
         END IF
         IF(KTDEXP(I).EQ.001031) THEN
            VALUE(IJ)=98.
         ELSEIF(KTDEXP(I).EQ.031031) THEN
            VALUE(IJ)=0.
         ELSE
            VALUE(IJ)=VALUES(IJ)
         END IF
         CONTINUE
 232     CONTINUE
 231     CONTINUE
C
         DO 233 I=1,KTDLEN
         IF(KTDLST(I).EQ.001201) THEN
            KTDLST1(I)=001032
         ELSE
            KTDLST1(I)=KTDLST(I)
         END IF
 233     CONTINUE
         KERR=0
C
         KDLEN=0
         DO 234 I=1,KTDEXL
         IF(KTDEXP(I).EQ.01001.OR.
     1      KTDEXP(I).EQ.031000.OR.
     1      KTDEXP(I).EQ.031002) THEN
            KDLEN=KDLEN+1
            KDATA(KDLEN)=NINT(VALUES(I))
         END IF
 234     CONTINUE
C
         KERR=0
         RETURN
      END IF
C
 300  CONTINUE
C
      IF(KST.EQ.92.and.KTDLST(6).eq.301023) THEN
C
C        PILOT 92
C        ----------------------
         J=0
         OSHEAR=.FALSE.
C
         DO 801 I=1,KTDLEN
C
         IF(KTDLST(I).GT.262000.AND.KTDLST(I).LT.300000) THEN
            J=J+1
            KTDLST1(J)=222000
         ELSEIF(KTDLST(I).EQ.301023) THEN
            J=J+1
            KTDLST1(J)=301024
         ELSE
            J=J+1
            KTDLST1(J)=KTDLST(I)
            IF(KTDLST(I).EQ.11061.OR.KTDLST(I).EQ.11062) OSHEAR=.TRUE.
         END IF
C
 801     CONTINUE
C
         KTDLEN=J
C
C        FIND FIRST AND LAST POINTER TO DATA PRESENT INDICATOR
C
         ODPF=.FALSE.
C
         DO 802 I=1,KTDEXL
         IF(KTDEXP(I).EQ.31031) THEN
            IF(.NOT.ODPF) IDPF=I
            ODPF=.TRUE.
         ELSE
            IF(ODPF) THEN
               IDPL=I-1
               GO TO 803
            END IF
         END IF
 802     CONTINUE
C
 803     CONTINUE
C
C        FIND START OF % CONFIDENCE
C
         DO 804 I=IDPL,KTDEXL
         IF(KTDEXP(I).EQ.033007) THEN
            IPCF=I
            GO TO 805
         END IF
 804     CONTINUE
C
 805     CONTINUE
C
C        MAKE DATA SUITABLE FOR COMPRESSION
C
         J=0
         KDLEN=0
         ISTAT=0
         IPC=IPCF-1
C
         DO 806 I=1,KTDEXL
         J=J+1
         IF(KTDEXP(I).EQ.031001.OR.KTDEXP(I).EQ.031002) THEN
            KDLEN=KDLEN+1
            KDATA(KDLEN)=NINT(VALUES(I))
            IF(KDLEN.EQ.2.AND..NOT.OSHEAR) THEN
               KDATA(KDLEN)=NINT(VALUES(I))+1
            END IF
            IF(KDLEN.EQ.3.AND.OSHEAR) THEN
               KDATA(KDLEN)=NINT(VALUES(I))+1
            END IF
         END IF
C
         IF(KTDEXP(I).EQ.031031) THEN
            ISTAT=ISTAT+1
            IF(ISTAT.EQ.12) THEN
               VALUE(J)=VALUES(I)
               J=J+1
               VALUE(J)=1.
               VALUE(IDPF)=VALUES(IDPF-1)+1
            ELSE
               VALUE(J)=VALUES(I)
            END IF
            GO TO 806
         END IF
C
         IF(KTDEXP(I).EQ.033007) THEN            ! % CONF
            VALUE(J)=VALUES(I)
            GO TO 806
         END IF
C
         IF(KTDEXP(I).EQ.006002) THEN
            VALUE(J)=VALUES(I)
            J=J+1
            VALUE(J)=RVIND                    ! INSERT STATION HEIGHT
            GO TO 806
         END IF
C
         IF(KTDEXP(I).EQ.10003.OR.KTDEXP(I).EQ.10008) THEN
            IF(ABS(VALUES(I)-RVIND)/RVIND.GT.EPS) THEN
               VALUE(J)=(VALUES(I)/9.8)*9.80665
            ELSE
               VALUE(J)=VALUES(I)
            END IF
         ELSE
            VALUE(J)=VALUES(I)
         END IF
C
 806     CONTINUE
C
 810     CONTINUE
C
         KERR=0
         RETURN
      END IF

      ICNF=0
      DO 121 I=1,KTDEXL
      IF(KTDEXP(I).EQ.33007) THEN
         ICNF=I
         GO TO 122
      END IF
 121  CONTINUE
 122  CONTINUE
      DO 123 J=ICNF-1,KTDEXL
      IF(KTDEXP(J).EQ.33007) THEN
         JCNF=J
      END IF
 123  CONTINUE
 124  CONTINUE
C
      IF(KST.eq.86) then
         if(KTDLST(1).eq.310195) then
            ktdlst1(1)=301043
            ktdlst1(2)=304001
            ktdlen=2 
c
            DO 2211 I=1,KSUP(5)
            DO 2222 J=1,KSUP(6)
            jm1k=(J-1)*KELEM
            value(1+jm1k)=values(1+jm1k)
            value(2+jm1k)=values(15+jm1k)
            value(3+jm1k)=values(5+jm1k)
            value(4+jm1k)=values(6+jm1k)
            value(5+jm1k)=values(7+jm1k)
            value(6+jm1k)=values(8+jm1k)
            value(7+jm1k)=values(9+jm1k)
            value(8+jm1k)=values(10+jm1k)
            value(9+jm1k)=values(11+jm1k)
            value(10+jm1k)=values(12+jm1k)
            value(11+jm1k)=2.
            value(12+jm1k)=values(16+jm1k)
            value(13+jm1k)=values(21+jm1k)
            value(14+jm1k)=values(17+jm1k)
            value(15+jm1k)=values(18+jm1k)
 2222       continue
 2211       continue
C
            return
         end if
      end if
c
      IF(KST.GT.0.AND.KST.LE.255) THEN
         if(kst.eq.87) ICNF=0
         DO 221 I=1,KSUP(5)
         DO 222 J=1,KSUP(6)
         IJ=I+(J-1)*KELEM
         IF(ICNF.NE.0) THEN
         IF(I.GE.ICNF.AND.I.LE.JCNF) THEN
           IF(ABS(VALUES(IJ)-RVIND)/RVIND.LT.EPS) VALUES(IJ)=70.
         END IF
         END IF
         VALUE(IJ)=VALUES(IJ)
 222     CONTINUE
 221     CONTINUE
C
         DO 223 I=1,KTDLEN
         IF(KTDLST(I).EQ.001201) THEN
            KTDLST1(I)=001032
         ELSE
         KTDLST1(I)=KTDLST(I)
         END IF
 223     CONTINUE
         KERR=0
C
         KDLEN=0
         DO 224 I=1,KTDEXL
         IF(KTDEXP(I).EQ.031001.OR.
     1      KTDEXP(I).EQ.031000.OR.
     1      KTDEXP(I).EQ.031002) THEN
            KDLEN=KDLEN+1
            KDATA(KDLEN)=NINT(VALUES(I))
         END IF
 224     CONTINUE
         RETURN
C
      END IF
C
      KERR=1
      PRINT*,'NO CONVERTION FOR ',KST,' SUBTYPE.'
C
      END
      SUBROUTINE DATE_TIME_CHECK(KTDEXP,KSEC1,KSEC3,KEY,KEL,VALUES,KERR)
C
C**** *DATE_TIME_CHECK*
C
C
C     PURPOSE.
C     --------
C         Check date time consistency in the bufr section1,
C         Key and data section.
C
C
C
C**   INTERFACE.
C     ----------
C
C              *CALL DATE_TIME_CHECK(KTDEXP,
C                        KSEC1,KSEC3,KEY,KEL,VALUES,KERR)
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
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/09/94
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
      DIMENSION KTDEXP(*), KSEC1(*),KSEC3(*), KEY(*)
      REAL*8 VALUES(*)
C
C     ------------------------------------------------------------------
C*          1. INITIALIZE CONSTANTS AND VARIABLES.
C              -----------------------------------
 100  CONTINUE
C
C     MISSING VALUE INDICATOR
C

      IPYEAR=0
      osec1=.true.
      okey=.true.
      odata=.true.
c
      DO 101 I=1,36
      IF(KTDEXP(I).EQ.004001.AND.IPYEAR.eq.0) IPYEAR=I
 101  CONTINUE
c
      IF(IPYEAR.NE.0) THEN
c       Determine date time differences
c
        if(ksec1(9).gt.100) then
           osec1=.false.
        elseif(ksec1(9).lt.20) then
           iys1=ksec1(9)+2000
        else
           iys1=ksec1(9)+1900
        end if
c
        if(ksec1(10).le.0.or.ksec1(10).gt.12) then
           osec1=.false.
        end if
c
        if(ksec1(11).le.0.or.ksec1(11).gt.31) then
           osec1=.false.
        end if
        if(ksec1(12).lt.0.or.ksec1(12).gt.23) then
           osec1=.false.
        end if
        if(ksec1(13).lt.0.or.ksec1(13).gt.59) then
           osec1=.false.
        end if
c
        if(osec1) then
          isdate=iys1*10000+ksec1(10)*100+ksec1(11)
          istime=ksec1(12)*100+ksec1(13)
        else
          isdate=-1
          istime=-1
        end if
c
        if(key(4).le.1940.or.key(4).gt.2020) then
           okey=.false.
        end if  
        if(key(5).le.0.or.key(5).gt.12) then
           okey=.false.
        end if
        if(key(6).le.0.or.key(6).gt.31) then
           okey=.false.
        end if
        if(key(7).lt.0.or.key(7).gt.23) then
           okey=.false.
        end if
        if(key(8).lt.0.or.key(8).gt.59) then
           okey=.false.
        end if
c
        if(okey) then
           ikdate=key(4)*10000+key(5)*100+key(6)
           iktime=key(7)*100+key(8)
        else
           ikdate=-2
           iktime=-2
        end if
c
        if(values(ipyear).le.1940.or.values(ipyear).gt.2020) then  
           odata=.false.
        end if
        if(values(ipyear+1).le.0.or.values(ipyear+1).gt.12) then
           odata=.false.
        end if
        if(values(ipyear+2).le.0.or.values(ipyear+2).gt.31) then
           odata=.false.
        end if
        if(values(ipyear+3).lt.0.or.values(ipyear+3).gt.23) then
           odata=.false.
        end if
        if(values(ipyear+4).lt.0.or.values(ipyear+4).gt.59) then
           odata=.false.
        end if
c
        if(odata) then
           iddate=values(ipyear)*10000+values(ipyear+1)*100+
     1            values(ipyear+2)
           idtime=values(ipyear+3)*100+values(ipyear+4)
        else
           iddate=-3
           idtime=-3
        end if
c
        if(isdate.lt.0.and.ikdate.lt.0.and.iddate.lt.0) then
c          some subtype 84 got wrong year as 3999
           if(values(ipyear).gt.3000) then
              values(ipyear)=values(ipyear)-2000.
              key(4)=key(4)-2000
           else
              kerr=1
              print*,'Date-time is wrong in the bufr message.'
              return
           end if
           return
        elseif(istime.lt.0.and.idtime.lt.0.and.iktime.ge.0) then
           kerr=1
           print*,'Date-time is wrong in the bufr message.'
           return
        end if
c
        if(isdate.eq.ikdate.and.isdate.ne.iddate.or.
     1     istime.eq.iktime.and.istime.ne.idtime) then
c
c          set the date of the first subset from the key
C
c          do ii=1,ksec3(3)
c          iii=ipyear+(ii-1)*kel
           iii=ipyear
           values(iii  )=key(4)
           values(iii+1)=key(5)
           values(iii+2)=key(6)
           values(iii+3)=key(7)
           values(iii+4)=key(8)
c          end do 
        elseif(isdate.eq.iddate.and.isdate.ne.ikdate.or.
     1     istime.eq.idtime.and.istime.ne.iktime) then
           key(4)=nint(values(ipyear))
           key(5)=nint(values(ipyear+1))
           key(6)=nint(values(ipyear+2))
           key(7)=nint(values(ipyear+3))
           key(8)=nint(values(ipyear+4))
        else 
           if(odata) then
              key(4)=nint(values(ipyear))
              key(5)=nint(values(ipyear+1))
              key(6)=nint(values(ipyear+2))
              key(7)=nint(values(ipyear+3))
              key(8)=nint(values(ipyear+4))
c
              ksec1(9)=nint(values(ipyear))-1900
              if(values(ipyear).gt.2001.) then
                  ksec1(9)=nint(values(ipyear))-2000
              end if
              ksec1(10)=nint(values(ipyear+1))
              ksec1(11)=nint(values(ipyear+2))
              ksec1(12)=nint(values(ipyear+3))
              ksec1(13)=nint(values(ipyear+4))
           else
c             do ii=1,ksec3(3)
c             iii=ipyear+(ii-1)*kel
              iii=ipyear
              values(iii  )=key(4)
              values(iii+1)=key(5)
              values(iii+2)=key(6)
              values(iii+3)=key(7)
              values(iii+4)=key(8)
c             end do
c
              ksec1(9)=nint(values(ipyear))-1900
              if(values(ipyear).gt.2001.) then
                  ksec1(9)=nint(values(ipyear))-2000
              end if
              ksec1(10)=nint(values(ipyear+1))
              ksec1(11)=nint(values(ipyear+2))
              ksec1(12)=nint(values(ipyear+3))
              ksec1(13)=nint(values(ipyear+4))
           end if
         end if
      else
        print*,'There is no year in the data.'
        kerr=1
        return
      end if
c
      return
      end
