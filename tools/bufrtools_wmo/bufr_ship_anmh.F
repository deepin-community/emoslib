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
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2=4096 ,JSEC3= 4,
     1       JSEC4=   2,JELEM=160000,JSUBS=400,JCVAL=150 ,JBUFL=512000,
#ifdef JBPW_64
     2          JBPW =  64,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200,
#else
     2          JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200,
#endif
     3          JWORK=4096000,JKEY=46,JBYTE=80000)
C
      PARAMETER (KELEM=40000)
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
      DIMENSION KTDLST1(KELEM),KTDEXP1(KELEM)
      DIMENSION KDATA(200)
      DIMENSION IOUT(12800)
      DIMENSION IHEIGHT(10000)
C
      CHARACTER*9   CALLSN(10000),CSGN
      CHARACTER*256 CF(100),COUT,CFIN,CLIST
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
      CHARACTER*80 YENC
      CHARACTER*256 CARG(10)
C
      logical olist
cs      EXTERNAL GETARG
C                                                                       
C     ------------------------------------------------------------------
C*          1. INITIALIZE CONSTANTS AND VARIABLES.
C              -----------------------------------
 100  CONTINUE
C
C     MISSING VALUE INDICATOR
C 
      olist=.false.
      callsn(1)=' '
      iunit2=55
      ITLEN=6400
      ITL=0
      IL=0
      JZ=0
      NW=0
      N=0
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
         PRINT*,'USAGE -- bufr_ship_anmh -f list -i infile -o outfile'
         STOP
      END IF
      NFILE=NARG
C
      DO 104 J=1,NARG
      CALL GETARG(J,CARG(J))
 104  CONTINUE

      IL=0
      II=0
      IO=0
      DO 105 J=1,NARG
      IF(CARG(J).EQ.'-i') THEN
         IN=J
      ELSEIF(CARG(J).EQ.'-o') THEN
         IO=J
      ELSEIF(CARG(J).EQ.'-f') THEN
         IL=J
      END IF
 105  CONTINUE
C
      IF(IO.EQ.0.OR.IN.EQ.0) THEN
         PRINT*,'USAGE -- bufr_ship_anmh -f list -i infile -o outfile'
         STOP
      END IF
C
      COUT=CARG(IO+1)
C
      if(IL.ne.0) then
         olist=.true.
         clist=carg(IL+1)
C
         jjl=INDEX(clist,' ')
         jjl=jjl-1
      end if
C
C
      JJ=INDEX(COUT,' ')
      JJ=JJ-1
C
      CALL PBOPEN(IUNIT1,COUT(1:JJ),'W',IRET)
      IF(IRET.EQ.-1) STOP 'OPEN FAILED ON BUFR.DAT'
      IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'
C
      CFIN=CARG(IN+1) 
      ILN=INDEX(CFIN,' ')
      iln=iln-1
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
      IF(OLIST) THEN
         open(iunit2,file=clist(1:jjl),iostat=ios,status='old')
         if(ios.ne.0) then
            stop 'Open error on list'
         end if
C
         I=0
         DO WHILE(IOS.EQ.0)
         I=I+1
         READ(IUNIT2,'(A,1X,I5)',IOSTAT=IOS) CALLSN(I),IHEIGHT(I)
         END DO
C
         close(IUNIT2)
C
      END IF
      NHEIGHT=I-1

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
         CALL MY_EXIT(2)
      END IF
C
       if(ksec1(7).ne.9.and.ksec1(7).ne.11.and.
     1    ksec1(7).ne.13.and.ksec1(7).ne.19) go to 630
c
      IF(KSUP(6).GT.1) THEN
         KEL=JWORK/KSUP(6)
         IF(KEL.GT.JELEM) KEL=JELEM
      ELSE
         KEL=KELEM
      END IF
C
      CALL BUFREX(KBUFL,KBUFF,KSUP,KSEC0 ,KSEC1,KSEC2 ,KSEC3 ,KSEC4,
     1            KEL,CNAMES,CUNITS,KVALS,VALUES,CVALS,IERR)
C
      IF(IERR.NE.0) THEN
         CALL MY_EXIT(2)
      END IF
      IOBS=IOBS+KSEC3(3)
C
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
      IF(KERR.NE.0) CALL MY_EXIT(2)
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
      IF(KERR.NE.0) CALL MY_EXIT(2)
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
C     IF(.NOT.OENC) GO TO 300
C
C               FIRST GET DATA DESCRIPTORS
C
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
      IF(KERR.NE.0) CALL MY_EXIT(2)
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
C
C     If ship already got ship anemometer height 
c     write data to target file
c
      oheight=.true.
      do i=1,ktdlen
       if(KTDLST(i).eq.010197) then
          oheight=.false.
          go to 630
       end if
      end do
c
      j=0
      do i=1,2
       j=j+1
       ktdlst1(j)=ktdlst(i)
      end do
      j=j+1
      ktdlst1(j)=010197
      j=j+1
      ktdlst1(j)=222000
      j=j+1
      ktdlst1(j)=101034
      j=j+1
      ktdlst1(j)=031031
      j=j+1
      ktdlst1(j)=001031
      j=j+1
      ktdlst1(j)=001032
      j=j+1
      ktdlst1(j)=101034
      j=j+1
      ktdlst1(j)=033007

      ktdlen1=j
      
C
C    ----------------------------------------
C    ! Modification to data section         !
C    ----------------------------------------
C
      ib=0
      do iii=1,ksup(6)
      do iiii=1,33
        ia=iiii+(iii-1)*kel
        ib=ib+1
        ibb=ib+(iii-1)*kel
        value(ibb)=values(ia)
      end do
      end do
c
      if(olist) then
         do iii=1,ksup(6)
         do iiii=34,34
           ia=iiii+(iii-1)*kel
           ib=ib+1
           ibb=ib+(iii-1)*kel
           ifound=0
           csgn=cvals(1)(1:9)
           call search(callsn,nheight,csgn,ifound)
           if(ifound.ne.0) then
              value(ibb)=iheight(ifound)
           else
              value(ibb)=rvind
           end if
         end do
         end do
      else
         do iii=1,ksup(6)
         do iiii=34,34
           ia=iiii+(iii-1)*kel
           ib=ib+1
           ibb=ib+(iii-1)*kel
           value(ibb)=rvind
         end do
         end do
      end if
c
      do iii=1,ksup(6)
      do iiii=34,67
        ia=iiii+(iii-1)*kel
        ib=ib+1
        ibb=ib+(iii-1)*kel
        value(ibb)=values(ia)
      end do
      end do
c
c     insert data present indicator
c
      ib=ib+1
      do iii=1,ksup(6)
        ibb=ib+(iii-1)*kel
        value(ibb)=0.
      end do
      do iii=1,ksup(6)
      do iiii=68,102
        ia=iiii+(iii-1)*kel
        ib=ib+1
        ibb=ib+(iii-1)*kel
        value(ibb)=values(ia)
      end do
      end do
c
c     insert% confidance
c
      ib=ib+1
      do iii=1,ksup(6)
        ibb=ib+(iii-1)*kel
        value(ibb)=70.
      end do
c
      kdata(1)=kdata(1)+1
      kdata(2)=kdata(2)+1
C
C     -----------------------------------------------------------------------
C
C*          6.2 ENCODE DATA INTO BUFR MESSAGE.
C               ------------------------------
 620  CONTINUE
C
      IF(KSEC1(5).NE.0) THEN
         CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)
         IF(KERR.NE.0) THEN
            PRINT*,'BUUKEY: ERROR ',KERR
            CALL MY_EXIT(2)
         END IF
      END IF
C
      IF(KSUP(6).EQ.0) THEN
         PRINT*,'ZERO SUBSETS'
         CALL MY_EXIT(2)
      END IF
C
C
C     GET INFORMATION FOR RDB KEY
C
C     CALL BUCREKEY(KEL,KTDEXP,KSUP,KSEC1,KSEC3,KEY,
C    1              VALUES,CVALS,KERR)
C     IF(KERR.NE.0) THEN
C        PRINT*,'ERROR IN BUCREKEY.'
C        CALL MY_EXIT(2)
C     END IF
C
C     PACK NEW RDB KEY
C
      ksec1(5)=128
      ksec2(1)=52
C
      CALL BUPKEY(KEY,KSEC1,KSEC2,KERR)
      IF(KERR.NE.0) CALL MY_EXIT(2)
C
c     KSEC3(4)=128
c     IF(KSEC3(3).GT.1) KSEC3(4)=192
c     IF(KSEC3(4).EQ.0) KSEC3(4)=192
C
      CALL BUFREN( KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
     1             KTDLEN1,KTDLST1,KDLEN,KDATA,KEL,        !KSUP(5),
     2             KVALS,VALUE,CVALS,KBUFL,KBUFR,KERR)
C
      IF(KERR.GT.0) THEN
         PRINT*,'ERROR DURING ENCODING.'
         CALL MY_EXIT(2)
      END IF
C
C           6.3 WRITE PACKED BUFR MESSAGE INTO FILE.
C               ------------------------------------
 630  CONTINUE
C
      if((ksec1(7).eq.9.or.ksec1(7).eq.11.or.
     1   ksec1(7).eq.13.or.ksec1(7).eq.19).and.oheight) then
         IKBUFL=KBUFL*4
         CALL PBWRITE(IUNIT1,KBUFR,IKBUFL,IERR)
         IF(IERR.LT.0) THEN
            PRINT*,'ERROR WRITING INTO TARGET FILE.'
            CALL MY_EXIT(2)
         END IF
      else
         CALL PBWRITE(IUNIT1,KBUFF,IKBUFL,IERR)
         IF(IERR.LT.0) THEN
            PRINT*,'ERROR WRITING INTO TARGET FILE.'
            CALL MY_EXIT(2)
         END IF
      end if
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
      SUBROUTINE SEARCH(KARRAY,K,KVAL,KINDEX)
C
C**** *SEARCH*
C
C
C     PURPOSE.
C     --------
C         
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SEARCH(KARRAY,K,KVAL,KINDEX)*
C             KARRAY  - list
C             K       - number of elements in the list
C             KVAL    - value to be searched for
C             KINDEX  - index of matched value
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
C          M. DRAGOSAVAC    *ECMWF*       15/01/92.
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
      character*(*)  KARRAY(*)
      character*(*)  KVAL
      logical found
      integer count
C
C     ------------------------------------------------------------------
C*          1.   Binary search. 
C                _____________
 100  CONTINUE
C
      IHIGH = K
      ILOW  = 1
C
      count=0
      found=.false.
      kindex=0
      numbis= int(log(real(k))/log(2.0)) +1

      ihigh = K+1
      ilow  = 1

      do while(.not.found .and.count .ne. numbis)

            mid = (ihigh + ilow)/2
            if(karray(mid).eq.kval) then
               found=.true.
            elseif(kval .lt.karray(mid)) then
               ihigh= mid
            elseif(kval.gt.karray(mid)) then
               ilow= mid
            end if
            count=count+1
      end do

      if(found) kindex=mid

C
      RETURN
      END
